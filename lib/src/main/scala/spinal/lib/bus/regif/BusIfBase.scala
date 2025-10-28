package spinal.lib.bus.regif

import spinal.core._
import spinal.core.fiber.Handle
import spinal.lib.{BigIntRicher, WhenBuilder}

import language.experimental.macros
import scala.collection.mutable.ListBuffer

trait BusIfBase extends Area{
  val busDataWidth: Int
  val busAddrWidth: Int

  val askWrite: Bool
  val askRead: Bool
  val doWrite: Bool
  val doRead: Bool
  lazy val cg_en: Bool = True
  lazy val bus_nsbit: Bool = False //NS-bit(Non-Secure Access flag bit)

  val reg_wrerr: Bool
  val reg_rderr: Bool
  val reg_rdata: Bits

  val bus_rdata: Bits
  def bus_slverr: Bool = if(withSecFireWall)  (reg_wrerr || reg_rderr) else reg_rderr

  @deprecated("readData rename to bus_rdata", "2024.12.30")
  lazy val readData: Bits = bus_rdata
  @deprecated("readError rename to bus_rderr", "2024.12.30")
  lazy val readError: Bool = bus_slverr

  val writeData: Bits
  val readSync: Boolean = true
  val withStrb: Boolean
  val withSecFireWall: Boolean

  val wstrb: Bits  //= withstrb generate(Bits(strbWidth bit))
  val wmask: Bits  //= withstrb generate(Bits(busDataWidth bit))
  val wmaskn: Bits //= withstrb generate(Bits(busDataWidth bit))

  private var version = s"SpinalHDL-${Spinal.version}"
  protected var _addrAlignCheck = true
  def setAlignCheck(value: Boolean = false) = _addrAlignCheck = value
  def setVersion(ver: String): Unit = version = ver
  def getVersion: String = version

  def readAddress(): UInt
  def writeAddress(): UInt

  def readHalt(): Unit
  def writeHalt(): Unit

  def busByteWidth: Int = scala.math.ceil(this.busDataWidth / 8.0).toInt
  def bw: Int = busByteWidth

  def wordAddressInc: Int = busByteWidth
  def strbWidth: Int = busByteWidth
  def underbitWidth: Int = log2Up(wordAddressInc)

  def mwdata(sec: Range): Bits = if(withStrb) writeData(sec) & wmask(sec) else writeData(sec)

  /**
   * Finds continuous blocks of RegSlice lists based on their starting addresses.
   *
   * This function groups consecutive lists of RegSlice objects that have continuous
   * starting addresses. It calculates the gap between consecutive blocks automatically
   * using the formula: gap = last.addr + last.size - head.addr, then determines
   * continuity based on the index derived from head.addr / gap.
   *
   * Commonly used in hardware design automation for generating parameterized files
   * like .h/.svh/.ralf/.html where continuous memory/register blocks need to be
   * grouped for efficient code generation.
   *
   * @param regSliceLists List of RegSlice lists to be analyzed
   * @return List of tuples containing (starting RegSlice list, continuous block length)
   */
  def findContinuousBlocks(regSliceLists: List[List[RegSlice]]): List[(List[RegSlice], Int)] = {
    if (regSliceLists.isEmpty) return List.empty

    val sortedLists = regSliceLists.sortBy(_.head.addr)

    val listWithIndex = sortedLists.map { list =>
      val gap = list.last.addr + list.last.size - list.head.addr
      val startIndex = list.head.addr / gap
      (list, startIndex)
    }

    val result = scala.collection.mutable.ListBuffer[(List[RegSlice], Int)]()
    var i = 0

    while (i < listWithIndex.length) {
      val currentStartList = listWithIndex(i)._1
      val currentStartIndex = listWithIndex(i)._2
      var continuousLength = 1
      var j = i + 1

      while (j < listWithIndex.length && listWithIndex(j)._2 == currentStartIndex + continuousLength) {
        continuousLength += 1
        j += 1
      }

      result += ((currentStartList, continuousLength))
      i = j
    }

    result.toList
  }

  def initStrbMasks() = {
    if (withStrb) {
      (0 until strbWidth).foreach { i =>
        wmask ((i + 1) * 8 - 1 downto i * 8) := Mux(wstrb(i), B(0xFF, 8 bit), B(0, 8 bit))
        wmaskn((i + 1) * 8 - 1 downto i * 8) := Mux(wstrb(i), B(0, 8 bit), B(0xFF, 8 bit))
      }
    }
  }
  def wdata(reg: BaseType, sec: Range): Bits = wdata(reg, sec, oper = "normal")
  def wdata(reg: BaseType, sec: Range, oper: String):Bits = {
    oper match {
      case "clear" =>
        if(withStrb){
          reg match {
            case t: Bits => (t        & wmaskn(sec))
            case t: UInt => (t.asBits & wmaskn(sec))
            case t: SInt => (t.asBits & wmaskn(sec))
            case t: Bool => (t.asBits & wmaskn(sec))
            case _       => SpinalError(s"only accept BiterVector ${reg} for section ${sec} Range")
          }
        } else B(0, sec.size bit)
      case "set" =>
        if(withStrb) {
          reg match {
            case t: Bits => (t        & wmaskn(sec)) | wmask(sec)
            case t: UInt => (t.asBits & wmaskn(sec)) | wmask(sec)
            case t: SInt => (t.asBits & wmaskn(sec)) | wmask(sec)
            case t: Bool => (t.asBits & wmaskn(sec)) | wmask(sec)
            case _ => SpinalError(s"only accept BiterVector ${reg} for section ${sec} Range")
          }
        }else Bits(sec.size bit).setAll()
      case "normal" =>
        if(withStrb){
          reg match {
            case t: Bits => (t        & wmaskn(sec)) | (writeData(sec) & wmask(sec))
            case t: UInt => (t.asBits & wmaskn(sec)) | (writeData(sec) & wmask(sec))
            case t: SInt => (t.asBits & wmaskn(sec)) | (writeData(sec) & wmask(sec))
            case t: Bool => (t.asBits & wmaskn(sec)) | (writeData(sec) & wmask(sec))
            case _ => SpinalError(s"only accept BiterVector ${reg} for section ${sec} Range")
          }
        } else writeData(sec)
      case "toggle" =>
        if(withStrb){
          reg match {
            case t: Bits => (t        & wmaskn(sec)) | (~t(sec)        & wmask(sec))
            case t: UInt => (t.asBits & wmaskn(sec)) | (~t.asBits(sec) & wmask(sec))
            case t: SInt => (t.asBits & wmaskn(sec)) | (~t.asBits(sec) & wmask(sec))
            case t: Bool => (t.asBits & wmaskn(sec)) | (~t.asBits(sec) & wmask(sec))
            case _ => SpinalError(s"only accept BiterVector ${reg} for section ${sec} Range")
          }
        } else ~reg.asBits(sec)
      case _ => SpinalError(s"unrecognize '${oper}''")
    }
  }
  def mwdata(pos: Int): Bool = if(withStrb) writeData(pos) & wmask(pos) else writeData(pos)
  def wdata(reg: Bool, pos: Int): Bool = wdata(reg, pos, oper = "normal")
  def wdata(reg: Bool, pos: Int, oper: String): Bool = {
    oper match {
      case "clear"  => if (withStrb) (reg & wmaskn(pos))                                 else False
      case "set"    => if (withStrb) (reg & wmaskn(pos)) |                   wmask(pos)  else True
      case "normal" => if (withStrb) (reg & wmaskn(pos)) | (writeData(pos) & wmask(pos)) else writeData(pos)
      case "toggle" => if (withStrb) (reg & wmaskn(pos)) | (~reg           & wmask(pos)) else ~reg
      case _ => SpinalError(s"unrecognize '${oper}''")
    }
  }
}
