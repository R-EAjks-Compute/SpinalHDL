package spinal.lib.bus.regif

import spinal.lib._


final case class DocRalf(name : String, backdoor: Boolean = true) extends BusIfDoc {
  override val suffix: String = "ralf"
  override def body(): String = {
      s"""
         |block ${this.name} {
         |  endian little;
         |  bytes ${bi.busByteWidth};
         |${bi.regSlicesNotReuse.map(_.toRalf()).mkString("\n")};
         |${groupRalf(bi.reuseGroupsById)}
         |}""".stripMargin
  }

  implicit class RegSliceExtend(reg: RegSlice) {
    def toRalf(base: BigInt = 0, tab: String = ""): String = {
      reg match {
        case ram: RamInst => toRamRalf(ram, base, tab)
        case reg: RegInst => toRegRalf(reg, base, tab)
        case _ => toRegRalf(reg, base, tab)
      }
    }

    def toRamRalf(ram: RamInst, base: BigInt, tab: String = ""): String = {
      val hdlpath = if(backdoor) s"(${ram.getName()})" else ""
      s"""${tab}  memory ${ram.upperName()} ${hdlpath} @'h${(ram.getAddr() - base).toString(16).toUpperCase} {
         |${tab}    size  ${ram.getSize()};
         |${tab}    bits  ${ram.bi.busDataWidth};
         |${tab}    access rw;
         |${tab}  }""".stripMargin
    }

    def toRegRalf(reg: RegSlice, base: BigInt = 0, tab: String = ""): String = {
        s"""${tab}  register ${reg.upperName()} @'h${(reg.getAddr() - base).toString(16).toUpperCase} {
           |${reg.getFields().map(_.toRalf(tab + "  ")).mkString("\n")}
           |${tab}  }""".stripMargin
    }
  }

  def groupRalf(lst: Map[String, Map[Int, List[RegSlice]]]) = {
    lst.map{t =>
      val firstgrp: List[RegSlice] = t._2.toList.sortBy(_._1).head._2
      val grpName = t._1
      val grpSize = t._2.size
      val grpBase = firstgrp.head.reuseTag.baseAddr
      s"""  regfile ${grpName}[${grpSize}] @'h${grpBase.hexString()} {
         |${firstgrp.map(_.toRalf(grpBase, tab = "  ")).mkString("\n")}
         |  }""".stripMargin
    }.mkString("\n")
  }

  implicit class FieldDescrExtend(fd: Field) {
    def toRalf(tab: String = ""): String = {
      def rename(name: String) = {
        val pre = if(fd.getWidth() == 1) s"${fd.getSection().start}" else s"${fd.getSection().start}_${fd.getSection().end}"
        val name = fd.getAccessType match {
          case AccessType.NA  => s"rsv_${pre}"
          case AccessType.ROV => s"rov_${pre}"
          case _ => fd.getName()
        }
        name
      }

      def access = {
        fd.getAccessType() match{
          case x if fd.uvmBaseAcc.contains(x) => x.toString.toLowerCase()
          case AccessType.NA  => "ro"
          case AccessType.ROV => "ro"
          case _ => "rw"
        }
      }

      def attribute = {
        if(fd.getAccessType() == AccessType.NA){
          s"\n${tab}    attributes {NO_REG_TEST 1};"
        } else ""
      }

      val forhdlpath = if(backdoor && !(fd.getAccessType() == AccessType.NA)) s"(${rename(fd.getName())})" else ""
      s"""${tab}  field ${rename(fd.getName())} $forhdlpath @${fd.getSection().end} {
         |${tab}    bits ${fd.getWidth()};
         |${tab}    access ${access};
         |${tab}    reset ${formatResetValue(fd.getResetValue(), fd.getWidth())};${attribute}
         |${tab}  }""".stripMargin
    }
  }
}
