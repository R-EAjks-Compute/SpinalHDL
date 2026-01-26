package spinal.core

import spinal.tester.scalatest.SpinalAnyFunSuite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

class ReportSourceLocationTester extends SpinalAnyFunSuite {

  private def displayFormatStrings(rtl: String): List[String] = {
    def displayFormatString(line: String): Option[String] = {
      val start = line.indexOf("$display(\"")
      if (start < 0) return None
      val from = start + "$display(\"".length
      val end = line.indexOf("\"", from)
      if (end < 0) None else Some(line.substring(from, end))
    }

    rtl.linesIterator.flatMap(displayFormatString).toList
  }

  test("report can opt-in to print Scala source location") {
    val outDir = Files.createTempDirectory("spinalhdl_report_source_location_").toFile

    val spinalReport = SpinalConfig(
      targetDirectory = outDir.getAbsolutePath,
      reportSourceLocationFormat = "$FILE:$LINE: $SEVERITY "
    )
      .generateVerilog(new Component {
        report("HELLO_NO_LOC")
        report("HELLO_WITH_LOC", includeSourceLocation = true)
      })

    val rtlPath = spinalReport.generatedSourcesPaths.find(_.endsWith(".v")).getOrElse {
      fail(s"No Verilog file generated, got: ${spinalReport.generatedSourcesPaths.mkString(", ")}")
    }

    val rtl = new String(Files.readAllBytes(Paths.get(rtlPath)), StandardCharsets.UTF_8)

    val formats = displayFormatStrings(rtl)
    assert(formats.exists(s => s.contains("HELLO_WITH_LOC") && s.matches(""".*ReportSourceLocationTester\.scala:\d+: NOTE HELLO_WITH_LOC.*""")))
    assert(formats.exists(_.contains("HELLO_NO_LOC")))
    assert(!formats.filter(_.contains("HELLO_NO_LOC")).exists(_.contains("ReportSourceLocationTester.scala:")))
  }

  test("report can enable source location globally") {
    val outDir = Files.createTempDirectory("spinalhdl_report_source_location_global_").toFile

    val spinalReport = SpinalConfig(
      targetDirectory = outDir.getAbsolutePath,
      reportIncludeSourceLocation = true,
      reportSourceLocationFormat = "$FILE:$LINE: $SEVERITY "
    )
      .generateVerilog(new Component {
        report("HELLO_GLOBAL_LOC")
      })

    val rtlPath = spinalReport.generatedSourcesPaths.find(_.endsWith(".v")).getOrElse {
      fail(s"No Verilog file generated, got: ${spinalReport.generatedSourcesPaths.mkString(", ")}")
    }

    val rtl = new String(Files.readAllBytes(Paths.get(rtlPath)), StandardCharsets.UTF_8)
    val formats = displayFormatStrings(rtl)
    assert(formats.exists(s => s.contains("HELLO_GLOBAL_LOC") && s.matches(""".*ReportSourceLocationTester\.scala:\d+: NOTE HELLO_GLOBAL_LOC.*""")))
  }

  test("report default source location format is SEVERITY(FILE:LINE) MSG") {
    val outDir = Files.createTempDirectory("spinalhdl_report_source_location_default_fmt_").toFile

    val spinalReport = SpinalConfig(targetDirectory = outDir.getAbsolutePath)
      .generateVerilog(new Component {
        report("HELLO_DEFAULT_FMT", includeSourceLocation = true)
      })

    val rtlPath = spinalReport.generatedSourcesPaths.find(_.endsWith(".v")).getOrElse {
      fail(s"No Verilog file generated, got: ${spinalReport.generatedSourcesPaths.mkString(", ")}")
    }

    val rtl = new String(Files.readAllBytes(Paths.get(rtlPath)), StandardCharsets.UTF_8)
    val formats = displayFormatStrings(rtl)
    assert(formats.exists(_.matches(""".*NOTE\(.*\.scala:\d+\) HELLO_DEFAULT_FMT.*""")))
  }
}
