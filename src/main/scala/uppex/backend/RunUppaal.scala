package uppex.backend

import uppex.semantics.{Configurations, Uppaal}
import uppex.syntax.Report

import java.io.{File, IOException, PrintWriter}
import scala.sys.process.Process

object RunUppaal:


  def checkProduct(prod: String, confProd: Configurations, model: Uppaal.Model, timeout: Int, rep: Report) =
//    val confProd = ExcelParser.parse(excel, prod)
//    val file = File.createTempFile(upp.dropRight(4), ".xml")
//    val (model, original) = UppaalParser.parseFile(upp, confProd)
    val file = File.createTempFile("uppex", ".xml")
    println(s"---Verifying '$prod'---") //Running: verifyta ${file.getAbsolutePath}")
    rep.addProduct(prod)
    val pw = new PrintWriter(file)
    pw.write(Uppaal.buildNew(model))
    pw.close()
    //      val reply = s"timeout 5 verifyta ${file.getAbsolutePath}".!!
    var replies = LazyList[String]()
    try
      replies =
        if timeout<=0
        then Process(s"verifyta -T ${file.getAbsolutePath}").lazyLines
        else Process(s"timeout $timeout verifyta -T ${file.getAbsolutePath}").lazyLines
    catch
      case e:java.io.IOException =>
        throw new IOException(s"Failed to run command 'timeoutt $timeout verifyta -T ${
          file.getAbsolutePath}'.\n"+
          "Check if 'timeout' command is installed and Uppaal's 'verifyta' command is in your $PATH.\n"+
          e.getMessage)

    var buff = ""
    val queries = confProd.xmlBlocks.get("queries")
    val total = queries.map(_.attrs.size).getOrElse(0)

    def checkStat(s: String): Option[Boolean] = s.headOption match
      //ok, failed(some(false)), or aborted(none)
      case Some('s') => Some(true)
      case Some('N') => Some(false)
      case _ => None

    def printStat(res: Option[Boolean]): String = res match
      case Some(true) => "OK"
      case Some(false) => "FAIL"
      case _ => "Aborted"

    try {
      var counter = 0
      for r <- replies do
        if r.startsWith("Verifying") then
          counter += 1
          val stat = s"$counter/$total "
          print(stat + "\b".repeat(stat.length))
        buff += r
      // Possible answers (patterns to search):
      //  - "Formula is satisfied"
      //  - "Formula is NOT satisfied"
      //  - "Aborted" (some exception, such as an overflow)
      val answ = buff.split("Formula is |Aborted").map(checkStat).toList.tail
      val comments = for
        qs <- queries.toList
        line <- qs.attrs.values.toList.sortWith((x, y) => x._1 < y._1)
        comm <- line._2.get(qs.header.indexOf("Comment"))
      yield
        comm
      println(comments.zip(answ).map((s, b) => s"[${printStat(b)}] $s").mkString("\n"))
      comments.zip(answ).foreach((s, b) => b match //if b then rep.addOk(s) else rep.addFail(s))
        case Some(true) => rep.addOk(s)
        case Some(false) => rep.addFail(s)
        case _ => rep.addFail("(Aborted) " + s))
    }
    catch {
      case e: RuntimeException =>

        val answ = buff.split("Formula is |Aborted").map(checkStat).toList.tail
        //buff.split("Formula is ").map(!_.startsWith("NOT")).toList.tail
        val comments = for
          qs <- queries.toList
          line <- qs.attrs.values.toList.sortWith((x, y) => x._1 < y._1)
          //qs.attrs // map: formula -> (line number, rowNr->attribute))
          //            .toList
          //            .sortBy(_._2._1) // sorting lines by how they appear in the file
          comm <- line._2.get(qs.header.indexOf("Comment"))
        yield
          comm
        println(comments.zip(answ).map((s, b) => s"[${printStat(b)}] $s").mkString("\n"))
        comments.zip(answ).foreach((s, b) => b match //if b then rep.addOk(s) else rep.addFail(s))
          case Some(true) => rep.addOk(s)
          case Some(false) => rep.addFail(s)
          case _ => rep.addFail("(Aborted) " + s))
        //          println(s"c:${comments.size}, a:${answ.size}")
        if (comments.size > answ.size) then
          val missing = comments.drop(answ.size)
          println(s"  | Error or time-out after ${timeout}s. Missing ${missing.size} properties. Failed on property:\n  | \"${
            uppex.syntax.ExcelParser.fixFromXML(missing.head)
          }\"")
          rep.addTO(missing)
      //          for r <- replies do
      //            println(s"line2: $r")
      case e: Throwable => throw e
    }
