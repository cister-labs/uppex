package uppex

import org.apache.poi.ss.usermodel.WorkbookFactory
import uppex.semantics.Uppaal.{AnnotationBl, Model, XmlBl, getDiff, getPrettyDiff}
import uppex.syntax.{ExcelParser, Report, UppaalParser}
import uppex.semantics.{Annotations, Configurations, Uppaal}

import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Calendar
import sys.process.*

object Main:
  // when extending App, `args` is alyways null
  def main(args: Array[String]): Unit =
    def help: Unit =  println("Usage: uppex.jar [--run | --runAll] [-t <timout>] [--info] [-p productName] <inputFile.xlsx>")
    if args == null then help
    else args.toList match
//      case "--runAll"::Nil =>
//        runAllChecks("uc10-nonreactive.xml")
//      case "--run"::Nil =>
//        runChecks("uc10-nonreactive","Main")
//      case Nil => applyAndUpdateUppaal("uc10-nonreactive")
      ////
      case "--help"::_ | "-h"::_ => help
      case "--functions"::_ | "-f"::Nil =>
        println(org.apache.poi.ss.formula.eval.FunctionEval.getSupportedFunctionNames.toArray.mkString("\n"))
      case "--info"::"-p"::prod::baseName::Nil =>
        applyProperties(baseName,prod)
      case "--runAll"::baseName::Nil =>
        runAllChecks(baseName)
      case "--runAll"::"-t"::n::baseName::Nil =>
        runAllChecks(baseName,n.toInt)
      case "--run"::"-p"::prod::baseName::Nil =>
        runChecks(baseName,prod)
      case "--run"::"-t"::n::"-p"::prod::baseName::Nil =>
        runChecks(baseName,prod,n.toInt)
      case "--run"::baseName::Nil =>
        runChecks(baseName,"Main")
      case baseName::Nil => applyAndUpdateUppaal(baseName)
      case "-p"::prod::baseName::Nil => applyAndUpdateUppaal(baseName,prod)
      case x => println(s"Unknown options: ${x.mkString(" ")}"); help



//  @main
  def applyAndUpdateUppaal(baseName:String, product:String="Main"): Unit =
    applyProperties(baseName,product) match
      case (model,original,_,uppFileName,true) => updateUppaal(model,original,uppFileName)
      case _ =>


  def getFileNames(basename:String): (String,String) =
    val res =
      if basename.toLowerCase.endsWith(".xlsx") then
        (basename,basename.dropRight(4)+"xml")
      else if basename.toLowerCase.endsWith(".xml") then
        (basename.dropRight(3)+"xlsx",basename)
      else (basename+".xlsx",basename+".xml")
    if !(new File(res._1).exists) then
      throw new RuntimeException(s"File ${res._1} not found.")
    if !(new File(res._2).exists) then
      throw new RuntimeException(s"File ${res._2} not found.")
    res


  def applyProperties(baseName:String, product:String): (Model,String,Configurations,String,Boolean) =
    val (propFile,uppFile) = getFileNames(baseName)
//    val propFile = baseName+".xlsx"
//    val uppFile = baseName+".xml"

    println(s"> Reading properties from '$propFile'")
    val conf = ExcelParser.parse(propFile,product)
    println(s"> Reading Uppaal file '$uppFile'")
    val (model,original) = UppaalParser.parseFile(uppFile,conf)

    println(" - Products in properties: "+ conf.products.map((n,s) => s"$n:{${s.mkString(",")}}").mkString("; "))

    println(" - Annotations in properties: "+ conf.annotations.anns.keys.mkString(", "))
    println(" - Annotations in Uppaal: "+(for AnnotationBl(a,_,_)<-model.blocks yield a).mkString(", "))

    println(" - Tags in properties: "+ conf.xmlBlocks.anns.keys.mkString(", "))
    println(" - Tags in Uppaal: "+(for XmlBl(a,_,_)<-model.blocks yield a).mkString(", "))

    if getDiff(model).isEmpty then
      println(s"\n> No differences detected. File '$uppFile' not updated.")
      (model,original,conf,uppFile,false)
    else
      println(getPrettyDiff(model))
      (model,original,conf,uppFile,true)
//      updateUppaal(model,original,baseName,uppFile)


  private def updateUppaal(model: Model, original: String, uppFile: String): Unit =
    backupOld(model, uppFile, original: String)
    println(s"\n> Updating file '$uppFile'")
    val pw = new PrintWriter(new File(uppFile))
    pw.write(Uppaal.buildNew(model))
    pw.close()

  private def backupOld(model: Model, uppFileName: String, original: String): Unit =
    val backupFile = uppFileName.dropRight(4)+"-"+
      (new SimpleDateFormat("yy-MM-dd_HH.mm.ss"))
        .format(Calendar.getInstance.getTime)+
      ".xml"
    println(s"\n> Backing up previous version in 'backups/$backupFile'")

    val file = new File(s"backups/$backupFile")
    require(file.getParentFile.exists() || file.getParentFile.mkdirs(),
      "Backup's directory creation failed")
    val pw = new PrintWriter(file)
    pw.write(original) // Safer with `original`, but would also work: Uppaal.buildOld(model)
    pw.close()


  private def runAllChecks(basename:String, timeout:Int = 30): Unit =
    val (excel,upp) = getFileNames(basename)
//    val excel = basename+".xlsx"
    val conf = ExcelParser.parse(excel,"Main")
    println(s"> Reading Uppaal file '$upp'")
    val rep = new Report(basename,timeout)
    for prod <- conf.products.keys if prod!="" do
      checkProduct(prod,excel,upp,conf,timeout,rep)
    rep.writeFile(s"report.html")

  private def runChecks(basename:String,prod:String, timeout:Int = 30): Unit =
    val (excel,upp) = getFileNames(basename)
    val conf = ExcelParser.parse(excel,"Main")
    println(s"> Reading Uppaal file '$upp'")
    val rep = new Report(basename,timeout)
    checkProduct(prod,excel,upp,conf,timeout,rep)
    rep.writeFile(s"report.html")

  private def checkProduct(prod:String, excel:String, upp:String ,conf:Configurations, timeout:Int, rep:Report) =
    val confProd = ExcelParser.parse(excel,prod)
    val file = File.createTempFile(upp.dropRight(4),".xml")
    val (model,original) = UppaalParser.parseFile(upp,confProd)
    println(s"---Verifying '$prod'---") //Running: verifyta ${file.getAbsolutePath}")
    rep.addProduct(prod)
    val pw = new PrintWriter(file)
    pw.write(Uppaal.buildNew(model))
    pw.close()
//      val reply = s"timeout 5 verifyta ${file.getAbsolutePath}".!!
    val replies = Process(s"timeout $timeout verifyta -T ${file.getAbsolutePath}").lazyLines
    var buff = ""
    val queries = confProd.xmlBlocks.get("queries")
    val total = queries.map(_.attrs.size).getOrElse(0)
    try {
      var counter = 0
      for r <- replies do
        if r.startsWith("Verifying") then
          counter += 1
          val stat = s"$counter/$total "
          print(stat+"\b".repeat(stat.length))
        buff += r
      val answ = buff.split("Formula is ").map(!_.startsWith("NOT")).toList.tail
      val comments = for
        qs <- queries.toList
        line <- qs.attrs.values.toList.sortWith((x,y)=>x._1<y._1)
        comm <- line._2.get(qs.header.indexOf("Comment"))
      yield
        comm

      println(comments.zip(answ).map((s, b) => s"[${if b then "OK" else "FAIL"}] $s").mkString("\n"))
      comments.zip(answ).foreach((s, b) => if b then rep.addOk(s) else rep.addFail(s))
    }
    catch {
      case e:RuntimeException =>
        val answ = buff.split("Formula is ").map(!_.startsWith("NOT")).toList.tail
        val comments = for
          qs <- queries.toList
          line <- qs.attrs.values.toList.sortWith((x,y)=>x._1<y._1)
            //qs.attrs // map: formula -> (line number, rowNr->attribute))
//            .toList
//            .sortBy(_._2._1) // sorting lines by how they appear in the file
          comm <- line._2.get(qs.header.indexOf("Comment"))
        yield
          comm
        println(comments.zip(answ).map((s, b) => s"[${if b then "OK" else "FAIL"}] $s").mkString("\n"))
        comments.zip(answ).foreach((s, b) => if b then rep.addOk(s) else rep.addFail(s))
//          println(s"c:${comments.size}, a:${answ.size}")
        if (comments.size > answ.size) then
          val missing = comments.drop(answ.size)
          println(s"  | Time-out after ${timeout}s. Missing ${missing.size} properties. Failed on property:\n  | \"${
              ExcelParser.fixFromXML(missing.head)}\"")
          rep.addTO(missing)
//          for r <- replies do
//            println(s"line2: $r")
      case e:Throwable => throw e
    }


//  private def runChecks2(basename:String,prod:String) =
//    val (model,content,conf,uppFile,updated) = applyProperties(basename, prod)
//    val file = File.createTempFile(s"$uppFile-",".xml");
//    println(s"\n> running: verifyta ${file.getAbsolutePath}")
//    val pw = new PrintWriter(file)
//    pw.write(Uppaal.buildNew(model))
//    pw.close()
//    val reply = s"verifyta ${file.getAbsolutePath}".!!
//    val answ = reply.split("Formula is ").map(!_.startsWith("NOT")).toList.tail
//    val queries = conf.xmlBlocks.get("queries")
//    val comments = for
//      qs <- queries.toList
//      line <- qs.attrs
//      comm <- line._2._2.get(qs.header.indexOf("Comment"))
//    yield
//      comm
//
//    println(comments.zip(answ).map((s,b) => s"[${if b then "OK" else "FAIL"}] $prod: $s").mkString("\n"))
//
////    println(s"reply: $reply")
////    println(s"answer: $answ")
////    println(s"queries: $queries")
////    println(s"x: $comments")
////    println(s"zip: ${comments zip answ}")
//
//



