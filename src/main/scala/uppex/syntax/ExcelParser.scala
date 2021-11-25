package uppex.syntax

//import com.microsoft.schemas.office.visio.x2012.main.CellType
import org.apache.poi.ss.usermodel.*
import uppex.semantics.{Annotations, Configurations, Uppaal}
import Annotations.Annotation

import java.util.ServiceLoader
import scala.io.Source
//import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import java.io.FileInputStream
import scala.jdk.CollectionConverters._
//import scala.collection.convert.ImplicitConversions

import java.io.File

object ExcelParser {

  def parse(pathName: String, product:String): Configurations =
//    val cl: ClassLoader = WorkbookFactory.getClass.getClassLoader()
//    val cl = classOf[WorkbookFactory].getClassLoader//ExcelParser.getClass.getClassLoader
//    val sl = ServiceLoader.load(classOf[WorkbookProvider], cl);
//    sl.forEach(x => print(s"--- $x : ${x.getClass}"))
    val file = new File(pathName)
    implicit val wb = WorkbookFactory.create(file)
//    val format = new DataFormatter().formatCellValue(_) // to display content of cells

    def isConfig(name:String): Boolean = name.toLowerCase == "@configurations"
    def isAnnot(name:String):Boolean = (name.headOption contains '@') && !isConfig(name)
    val tagRx = "<[a-zA-Z0-9_\\-]*>".r
    val fixXml: String=>String =
      _.replaceAll("&","&amp;")
        .replaceAll("<","&lt;")
        .replaceAll(">","&gt;")

    val prods = extractConfig(isConfig)
    val feats = prods.getOrElse(product,Set())
    val ann = extractAnnotations(isAnnot,_.tail,x=>x,feats)
    val xml = extractAnnotations(tagRx.matches,_.tail.init,fixXml,feats)

    wb.close()

    Configurations(prods, Annotations(ann.toMap) , Annotations(xml.toMap) )

  private def extractAnnotations(check:String=>Boolean,clean:String=>String,fix:String=>String, feats:Set[String])
                                (using wb: Workbook): Set[(String,Annotation)] =
    def hasPattAndHeader(sheet:Sheet): Boolean =
      try {
        sheet.getRow(0).getCell(0)
        sheet.getRow(1)
        true
      } catch {
        case e:NullPointerException => false
      }

//    for sheet <- wb.asScala.toSet do
//      val s = sheet.getSheetName
//      println(s"§ $s checks? - ${check(s)}")
    for sheet <- wb.asScala.toSet
        if check(sheet.getSheetName) && hasPattAndHeader(sheet) yield
      val attr = clean(sheet.getSheetName)
      //sheet.getRow(0).createCell(3).setCellValue(42) // trying to update the spreadhseet
      ///
      //val fos = new java.io.FileOutputStream(new java.io.File("output.xlsx"))
      //wb.write(fos)
      ///
      val patt = evalString(sheet.getRow(0).getCell(0))// position 1.1
      val header: List[String] = sheet.getRow(1).asScala.map(evalString).toList // row 2
      val size = header.size
      val featIdx = header.indexOf("Features") // -1 if not found
      var ann = Annotation(patt,header,Map())
      for row <- sheet.asScala
          if row.getRowNum > 1 //&&
      // row.getCell(0)!=null &&
      //               evalString(row.getCell(0))!=""
      do
        val newRow = (for i <- 0 until size
          yield i -> fix(evalString(row.getCell(i)))).toMap
        if newRow.map(_._2).exists(_!="") && selected(feats,newRow,featIdx)
        then ann = ann + newRow
      attr -> ann //Annotation(ann.pattern,ann.header,ann.attrs.reverse)

  private def selected(product:Set[String], row:Map[Int,String], idx:Int): Boolean =
    if !row.contains(idx)  || row(idx)=="" then true
    else
      val lineFeats = row(idx).split(",").map(_.trim).toSet - ""
      lineFeats.exists(product)

  private def extractConfig(check: String => Boolean)(using wb:Workbook): Map[String,Set[String]] =
    (for sheet <- wb.asScala.toSet if check(sheet.getSheetName) yield
      val header: List[String] = sheet.getRow(0).asScala.map(evalString).toList // row 1 (not 2!)
      val size = header.size
      var products = Map[String,Set[String]]()
      for row <- sheet.asScala if row.getRowNum > 0 do
        val newRow = for i <- 1 until size// skip first row with the product name
          if evalString(row.getCell(i))!=""
          yield header(i)
        products += evalString(row.getCell(0)) -> newRow.toSet
      products //Annotation(ann.pattern,ann.header,ann.attrs.reverse)
    ).flatten.toMap

  private def evalString(c:Cell)(using wb:Workbook): String =
    if c == null then return ""
    val eval = wb.getCreationHelper.createFormulaEvaluator.evaluate(_) // to calculate values of cells
    val value = eval(c)
    if value == null then return ""
    value.getCellType.name() match
      case "STRING" => value.getStringValue
      case "NUMERIC" =>
        val double = value.getNumberValue
        if double % 1 == 0 then double.toInt.toString else double.toString
      case "BOOLEAN" =>
        value.formatAsString().toLowerCase() // adjust "TRUE" to "true"
      case x =>
        value.formatAsString()


  //  def readTest(pathName: String): Unit =
//    val wb = WorkbookFactory.create(new File(pathName))
//    val format = new DataFormatter().formatCellValue(_) // to display content of cells
//    val eval = wb.getCreationHelper.createFormulaEvaluator.evaluate(_) // to calculate values of cells
//    var i = 1
//    val nbrSheets = wb.getNumberOfSheets
//    for sheet <- wb.asScala do
//      println(s"Sheet $i of $nbrSheets : ${sheet.getSheetName}")
//      i +=1
//      for row <- sheet.asScala do
//        println(s"\tRow ${row.getRowNum}")
//        for cell <- row.asScala do
//          println(s"\t\t ${cell.getAddress.formatAsString()} : ${format(cell)} (${eval(cell).formatAsString()} / ${eval(cell).getCellType.name()} / ${eval(cell).getStringValue})")




//  def main(args: Array[String]): Unit =
//    val anns = parse("uppaal3.xlsx")
//    for ann <- anns.anns do
//      println(ann)
//      println("---")
//      println(ann._1+": "+ ann._2.instantiateAll.mkString("\n"))
//      println("===")
//
//    val fileStr = Source.fromFile("uppaal3.xml").getLines().mkString("\n")
//    val model = UppaalParser.parse(fileStr,anns)
//    println(s"===\n$model\n===\n${model.getDiff.mkString("\n")}")
//
//    println(s"\n# New file:\n${Uppaal.buildNew(model)}")

}
