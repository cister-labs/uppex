package uppx.syntax

//import com.microsoft.schemas.office.visio.x2012.main.CellType
import org.apache.poi.ss.usermodel._
import uppx.semantics.{Annotations, Uppaal}
import Annotations.Annotation

import java.util.ServiceLoader
import scala.io.Source
//import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import java.io.FileInputStream
import scala.jdk.CollectionConverters._
//import scala.collection.convert.ImplicitConversions

import java.io.File

object ExcelParser {

  def parse(pathName: String): Annotations =
//    val cl: ClassLoader = WorkbookFactory.getClass.getClassLoader()
//    val cl = classOf[WorkbookFactory].getClassLoader//ExcelParser.getClass.getClassLoader
//    val sl = ServiceLoader.load(classOf[WorkbookProvider], cl);
//    sl.forEach(x => print(s"--- $x : ${x.getClass}"))
    val wb = WorkbookFactory.create(new File(pathName))
    val format = new DataFormatter().formatCellValue(_) // to display content of cells
    val eval = wb.getCreationHelper.createFormulaEvaluator.evaluate(_) // to calculate values of cells
    def evalString(c:Cell): String =
      if c == null then return ""
      val value = eval(c)
      if value == null then return ""
      value.getCellType.name() match
        case "STRING" => value.getStringValue
        case _ => value.formatAsString()
    val res = for sheet <- wb.asScala.toSet
                  if sheet.getSheetName.headOption contains '@' yield
        val attr = sheet.getSheetName.tail
        val patt = evalString(sheet.getRow(0).getCell(0))// position 1.1
        val header: List[String] = sheet.getRow(1).asScala.map(evalString).toList // row 2
        var ann = Annotation(patt,header,Nil)
        for row <- sheet.asScala
            if row.getRowNum > 1
          do
            ann = ann + (row
              .asScala
              .zipWithIndex
              .map(pair=>(pair._2,evalString(pair._1)))
              .toMap)
        attr -> ann
    Annotations(res.toMap)


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
