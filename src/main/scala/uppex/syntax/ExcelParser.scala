package uppex.syntax

//import com.microsoft.schemas.office.visio.x2012.main.CellType
import org.apache.poi.ss.usermodel.*
import uppex.semantics.{Annotations, Configurations, FeatureModel, Uppaal}
import Annotations.Annotation
import uppex.semantics.Configurations.{FProd, Products}
import FeatureModel.{Card, FM, FMTable, FeatureCell, FeatureTable, show}
import uppex.semantics

import java.util.ServiceLoader
import scala.collection.mutable.ListBuffer
import scala.io.Source
//import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import java.io.FileInputStream
import scala.jdk.CollectionConverters._
//import scala.collection.convert.ImplicitConversions

import java.io.File

object ExcelParser {

  /**
   * Extracts all configurations (@-annotations, XML-annotations, and @Configuration)
   * @param pathName path until the file with the spreadsheet to be parsed
   * @param product name of the product to be used
   * @return All 3 configurations (@-annotations, XML-annotations, and the @Configuration (existing products))
   */
  def parse(pathName: String, product:String): Configurations =
//    val cl: ClassLoader = WorkbookFactory.getClass.getClassLoader()
//    val cl = classOf[WorkbookFactory].getClassLoader//ExcelParser.getClass.getClassLoader
//    val sl = ServiceLoader.load(classOf[WorkbookProvider], cl);
//    sl.forEach(x => print(s"--- $x : ${x.getClass}"))
    val file = new File(pathName)
    implicit val wb = WorkbookFactory.create(file)
//    val format = new DataFormatter().formatCellValue(_) // to display content of cells

    def isConfig(name:String): Boolean = name.toLowerCase == "@configurations"
    def isFM(name:String): Boolean = name.toLowerCase == "@featuremodel"
    def isAnnot(name:String):Boolean = (name.headOption contains '@') && !isConfig(name) && !isFM(name)
    val tagRx = "<[a-zA-Z0-9_\\-]*>".r

    val prods = extractConfig(isConfig)
    val feats = prods.getOrElse(product,Map())
    val ann = extractAnnotations(isAnnot,_.tail,x=>x,feats)
    val xml = extractAnnotations(tagRx.matches,_.tail.init,fixToXML,feats)
    val fm = extractFM(isFM)

    if fm.isEmpty then println("no FM found") else println(s"---\nFound FM:\n${fm.get}\n---")

    wb.close()

    fm match
      case Some(fm2) =>
        val err = FeatureModel.validate(prods,fm2).map(err => s" - $err")
        if err.nonEmpty then
          sys.error(s"Products are invalid. Stopping.\n${err.mkString("\n")}")
      case _ =>
    Configurations(prods, Annotations(ann.toMap) , Annotations(xml.toMap), fm)

  /**
   * Extracts all annotations from all sheets that are expected to contain annotations
   * @param check function that infers, based on the sheet name, if it is an annotation table
   * @param clean function that extracts the annotation name from the sheet name
   * @param fix function that pre-processes each cell imported as an annotation
   * @param feats set of features in the selected product
   * @param wb reference to the full spreadsheet
   * @return a collection of annotation tables, each labelled with their name
   */
  private def extractAnnotations(check:String=>Boolean,clean:String=>String,
                                 fix:String=>String, feats:FProd)
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
        val first = fix(evalString(row.getCell(0)))
        if first!="" then
          val newRow = (for i <- 1 until size
            yield i -> fix(evalString(row.getCell(i)))).toMap
          if selected(feats,newRow,featIdx)
          then ann = ann + (newRow+(0->first))
      attr -> ann //Annotation(ann.pattern,ann.header,ann.attrs.reverse)

  /**
   * Checks if a given row should be selected based on its features
   * @param product the set of selected features
   * @param row the row that is being checked
   * @param idx the index of the column where the feature expression can be found
   * @return true if the given row should be selected.
   */
  private def selected(product:FProd, row:Map[Int,String], idx:Int): Boolean =
    // a row is selected if its features' column us empty OR if all mentioned features are active
    if !row.contains(idx) then true
    else
      val strFeatExpr = fixFromXML(row(idx))
      FeatExprParser.eval(FeatExprParser.parse(strFeatExpr))(using product)

  //    // return true if no feature exists
//    if !row.contains(idx)  || row(idx).trim=="" then true
//    else
//      val lineFeats = row(idx).split(",").map(_.trim).toSet - ""
//      lineFeats.forall(product)

  /**
   * Parses the "@Configurations" sheet.
   * @param check Check if the name of the sheet is "@Configurations"
   * @param wb the reference to the full spreadsheet
   * @return a mapping from each configuration name to its selected features.
   */
  def extractConfig(check: String => Boolean)(using wb:Workbook): Products =
    (for sheet <- wb.asScala.toSet if check(sheet.getSheetName) yield
      val header: List[String] = sheet.getRow(0).asScala.map(evalString).toList // row 1 (not 2!)
      val size = header.size
      var products = Map[String,FProd]()
      for row <- sheet.asScala if row.getRowNum > 0 do
        val newRow = for i <- 1 until size// skip first row with the product name
          if evalString(row.getCell(i))!=""
          yield header(i)->evalString(row.getCell(i))
        products += evalString(row.getCell(0)) -> newRow.toMap
      products //Annotation(ann.pattern,ann.header,ann.attrs.reverse)
    ).flatten.toMap

  /** Evaluate the content of a specific cell. */
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

  /**
   * Infer a feature model from a @FeatureModel spreadsheet
   * @param check function that checks if the name of the spreadsheet is correct
   * @param wb full excep spreadsheets
   * @return an optional Feature model
   */
  def extractFM(check: String => Boolean)(using wb:Workbook): Option[FM] =
    (for sheet <- wb.asScala.toSet if check(sheet.getSheetName) yield  // for the "@FeatureModel spreadsheet
      var fs = Set[FeatureCell]()
      var cs = Set[FeatureModel.Constraint]()
      var card = Set[(FeatureModel.Card,String,FeatureModel.Loc)]()
      val last = ListBuffer[FeatureCell]()

      // for non-# rows
      for (row,_) <- sheet.asScala.zipWithIndex if !evalString(row.getCell(0)).startsWith("#") do
        //val sheet.getRow(0).asScala.map(evalString).toList
        for (cell,_) <- row.asScala.zipWithIndex do
//            if evalString(cell) != "" do
           val x = cell.getColumnIndex
           val y = cell.getRowIndex
           val cellVal = evalString(cell)
           if x>last.size then sys.error(s"Feature $cellVal at ${show(x->y)} without a parent.")
           if cellVal.nonEmpty then
             val featCell = if x==0
                            then FeatureCell(cellVal,None,x->y)
                            else FeatureCell(cellVal,Some(last(x-1)),x->y)
             fs += featCell
             last.takeInPlace(x) += featCell

      for (row,_) <- sheet.asScala.zipWithIndex if evalString(row.getCell(0)).startsWith("#") do
        val y = row.getRowNum
        evalString(row.getCell(0)).drop(1).split(" +",2) match
          case Array("constraint",rest) => cs += FeatExprParser.parse(rest)->(0->y)
          case Array("or",rest) => card += (Card.Or,rest,(0,y))
          case Array("alternative",rest) => card += (Card.Alt,rest,(0,y))
          case Array("optional",rest) => card += (Card.Opt,rest,(0,y))
          case x => sys.error(s"Unknown operator in feature model: ${x.mkString(" ")}")


      val fmtable = FMTable(FeatureTable(fs),cs.toList,card.toList)
      FeatureModel.preprocess(fmtable)

    ).headOption //getOrElse(FeatureModel.emptyFM)


  /** Convert &,<,> FROM their html counterpart */
  def fixFromXML(str:String): String = str
    .replaceAll("&amp;","&")
    .replaceAll("&lt;","<")
    .replaceAll("&gt;",">")
  /** Convert &,<,> TO their html counterpart */
  def fixToXML(str:String): String = str
    .replaceAll("&","&amp;")
    .replaceAll("<","&lt;")
    .replaceAll(">","&gt;")


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
