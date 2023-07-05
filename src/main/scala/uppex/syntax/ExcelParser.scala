package uppex.syntax

//import com.microsoft.schemas.office.visio.x2012.main.CellType
import org.apache.poi.ss.usermodel.*
import uppex.semantics.{Annotations, Configurations, FeatureModel, Uppaal}
import Annotations.Annotation
import uppex.semantics.Configurations.{FProd, Products}
import FeatureModel.{Card, FM, FMTable, FeatureCell, FeatureTable, show}
import uppex.semantics
import uppex.syntax.FeatExprParser.ParseException

import java.util.ServiceLoader
import scala.collection.mutable.ListBuffer
import scala.io.Source
//import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import java.io.FileInputStream
import scala.jdk.CollectionConverters._
//import scala.collection.convert.ImplicitConversions

import java.io.File

object ExcelParser {

  type Loc = (Int, Int)
  def loc(c:Cell): Loc = c.getColumnIndex->c.getRowIndex
  def show(l: Loc)(using sheet:String=""): String =
    (if sheet != "" then s"'$sheet'!" else "")+
      s"${(l._1 + 65).toChar}${l._2 + 1}"



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

    var prods = extractConfig(isConfig)
    val fm = extractFM(isFM)
    fm match
      case Some(fm2) =>
        val (errs, prods2) = FeatureModel.validate(prods, fm2)
        prods = prods2
        val err = errs.map(err => s" - $err")
        if err.nonEmpty then
          sys.error(s"Products are invalid. Stopping.\n${err.mkString("\n")}\n$fm2")
      case _ =>

    val feats = prods.getOrElse(product,Map())
    val ann = extractAnnotations(isAnnot,_.tail,x=>x,feats)
    val xml = extractAnnotations(tagRx.matches,_.tail.init,fixToXML,feats)

    wb.close()

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

    /** Gets a cell from the row, and pre-processes it before being sent to the annotations */
    def getRowCell(r: Row, i: Int)(using sheet:String): String =
      fix(evalProd(feats, evalString(r.getCell(i))))

    //    for sheet <- wb.asScala.toSet do
//      val s = sheet.getSheetName
//      println(s"§ $s checks? - ${check(s)}")
    for sheet <- wb.asScala.toSet
        if check(sheet.getSheetName) && hasPattAndHeader(sheet) yield
      implicit val sheetName = sheet.getSheetName
      val attr = clean(sheetName)
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
        val first = getRowCell(row,0)
        if first!="" then
          val newRow = (for i <- 1 until size
            yield i -> getRowCell(row,i)).toMap
//          try
          if selected(feats,newRow,featIdx,row.getRowNum)
          then ann = ann + (newRow+(0->first)) /// ADD FEATURE VALUES HERE!!
//          catch
//            case e:FEParseException => throw new FEParseException(e.getMessage+s" in ${sheet.getSheetName}!${FeatureModel.show()}")
      attr -> ann //Annotation(ann.pattern,ann.header,ann.attrs.reverse)

  private def evalProd(feats:FProd,str:String): String =
    val varPattern = """\$([a-zA-Z0-9\-_]+)|\$\{([a-zA-Z0-9\-_]+)\}""".r
    varPattern.replaceAllIn(str, mtch => mtch.group(1) match
      case null => ""
      case x if !feats.contains(x) => ""
      case x => feats(x).toString
    )

  /**
   * Checks if a given row should be selected based on its features
   * @param product the set of selected features
   * @param row the row that is being checked
   * @param x the index of the column where the feature expression can be found
   * @return true if the given row should be selected.
   */
  private def selected(product:FProd, row:Map[Int,String],
                       x:Int, y:Int)(using sheet:String = ""): Boolean =
    // a row is selected if its features' column us empty OR if all mentioned features are active
    if !row.contains(x) then true
    else
      val strFeatExpr = fixFromXML(row(x))
      FeatExprParser.eval(FeatExprParser.parse(strFeatExpr,x->y))(using product)

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
      implicit val sheetName = sheet.getSheetName
      val header: List[String] = sheet.getRow(0).asScala.map(evalString).toList // row 1 (not 2!)
      val size = header.size
      var products = Map[String,FProd]()
      for row <- sheet.asScala if row.getRowNum > 0 && evalString(row.getCell(0))!="" do //skip first row with header (features)
        val newRow = for i <- 1 until size// skip first column with the product name
          if evalString(row.getCell(i))!=""
          yield header(i)->evalString(row.getCell(i))
        products += evalString(row.getCell(0)) -> newRow.toMap
      products //Annotation(ann.pattern,ann.header,ann.attrs.reverse)
    ).flatten.toMap

  /** Evaluate the content of a specific cell. */
  private def evalString(c:Cell)(using wb:Workbook, sheet:String = ""): String = try
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
    catch
      case _: java.lang.IllegalArgumentException =>
        //println(s" - FAILED to evaluate cell ${show(loc(c))} ${c.getCellFormula}")
        c.getCellFormula
      case e: java.lang.RuntimeException =>
        throw new RuntimeException(s"at ${show(loc(c))}: ${e.getMessage}")
      case e: Throwable => throw e

  /**
   * Infer a feature model from a @FeatureModel spreadsheet
   * @param check function that checks if the name of the spreadsheet is correct
   * @param wb full excep spreadsheets
   * @return an optional Feature model
   */
  def extractFM(check: String => Boolean)(using wb:Workbook): Option[FM] =
    (for sheet <- wb.asScala.toSet if check(sheet.getSheetName) yield  // for the "@FeatureModel spreadsheet
      //println(s"[DEBUG] extracting FM: sheet ${sheet.getSheetName}")
      implicit val sheetName = sheet.getSheetName
      var fcs = Set[FeatureCell]()
      var fs = Set[String]()
      var cs = List[FeatureModel.Constraint]()
      var card = Set[(FeatureModel.Card,String,Loc)]()
      val last = ListBuffer[FeatureCell]()

      // extract non-# rows (feature tree)
      for (row,_) <- sheet.asScala.zipWithIndex if !evalString(row.getCell(0)).startsWith("#") do
        //val sheet.getRow(0).asScala.map(evalString).toList
        for (cell,_) <- row.asScala.zipWithIndex do
//            if evalString(cell) != "" do
           val x = cell.getColumnIndex
           val y = cell.getRowIndex
           val cellVal = evalString(cell)
           if x>last.size then
             sys.error(s"Feature $cellVal at ${show(x->y)} without a parent.")
           if fs contains cellVal then
             sys.error(s"Feature $cellVal at ${show(x->y)} is repeated.")
           if cellVal.nonEmpty then
             val featCell = if x==0
                            then FeatureCell(cellVal,None,x->y)
                            else FeatureCell(cellVal,Some(last(x-1)),x->y)
             fs  += cellVal
             fcs += featCell
             last.takeInPlace(x) += featCell

      // extract constraints and cardinality restrictions
      for (row,_) <- sheet.asScala.zipWithIndex if evalString(row.getCell(0)).startsWith("#") do
        val y = row.getRowNum
        evalString(row.getCell(0)).drop(1).split(" +",2) match
          case Array("constraint",rest) => cs ::= FeatExprParser.parse(rest,0->y)->(0->y)
          case Array("or",rest) => card += (Card.Or,rest,(0,y))
          case Array("alternative",rest) => card += (Card.Alt,rest,(0,y))
          case Array("optional",rest) => card += (Card.Opt,rest,(0,y))
          case x => sys.error(s"Unknown operator in feature model: ${x.mkString(" ")}")

      //println(s"[DEBUG] finishing extracting. About to preprocess")
      val fmtable = FMTable(FeatureTable(fcs),cs.reverse,card.toList)
      val res = FeatureModel.preprocess(fmtable)
      //println(s"[DEBUG] finishing preprocessing")
      res

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
