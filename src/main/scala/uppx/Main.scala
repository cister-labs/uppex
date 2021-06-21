package uppx

import org.apache.poi.ss.usermodel.WorkbookFactory
import uppx.semantics.Uppaal.{AnnotationBl, Model}
import uppx.syntax.{ExcelParser, UppaalParser}
import uppx.semantics.{Annotations, Uppaal}

import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Calendar

object Main:
  // when extending App, `args` is alyways null
  def main(args: Array[String]): Unit =
    if args == null then applyProperties("uppaal")
    else args.headOption match
      case Some("--help") | Some("-h") => println("usage: uppx.jar [baseName]")
      case Some(baseName) => applyProperties(baseName)
      case None => applyProperties("uppaal")



//  @main
  def applyProperties(baseName:String) =
    val propFile = baseName+".xlsx"
    val uppFile = baseName+".xml"

    println(s"> Reading properties from '$propFile'")
    val anns: Annotations = ExcelParser.parse(propFile)
    println(s"> Reading Uppaal file '$uppFile'")
    val (model,original) = UppaalParser.parseFile(uppFile,anns)

    println(" - Annotations in properties: "+anns.anns.keys.mkString(", "))
    println(" - Annotations in Uppaal: "+(for AnnotationBl(a,_,_)<-model.blocks yield a).mkString(", "))

    if model.getDiff.isEmpty then
      println(s"\n> No differences detected. File '$uppFile' not updated.")
    else
      println(model.getPrettyDiff)
      updateUppaal(model,original,baseName,uppFile)

//    for ann <- anns.anns do
//      println(ann)
//      println("---")
//      println(ann._1+": "+ ann._2.instantiateAll.mkString("\n"))
//      println("===")


//    println(s"===\n$model\n===\n${model.getPrettyDiff}")
//    println(s"\n# New file:\n${Uppaal.buildNew(model)}")

  private def updateUppaal(model: Model, original: String, baseName: String, uppFile: String): Unit =
    backupOld(model, baseName, original: String)
    println(s"\n> Updating file '$uppFile'")
    val pw = new PrintWriter(new File(uppFile))
    pw.write(Uppaal.buildNew(model))
    pw.close

  private def backupOld(model: Model, baseName: String, original: String): Unit =
    val backupFile = baseName+"-"+
      (new SimpleDateFormat("yy-mm-dd_hh.mm.ss"))
        .format(Calendar.getInstance.getTime)+
      ".xml"
    println(s"\n> Backing up previous version in 'backups/$backupFile'")

    val file = new File(s"backups/$backupFile")
    require(file.getParentFile.exists() || file.getParentFile.mkdirs(),
      "Backup's directory creation failed")
    val pw = new PrintWriter(file)
    pw.write(original) // Safer with `original`, but would also work: Uppaal.buildOld(model)
    pw.close