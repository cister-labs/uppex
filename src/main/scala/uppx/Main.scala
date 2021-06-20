package uppx

import org.apache.poi.ss.usermodel.WorkbookFactory
import uppx.semantics.Uppaal.AnnotationBl
import uppx.syntax.{ExcelParser, UppaalParser}
import uppx.semantics.{Annotations, Uppaal}

import java.io.{File, PrintWriter}

object Main extends App:
  if args == null then applyProperties("uppaal")
  else args.headOption match
    case Some(baseName) => applyProperties(baseName)
    case None => applyProperties("uppaal")



//  @main
  def applyProperties(baseName:String) =
    val propFile = baseName+".xlsx"
    val uppFile = baseName+".xml"

    println(s"> Reading properties from $propFile")
    val anns: Annotations = ExcelParser.parse(propFile)
    println(s"> Reading Uppaal file $uppFile")
    val model = UppaalParser.parseFile(uppFile,anns)

    println(" - Annotations in properties: "+anns.anns.keys.mkString(", "))
    println(" - Annotations in Uppaal: "+(for AnnotationBl(a,_,_)<-model.blocks yield a).mkString(", "))

    if model.getDiff.isEmpty then
      println(s"\n> No differences detected. File $uppFile not updated.")
    else
      println(model.getPrettyDiff)
      println(s"\n> Updating file $uppFile")

    val pw = new PrintWriter(new File(uppFile ))
    pw.write(Uppaal.buildNew(model))
    pw.close

//    for ann <- anns.anns do
//      println(ann)
//      println("---")
//      println(ann._1+": "+ ann._2.instantiateAll.mkString("\n"))
//      println("===")


//    println(s"===\n$model\n===\n${model.getPrettyDiff}")
//    println(s"\n# New file:\n${Uppaal.buildNew(model)}")
