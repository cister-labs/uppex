package uppx.semantics

import scala.io.Source

object Uppaal:
  sealed trait Block
  case class Content(c:String) extends Block
  case class AnnotationBl(name:String,oldLines:List[String],newLines:List[String]) extends Block

  case class Model(blocks:List[Block]):
    def getDiff: List[AnnotationBl] =
      for AnnotationBl(n,txt,newTxt) <-blocks if txt != newTxt yield AnnotationBl(n,txt,newTxt)


    def getPrettyDiff: String =
      (for AnnotationBl(n,txt,newTxt) <-getDiff
        yield
          val diff = for (l,r) <- txt.zipAll(newTxt,"","") if l!=r yield (s"| $l",s"| $r")
          val (txt2,newTxt2) = diff.unzip
          s"=== @$n ===\n${txt2.mkString("\n")}\n|-- becomes --\n${newTxt2.mkString("\n")}")
        .mkString("\n")

    override def toString: String =
      (for b<-blocks yield s" | $b").mkString("\n")

  def buildNew(m:Model): String =
      (for b<-m.blocks yield build(b,true)).mkString("\n")

  def buildOld(m:Model): String =
    (for b<-m.blocks yield build(b,false)).mkString("\n")

  private def build(b: Block,isNew: Boolean): String = b match
    case Content(c) => c
    case AnnotationBl(n,_,newTxt) if isNew => s"// @${n}\n${newTxt.mkString("\n")}\n"
    case AnnotationBl(n,oldTxt,_)          => s"// @${n}\n${oldTxt.mkString("\n")}\n"




//  def main(args: Array[String]): Unit =
//    val fileStr = Source.fromFile("uppaal3.xml").getLines().mkString("\n")
//    val model = UppaalParser.parse(fileStr)
//    println(s"===\n$model\n===\n${model.getAnnotations.mkString("\n")}")


