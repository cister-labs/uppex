package uppex.semantics

import scala.io.Source

object Uppaal:
  sealed trait Block
  case class Content(c:String) extends Block
  sealed abstract class NamedBl(val name:String,val oldLines:Lines,val newLines:Lines) extends Block
  case class AnnotationBl(n:String, oldL:Lines, newL:Lines) extends NamedBl(n,oldL,newL)
  case class XmlBl(n:String, oldL:Lines, newL:Lines) extends NamedBl(n,oldL,newL)

  case class Model(blocks:List[Block]):
    override def toString: String =
      (for b<-blocks yield s" | $b").mkString("\n")


  //////////// Auxiliar functions ////////////

  private type Lines = List[String]

  def getDiff(m:Model): List[NamedBl] =
    for
      (nb:NamedBl) <- m.blocks if flat(nb.oldLines) != flat(nb.newLines)
    yield nb
//    for
//      AnnotationBl(n,txt,newTxt) <-m.blocks if flat(txt) != flat(newTxt)
//      XmlBl(n,txt,newTxt) <-m.blocks if flat(txt) != flat(newTxt)
//    yield (n,flat(txt),flat(newTxt))

  private def flat(strs:List[String]) = strs.flatMap(_.split('\n').map(_.trim))

  def getPrettyDiff(m:Model): String =
    (for nb <-getDiff(m)
      yield
        val diff = for (l,r) <- nb.oldLines.zipAll(nb.newLines,"","")
                        if l!=r yield (s"| $l",s"| $r")
        val (txt2,newTxt2) = diff.unzip
        s"=== ${nb.name} ===\n${txt2.mkString("\n")}\n|-- becomes --\n${newTxt2.mkString("\n")}")
      .mkString("\n")

  def buildNew(m:Model): String =
      (for b<-m.blocks yield build(b,true)).mkString("\n")

  def buildOld(m:Model): String =
    (for b<-m.blocks yield build(b,false)).mkString("\n")

  private def build(b: Block,isNew: Boolean): String = b match
    case Content(c) => c
    case AnnotationBl(n,_,newTxt) if isNew => s"// @${n}\n${newTxt.mkString("\n")}"
    case AnnotationBl(n,oldTxt,_)          => s"// @${n}\n${oldTxt.mkString("\n")}"
    case XmlBl(n,_,newTxt) if isNew => s"<$n>\n${newTxt.mkString("\n")}\n</$n>"
    case XmlBl(n,oldTxt,_)          => s"<$n>\n${oldTxt.mkString("\n")}\n</$n>"



//  def main(args: Array[String]): Unit =
//    val fileStr = Source.fromFile("uppaal3.xml").getLines().mkString("\n")
//    val model = UppaalParser.parse(fileStr)
//    println(s"===\n$model\n===\n${model.getAnnotations.mkString("\n")}")


