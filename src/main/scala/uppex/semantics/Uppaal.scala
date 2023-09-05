package uppex.semantics

import scala.io.Source

object Uppaal:
  sealed trait Block

  case class Content(c:String) extends Block
  sealed abstract class NamedBl(val name:String,val oldLines:Lines,val newLines:Lines) extends Block:
    def prettyName: String = this match
      case AnnotationBl(n,_,_) => s"@$n"
      case XmlElm(n,_,_) => s"<$n>"

  case class AnnotationBl(n:String, oldL:Lines, newL:Lines) extends NamedBl(n,oldL,newL)
  case class XmlElm(n:String, oldL:Lines, newL:Lines)       extends NamedBl(n,oldL,newL)

  /**
   * A Uppaal model is seen as a sequence of either
   *  - a sequence of unstructured lines
   *  - a modified annotation block: a sequence of lines with an annotation name, ending with a empty line
   *  - a modified XML element: a sequence of lines inside an XML element with a given tag name.
   * @param blocks
   */
  case class Model(blocks:List[Block]):
    override def toString: String =
      (for b<-blocks yield s" | $b").mkString("\n")


  //////////// Auxiliar functions ////////////

  private type Lines = List[String]

  /** Collect set of named blocks (annotation blocks or XML elements) that are modified, after trimming every line. */
  def getDiff(m:Model): List[NamedBl] =
    for
      (nb:NamedBl) <- m.blocks if flat(nb.oldLines) != flat(nb.newLines)
    yield nb
//    for
//      AnnotationBl(n,txt,newTxt) <-m.blocks if flat(txt) != flat(newTxt)
//      XmlBl(n,txt,newTxt) <-m.blocks if flat(txt) != flat(newTxt)
//    yield (n,flat(txt),flat(newTxt))

  private def flat(strs:List[String]) = strs.flatMap(_.split('\n').map(_.trim))


//  def getPrettyDiffOld(m:Model): String =
//    (for nb <-getDiff(m)
//      yield
//        val diff = for (l,r) <- nb.oldLines.zipAll(nb.newLines,"","")
//                       if l!=r
//                   yield (s"| $l",s"| $r")
//        val (txt2,newTxt2) = diff.unzip
//        s"=== ${nb.name} ===\n${txt2.mkString("\n")}\n|-- becomes --\n${newTxt2.mkString("\n")}")
//      .mkString("\n")

  def getPrettyDiff(m:Model): String =
    getDiff(m).map(nb =>
      val (a,b) = getPrettyDiff(nb)
      s"=== ${nb.prettyName} (overview) ===\n$a\n|-- becomes --\n$b"
    ).mkString("\n")

  def getPrettyDiff(nb:NamedBl): (String,String) =
    val shared = flat(nb.oldLines).toSet intersect flat(nb.newLines).toSet
    val (oldShared,oldDiff) = flat(nb.oldLines).zipWithIndex.partition(x => shared contains x._1)
    val (newShared,newDiff) = flat(nb.newLines).zipWithIndex.partition(x => shared contains x._1)
//    println(s"### DEBUG:\nshared: ${shared.mkString("--\n")}\nold: ${oldDiff.mkString("\n--")}\nnew: ${newDiff.mkString("\n--")}")
//    println(s"### DEBUG:\nnoldShared: ${oldShared.mkString("\n--")}\nnewShared: ${newShared.mkString("\n--")}")
    val mx = oldDiff.lastOption.getOrElse(("",0))._2 max
             newDiff.lastOption.getOrElse(("",0))._2
//    if oldShared.map(_._1)==newShared.map(_._1) // if shared lines are in the same order then discard them
//    then
    getPretty(oldDiff,mx).mkString("\n") ->
      getPretty(newDiff,mx).mkString("\n")
//    else nb.oldLines.flatMap(_.split('\n')).map("| "+_).mkString("\n") ->
//         nb.newLines.flatMap(_.split('\n')).map("| "+_).mkString("\n")

//  /** Previous version of `getPretty`, which added "(...)" for
//   *  every (group of) line(s) that were shared (before and after).  */
//  private def getPrettyOld(lst:List[(String,Int)],max:Int,last:Int=(-1)): List[String] =
//    lst match
//      case Nil => if max==last then Nil else List("| (...)")
//      case (hd,i)::tl if i==last+1 => // continuing a normal line
//        s"| $hd" :: getPretty(tl,i)
//      case (hd,i)::tl => // started a new block
//        "| (...)" :: s"| $hd" :: getPretty(tl,i)

  /** Prints a string with a block of lines, numbered by their index (starting in 0 until max) */
  private def getPretty(lst:List[(String,Int)],max:Int): List[String] =
    if lst.isEmpty then List("| ") else
       lst.map(x => s"| ${x._1}")

  /** Builds the text of a new Uppaal file, using the updated version. */
  def buildNew(m:Model): String =
    (for b<-m.blocks yield build(b,true)).mkString("\n")

  /** Builds the text of a new Uppaal file, using the original version. */
  def buildOld(m:Model): String =
    (for b<-m.blocks yield build(b,false)).mkString("\n")

  private def build(b: Block,isNew: Boolean): String = b match
    case Content(c) => c
    case AnnotationBl(n,_,newTxt) if isNew => s"// @${n}\n${newTxt.mkString("\n")}"
    case AnnotationBl(n,oldTxt,_)          => s"// @${n}\n${oldTxt.mkString("\n")}"
    case XmlElm(n,_,newTxt) if isNew => s"<$n>\n${newTxt.mkString("\n")}\n</$n>"
    case XmlElm(n,oldTxt,_)          => s"<$n>\n${oldTxt.mkString("\n")}\n</$n>"



//  def main(args: Array[String]): Unit =
//    val fileStr = Source.fromFile("uppaal3.xml").getLines().mkString("\n")
//    val model = UppaalParser.parse(fileStr)
//    println(s"===\n$model\n===\n${model.getAnnotations.mkString("\n")}")


