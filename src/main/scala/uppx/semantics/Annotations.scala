package uppx.semantics

import org.apache.fontbox.ttf.TrueTypeFont
import uppx.semantics.Annotations.Annotation


case class Annotations(anns:Map[String,Annotation]):
  override def toString: String =
    anns.map((k,v)=>s"$k: $v").mkString("\n")
  def get(annotation:String) = anns.get(annotation)



object Annotations:
  // a line maps it ID (first element) to the remaining ones. Avoids repeated IDs.
  private type Lines = Map[String,(Int,Line)]
  private type Line = Map[Int,String]
  /** An annotation has a name, pattern, and a table (header and attibutes) */
  sealed case class Annotation(pattern:String,
                               header: List[String],
//                               attrs: List[Map[Int,String]],
                               attrs: Lines):
    /** Add a new line of attributes */
    def +(line: Line): Annotation =
      if line contains 0 then
        Annotation(pattern,header,attrs+(line(0) -> (attrs.size,line)))
      else
        this
    /** Add a new line of attributes */
    def +(line:List[String]):Annotation =
      this+(line.zipWithIndex.map((x,y)=>(y,x)).toMap)
    /** Adds a new header value */
    def addHeader(h:String) = Annotation(pattern,h::header,attrs)
    private lazy val headerMap = header.zipWithIndex.toMap
    /** Generates code from the table, applying the pattern to a given line. */
    def instantiatePattern(line:Line): String =
      val varPattern = """\$([a-zA-Z0-9\-_]+)|\$\{([a-zA-Z0-9\-_]+)\}""".r
      def findAttr(attr:String): Option[String] =
      //println(s"# finding $attr from pattern using ${line.mkString(",")} ")
        for idx <- headerMap.get(attr)
            value <- line.get(idx)
        yield
          value
      varPattern.replaceAllIn(pattern, x =>
        findAttr(if x.group(1)==null then x.group(2) else x.group(1)).getOrElse(""))

    /** Generates code from the table, applying the pattern all lines. */
    def instantiateAll: List[String] =
      attrs.values.toList.sorted.map(x=>instantiatePattern(x._2))
//      for v <- attrs.values.toList.sorted if selected(v._2,feats) yield
//        instantiatePattern(v._2)

//    private def selected(line:Line, feats:Set[String]): Boolean =
//      val lineFeats = for
//        idx <- headerMap.get("Features")
//        cond <- line.get(idx)
//      yield
//        cond.split(',').map(_.trim).toSet
//      lineFeats match
//        case None => true
//        case Some(selFs) =>
//          (selFs-"").forall(feats)
//
//
//    private val featureIdx: Option[Int] =
//      headerMap.get("Features")

    /** Pretty print of the annotations */
    override def toString: String =
      s"'$pattern' [${header.mkString(",")}]\n - ${attrs.values.toList.sorted.map(_._2).mkString("\n - ")}"
