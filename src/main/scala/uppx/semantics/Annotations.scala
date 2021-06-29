package uppx.semantics

import uppx.semantics.Annotations.Annotation


case class Annotations(anns:Map[String,Annotation]):
  override def toString: String =
    anns.map((k,v)=>s"$k: $v").mkString("\n")
  def get(annotation:String) = anns.get(annotation)



object Annotations:
  /** An annotation has a name, pattern, and a table (header and attibutes) */
  sealed case class Annotation(pattern:String,
                               header: List[String],
                               attrs: List[Map[Int,String]]): // add querries later
    /** Add a new line of attributes */
    def +(line: Map[Int,String]): Annotation =
      Annotation(pattern,header,line::attrs)
    /** Add a new line of attributes */
    def +(line:List[String]):Annotation =
      this+(line.zipWithIndex.map((x,y)=>(y,x)).toMap)
    /** Adds a new header value */
    def addHeader(h:String) = Annotation(pattern,h::header,attrs)
    private lazy val headerMap = header.zipWithIndex.toMap
    /** Generates code from the table, applying the pattern to a given line. */
    def instantiatePattern(line:Map[Int,String]): String =
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
    def instantiateAll: List[String] = attrs.map(instantiatePattern)

    /** Pretty print of the annotations */
    override def toString: String =
      s"'$pattern' [${header.mkString(",")}]\n - ${attrs.mkString("\n - ")}"
