package uppx.syntax

import uppx.semantics.Annotations
import uppx.semantics.Uppaal.{AnnotationBl, Content, Model}

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object UppaalParser extends RegexParsers:

  def parseFile(file: String, anns:Annotations): Model =
    val fileStr = Source.fromFile(file).getLines().mkString("\n")
    parse(fileStr,anns)

  def parse(code: String, anns: Annotations): Model =
    parseAll(uppaal(using anns), code) match
      case Success(result, _) => result
      case f: NoSuccess => sys.error("Parser failed: " + f)

  override val skipWhitespace = false

  private val line = """[^\n]*""".r

  def uppaal(using anns:Annotations): Parser[Model] =
    annotation~uppaal ^^ {
      case ann~rest => Model(ann::rest.blocks)
    } |
    (line <~ "\n") ~ uppaal ^^ {
      case bl~rest => Model(Content(bl)::rest.blocks)
    } |
    line ^^ { str => Model(List(Content(str)))}

  private val at = """// *@ *""".r
  private val name = """[a-zA-Z][a-zA-Z\-0-9]*""".r
  private val newLine = " *\\n".r

  def annotation(using anns:Annotations): Parser[AnnotationBl] =
    (at ~> name <~ newLine)~annotationCont ^^ {
      case name~cont => AnnotationBl(name,cont,
                          anns.get(name).map(_.instantiateAll).getOrElse(Nil))
    }
  def annotationCont: Parser[List[String]] =
    newLine ^^ { _ => Nil} |
    (line <~ "\n") ~ annotationCont ^^ {
      case content~rest =>  content :: rest
    }

