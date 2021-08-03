package uppx.syntax

import uppx.semantics.Annotations
import uppx.semantics.Uppaal.{AnnotationBl, Block, Content, Model}

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object UppaalParser extends RegexParsers:

//  def parseFile(file: String, anns:Annotations): (Model,String) =
//    val fileStr = Source.fromFile(file).getLines().mkString("\n")
//    (parse(fileStr,anns),fileStr)
//
//  def parse(code: String, anns: Annotations): Model =
//    parseAll(uppaal(using anns), code) match
//      case Success(result, _) => result
//      case f: NoSuccess => sys.error("Parser failed: " + f)

//  override val skipWhitespace = false
  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t)+".r

//  private val untilAnnot = """.|\n""".r
  private val word = """[^\n]+""".r // non-empty sequence of characters without new lines
  def line: Parser[String] = rep(word) ^^ (_.mkString) // possibly empty line
  def neLine: Parser[String] = rep1(word) ^^ (_.mkString) // non-empty line
//  private val at = """// *@ *""".r // slashes and @, with possible spaces
  private val name = """[a-zA-Z0-9_][a-zA-Z\-0-9_]*""".r // identifier of annotations
  private val newLine = "\\n".r // Line break, with possible spaces


  def parseFile(file: String, anns:Annotations): (Model,String) =
    val fileStr = Source.fromFile(file).getLines().mkString("\n")
    (parse(fileStr,anns),fileStr)
  def parse(code: String, anns: Annotations): Model =
    parseAll(uppaal(using anns), code) match
      case Success(result, _) => result
      case f: NoSuccess => sys.error("Parser2 failed: " + f)

  def uppaal(using anns:Annotations): Parser[Model] =
    repsep(uppaalElem,newLine) ^^ ( lines => Model(lines))

  def uppaalElem(using anns:Annotations): Parser[Block] =
    // Option 1: annotation with body
    "//"~"@"~word~opt(newLine ~> body)^^ {
      case _~_~name~bod =>
        AnnotationBl(name,bod.getOrElse(Nil),
          anns.get(name).map(_.instantiateAll).getOrElse(Nil))
    } |
    // Option 2: Anything else
    line ^^ Content.apply

  def body: Parser[List[String]] =
    repsep(neLine,newLine)


//
//  def uppaal(using anns:Annotations): Parser[Model] =
//    annotation~uppaal ^^ {
//      case ann~rest => Model(ann::rest.blocks)
//    } |
//    (line <~ "\n") ~ uppaal ^^ {
//      case bl~rest => Model(Content(bl)::rest.blocks)
//    } |
//    line ^^ { str => Model(List(Content(str)))}
//
//
//
//  def annotation(using anns:Annotations): Parser[AnnotationBl] =
//    (at ~> name <~ newLine)~annotationCont ^^ {
//      case name~cont => AnnotationBl(name,cont,
//                          anns.get(name).map(_.instantiateAll).getOrElse(Nil))
//    }
//  def annotationCont: Parser[List[String]] =
//    newLine ^^ { _ => Nil} |
//    (line <~ "\n") ~ annotationCont ^^ {
//      case content~rest =>  content :: rest
//    }

