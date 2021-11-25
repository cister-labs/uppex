package uppex.syntax

import uppex.semantics.{Annotations, Configurations}
import uppex.semantics.Uppaal.{AnnotationBl, Block, Content, Model, XmlBl}

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
  private val tag = """[a-zA-Z\-0-9_]+""".r // identifier of tags
  private val newLine = "\\n".r // Line break, with possible spaces


  def parseFile(file: String, conf:Configurations): (Model,String) =
    val fileStr = Source.fromFile(file).getLines().mkString("\n")
    (parse(fileStr,conf),fileStr)

  def parse(code: String, conf: Configurations): Model =
    parseAll(uppaal(using conf), code) match
      case Success(result, _) => result
      case f: NoSuccess => sys.error("Parser2 failed: " + f)

  def uppaal(using conf:Configurations): Parser[Model] =
    repsep(uppaalElem,newLine) ^^ ( lines => Model(lines))

  def uppaalElem(using conf:Configurations): Parser[Block] =
    // Option 1: annotation with body
    "//"~"@"~word~opt(newLine ~> body)^^ {
      case _~_~name~bod =>
        AnnotationBl(name,bod.getOrElse(Nil),
          conf.annotations.get(name).map(_.instantiateAll).getOrElse(Nil))
    } |
    // Option 2 and 3: a tag or anything else
    parseTags(conf.xmlBlocks.anns.keys)
    // Option 2: Anything else
//    line ^^ Content.apply

  private val lineBlock = line ^^ Content.apply

  def parseTags(tags:Iterable[String])(using conf:Configurations): Parser[Block] =
    tags
      .map(parseTag)
      .foldRight[Parser[Block]](lineBlock)(_ | _)

  def parseTag(t:String)(using conf:Configurations): Parser[XmlBl] =
    ("<"~t~">"~opt(newLine))~>bodyTag(t)

  def bodyTag(t:String)(using conf:Configurations): Parser[XmlBl] =
    repsep(notClose(t),newLine) ~ opt(s"\\n( |\\t)*</$t>".r) ^^ {
      case list~_ => // XmlBl(t,list,Nil)
        XmlBl(t,list,
          conf.xmlBlocks.get(t).map(_.instantiateAll).getOrElse(Nil))
    }

  def notClose(t:String): Parser[String] =
    s"(?!</$t>).*".r

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

