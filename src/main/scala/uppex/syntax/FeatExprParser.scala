package uppex.syntax

import uppex.semantics.Uppaal.*
import uppex.semantics.{Annotations, Configurations}

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object FeatExprParser extends RegexParsers:

//  override val skipWhitespace = false
  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\n)+".r

  ////////////////////

  enum FeatExpr:
    case Feature(id:String)
    case And(f1:FeatExpr,f2:FeatExpr)
    case Or(f1:FeatExpr,f2:FeatExpr)
//    case Imply(f1:FeatExpr,f2:FeatExpr)
    case Not(f:FeatExpr)
    case True

  import FeatExpr._

  def eval(fe:FeatExpr)(using fs:Set[String]): Boolean = fe match
    case Feature(id) => fs contains id
    case And(f1,f2) => eval(f1) && eval(f2)
    case Or(f1,f2) => eval(f1) || eval(f2)
    case Not(f2) => !eval(f2)
    case True => true

  def parse(txt: String): FeatExpr =
    parseAll(featExpr, txt) match
      case Success(result, _) => result
      case f: NoSuccess => sys.error(s"Failed to parse feature expression: '$txt' -- $f")

  ///////////////////

  def featExpr: Parser[FeatExpr] =
    featImpl |
    ("" ^^^ True)

  def featImpl: Parser[FeatExpr] =
    featConj ~ opt(("->"|"=>")~>featImpl) ^^ {
      case f1 ~ Some(f2) => Or(Not(f1), f2)
      case f1 ~ None => f1
    }
  def featConj: Parser[FeatExpr] =
    featDisj ~ opt("&"~>featConj) ^^ {
      case f1 ~ Some(f2) => And(f1, f2)
      case f1 ~ None => f1
    }

  def featDisj: Parser[FeatExpr] =
    literal ~ opt("||"~>featDisj) ^^ {
      case f1 ~ Some(f2) => Or(f1, f2)
      case f1 ~ None => f1
    }
  def literal: Parser[FeatExpr] =
    "("~>featImpl<~")" |
    "!"~>literal ^^ Not.apply |
    "true" ^^^ True |
    "false" ^^^ Not(True) |
    feature

  def feature: Parser[FeatExpr] =
    """[a-zA-Z0-9_][a-zA-Z\-0-9_]*""".r ^^ Feature.apply


