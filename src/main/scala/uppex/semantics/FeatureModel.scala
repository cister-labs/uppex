package uppex.semantics

import uppex.semantics.Configurations.FProd
import uppex.syntax.FeatExprParser
import uppex.syntax.FeatExprParser.FeatExpr

/*
Example:
Poling	Heartbeats
  SyncMon
  SelfTest	StartWithSelfTest
Scenario	Scn1
  Scn2
  Scn3
#alternative Scenario.*
#optional Poling.*
#or Heartbeats, SyncMon
#optional StartWithSelfTest
#constraint SelfTest <= 200
#constraint Heartbeats <= SyncMon

Model is a (f-group)+ and (constraint)*
f-group is a cardinality and (feature)*
feature is a name
constraint is a boolean (FeatExpr) over literals or equations
equation is a (in)equality of expressions
expression is a value, feature, or a function (sum, avg, len, +,-,*,/, floor, ceil)
  (integers only: sum,avg,+,-,*,/)
*/


object FeatureModel:

//  case class Loc[A](loc:(Int,Int),elem:A)
  type Loc = (Int,Int)
  def show(l:Loc) = s"${(l._1+65).toChar}${l._2+1}" // maybe all-1 (if starting in 1)

  /** An FM table is a unprocessed feature model. */
  case class FMTable(root:FeatureTable,cs:List[Constraint],card:List[(Card,String,Loc)])
  enum Card:
    case Or
    case Alt
    case Opt
    case Mand
    case Range(from:Int,to:Int)
    case From(from:Int)
  def show(c:Card):String = c match
    case Card.Or => "or"
    case Card.Alt => "alternative"
    case Card.Opt => "optional"
    case Card.Mand => "mandatory"
    case Card.Range(f,t) => s"[$f..$t]"
    case Card.From(f) => s"[$f..*]"

//  case class FeatureTable(name:String,children:List[FeatureTable],value:Any,loc:Loc):
//    def toFeature:Feature = Feature(name,List(Group(Card.Mand,children.map(_.toFeature))),value,false,loc)

  case class FeatureTable(fs:Set[FeatureCell]):
    def toFeature: Feature =
      //println(s"producing Feature from cells: '${fs.mkString("\n")}'")
      val roots = for f<-fs if f.up.isEmpty yield f->Set[FeatureCell]()
      if roots.size!=1 then
        sys.error(s"Exactly one root must exist. Found roots: {${roots.map(_._1).mkString(",")}}")
      val root = roots.head._1

      var rev:Map[String,Set[FeatureCell]] = roots.map(kv=>kv._1.name->kv._2).toMap
      for f <- fs; up<-f.up do
        rev += (up.name->(rev.getOrElse(up.name,Set())+f))
      //println(s"Got reversed: ${rev.mkString("\n")}")

      def complete(f: FeatureCell): Feature =
        //println(s"--- completting: $f")
        val children = for ch<-rev.getOrElse(f.name,Set()) yield complete(ch)
        //println(s"--- got children: ${children.mkString("\n")}\n---")
        Feature(f.name,List(Group(Card.Mand,children)),false,f.loc)
      complete(root)


//      val res = addToFT(fs.headOption.getOrElse(sys.error("No feature was found.")),fs)
//      res
//    private def addToFT(cell: FeatureCell,rest:Set[FeatureCell]):
  case class FeatureCell(name: String, up: Option[FeatureCell], loc: Loc):
    def toFeature: Feature = Feature(name, Nil, false, loc)
    override def toString:String =
      s"$name[${up.map(_.name).getOrElse("-")}@${show(loc)}]"

  case class Group(card:Card,lst:Set[Feature]):
    override def toString: String =
      if lst.isEmpty then "" else
        s"${show(card)}\n${ind(lst.mkString("\n"))}"

  /** Core class: a feature is also a node in our diagram. */
  case class Feature(name:String,gs:List[Group],isAbstract:Boolean,loc:Loc):
    lazy val features: Set[String] = (for
       g <- gs
       next <- g.lst
       more <- next.features
     yield more).toSet + name
    override def toString =
      s"$name ${if isAbstract then s"{abstract} " else s" @ ${show(loc)}"}${
        val gss = gs.mkString("\n"); if gss.nonEmpty then "\n"+ind(gss) else ""}"

  type Constraint = (FeatExpr,Loc) // temporary

  ///
  case class FM(root:Feature, cs:List[Constraint]):
    /** Gets the (possible) parent of a feature. */
    def parent(feat:String): Option[String] =
      parentsMap.getOrElse(feat,None)
    def allParents(feat:String): Set[String] =
      parent(feat).toSet.flatMap(allParents) + feat
    lazy private val parentsMap: Map[String,Option[String]] =
      addParents(root,None,Map())
    override def toString: String =
      s"$root\n${cs.map(x=>s"#constraint ${FeatExprParser.show(x._1)}").mkString("\n")}"

  private def addParents(f:Feature, prev:Option[String], ps:Map[String,Option[String]])
      : Map[String,Option[String]] =
    (ps + (f.name -> prev)) ++ (for
       g <- f.gs
       next <- g.lst
       more <- addParents(next,Some(f.name),Map())
      yield more)

  /** Preprocess an FMTable by applying cardinality restrictions and checking its consistency (latter not yet) */
  def preprocess(fm:FMTable): FM =
    var res = fm.root.toFeature
    val feats = fm.root.fs.map(_.name)
    for (c,patt,loc) <- fm.card // a cardinality restriction
    do applyCard(res,c,loc,expandGrPatterns(patt,fm.root)) match
      case None => sys.error(s"Failed to add group '$patt' @ ${show(loc)} - maybe they are not siblings or they are misspelled.")
      case Some(f) => res = f // update result with the previous root with the new groups addapted
    for (c,loc)<-fm.cs; f<-FeatExprParser.vars(c) do
      if !feats(f) then sys.error(s"Unknown feature $f @${show(loc)}")
    FM(res,fm.cs)

  def emptyFM: FM = FM(Feature("",Nil,false,(0,0)),Nil)

  /** Adds a new group cardinality for a set of (sibling) features */
  private def applyCard(f:Feature, c:Card, loc:Loc, g:Set[String]): Option[Feature] =
    f.gs.find(g2 => g.subsetOf(g2.lst.toSet.map(_.name))) match
      case Some(s2) =>
        //println(s"$g is a subset of $s2")
        val newg = Group(c,s2.lst.filter(f => g(f.name)))
        val restg = Group(s2.card,s2.lst.filterNot(f => g(f.name)))
        val restgs = f.gs.filterNot(_==s2)
        val newgs = if restg.lst.isEmpty then newg::restgs else newg::restg::restgs
        Some(Feature(f.name,newgs,f.isAbstract,f.loc))
      case None =>
        //println(s"no subset of $g found in ${f.gs.mkString(" / ")}")
        var done = false
        val newgs = f.gs.map(g2 => Group(g2.card,g2.lst.map(f2=> applyCard(f2,c,loc,g) match
          case Some(f3) => done = true; f3
          case None => f2 )))
        if !done then None //sys.error(s"failed to add group {${g.mkString(",")}} @ ${loc}")
        else Some(Feature(f.name,newgs,f.isAbstract,f.loc))

  /** Expands a list of patterns into a set of feature names. */
  def expandGrPatterns(g:String, f:FeatureTable):Set[String] =
    g.split(" *, *").toSet
      .flatMap(expandGrPattern(_,f))

  private def expandGrPattern(p:String, f:FeatureTable):Set[String] =
    p.split('.') match
      case Array(lit) => Set(lit)
      case Array(top,"*") => //findChildren(top,f)
        for tf <- findFeature(top,f.toFeature).toSet
            g  <- tf.gs
            c  <- g.lst
        yield c.name

  private def findFeature(name: String, root: Feature): Option[Feature] =
    if name == root.name
    then Some(root)
    else root.gs.flatMap(g => g.lst.flatMap(f2 => findFeature(name,f2))).headOption

  private def ind(s:String) = "  "+s.replaceAll("\n","\n  ")

 /////

  /** Checks the validity of a set of products for a given feature model. */
  def validate(prods:Configurations.Products,fm:FM): List[String] = // list of errors
    val allFeats = fm.root.features
    val unk = (for (_,fs)<-prods.toList; (f,_)<-fs  yield
      if !allFeats(f) then
        List(s"Feature unknown: '$f'; known features: {${allFeats.mkString(",")}}")
      else List()).flatten
    if unk.nonEmpty then return unk

    (for (name,prod)<-prods.toList yield
      validateProd(prod.flatMap(fv => fm.allParents(fv._1).map(f2=>f2->fv._2))+
        (fm.root.name->prod.getOrElse(fm.root.name,"")), fm)
        .map(s=>s"Product '$name' is invalid. - $s"))
      .flatten

  /** Given a core product, extends it with parents and checks if
   * the extended product is valid for a given feature model. */
  def validateProd(p:FProd, fm:FM): List[String] =
    validateProd(p.keySet,fm.root) :::
      fm.cs.flatMap(c =>
        if !FeatExprParser.eval(c._1)(using p)
        then List(s"Failed constraint ${c._1} @ ${show(c._2)}; selection {${p.keySet.mkString(",")}}")
        else Nil)

  /** Checks if a product is valid for a given feature. */
  def validateProd(p:Set[String], root:Feature): List[String] =
    if !p(root.name) then
      List(s"Feature '${root.name}' should have been selected; selection: {${p.mkString(",")}}.")
    else
      root.gs.flatMap(g=>validateProd(p,g))

  /** Checks if a product is valid for a given group. */
  def validateProd(p:Set[String], g:Group): List[String] = g.card match
    case Card.Mand => // all should be
      g.lst.toList.flatMap(f=>validateProd(p,f))
    case Card.Alt => // exactly one should be
      val res = g.lst.map(f=>(f->p(f.name))).filter(_._2)
      if res.size!=1 then List(s"Exactly one feature in {${
        g.lst.map(_.name).mkString(",")}} should be selected, but found {${res.map(_._1.name).mkString(",")}}")
      else validateProd(p,res.head._1)
    case Card.Opt =>
      val res = g.lst.map(f=>(f->p(f.name))).filter(_._2)
      res.toList.flatMap(f=>validateProd(p,f._1))
    case Card.Or =>
      val res = g.lst.map(f => (f -> p(f.name))).filter(_._2)
      if res.isEmpty then
        List(s"Some feature in {${
          g.lst.map(_.name).mkString(",")}} should be selected, but none was found.")
      else res.toList.flatMap(f => validateProd(p, f._1))
    case Card.Range(i,j) =>
      val res = g.lst.map(f => (f -> p(f.name))).filter(_._2)
      if res.size<i || res.size>j then
        List(s"Expected $i..$j features in {${
          g.lst.map(_.name).mkString(",")}}, but found {${res.map(_._1.name).mkString(",")}}")
      else res.toList.flatMap(f => validateProd(p, f._1))
    case Card.From(i) =>
      val res = g.lst.map(f => (f -> p(f.name))).filter(_._2)
      if res.size < i then
        List(s"Expected $i..* features in {${
          g.lst.map(_.name).mkString(",")
        }}, but found {${res.map(_._1.name).mkString(",")}}")
      else res.toList.flatMap(f => validateProd(p, f._1))





  //// experiments

  import scala.language.implicitConversions
  implicit def str2feat(s: String): FeatureCell = FeatureCell(s,None,(9,9))
  implicit def str2feattb(s: String): FeatureTable = FeatureTable(Set(str2feat(s)))

//  def mkFt(n:String, ft:FeatureTable): FeatureTable = mkFt(n,ft.fs)
  def mkFt(n: String, ch: Set[FeatureTable]): FeatureTable =
    val rest = for t <- ch; c<-t.fs yield
      FeatureCell(c.name, Some(c.up.getOrElse(str2feat(n))), c.loc)
    FeatureTable(rest+str2feat(n))

  val ex = mkFt("a", Set("b",mkFt("c",Set("d","e","f"))))

//  val ex = mkFt("a",List(
//    "b",mkFt("c",List(
//      "d","e","f"))))
  val fmt = (patt:String) => preprocess(FMTable(ex, Nil, List((Card.Or, patt, (2,3)))))

/**
   * Selects abstract features that have concrete selected features
 *
 * @param fm
   * @return
   */
//  def expand(f:Feature, sel:Set[String]): FM =
//    val
//      f2<-f.features
//
//
//    def collect(f2:Feature, prod:Set[String]): Set[String] =
//      val s = if sel(f.name) then Set(f.name) else Set()
//      val rest = for
//         g <- f.gs
//         next <- f.lst
//        yield
//         collect(next,prod

