package uppex.syntax

import java.io.PrintWriter
import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.util.{Calendar, Date}

class Report(val name:String, val timeout:Int = 30) {
  import Report.Result

  var products: List[(String,List[Result])] = Nil

  def addProduct(n:String): Unit = products ::= (n->Nil)

  def addOk(msg:String): Unit = products match
    case (p,l)::rest => products = ((p,Result.OK(msg)::l)::rest)
    case _ => products = (("",Result.OK(msg)::Nil)::Nil)

  def addFail(msg:String): Unit = products match
    case (p,l)::rest => products = ((p,Result.Fail(msg)::l)::rest)
    case _ => products = (("",Result.Fail(msg)::Nil)::Nil)

  def addTO(missing:List[String]) //missing:Int, prop:String)
       : Unit = products match
    case (p,l)::rest => products = ((p,Result.TO(missing)::l)::rest)
    case _ => products = (("",Result.TO(missing)::Nil)::Nil)

  def writeFile(fileName:String): Unit =
    //println(s"My report: $products")
    val pw = new PrintWriter(fileName)
    pw.write(Report.genHtmlByBoth(this))
    pw.close()
}

object Report {
  enum Result {
    case OK(msg:String)
    case Fail(msg:String)
    case TO(missing:List[String])
  }

  /////////////////////////
  // grouped by products //
  /////////////////////////

  def printProducts(rep:Report): String =
    (for (prod,res)<-rep.products.sortWith(_._1<_._1) yield
      s"<h3>$prod</h3>\n<ul>${printResults(res,rep.timeout)}</ul>").mkString("\n\n")
  def printResults(res:List[Result],timeout:Int): String =
    (for (r<-res.reverse) yield r match {
      case Result.OK(msg) => s"   <li class=\"ok\"> $msg </li>"
      case Result.Fail(msg) => s"   <li class=\"fail\"> $msg </li>"
//      case Result.TO(miss) => s"   <li class=\"timeout\"> Time-out after ${timeout}s. Missing ${miss.size} properties. Failed on property: \"${miss.head}\"</li>"
      case Result.TO(miss) => s"   <li class=\"timeout\"> Time-out after ${timeout}s. Missing: ${miss.map(x=>s"<li>$x</li>").mkString("<ul>","\n","</ul>")}</li>"
    }).mkString("\n")

  /////////////////////////////
  // grouped by requirements //
  /////////////////////////////

  def printReqs(rep:Report): String =
    var reqs = Map[String,List[(String,Result)]]() // requirement -> (prod x result)*
    var timeOuts = List[(String,Result.TO)]() // prod -> timeout
    var miss = Map[String,Set[String]]() // req -> prod*
    var reallyTimeOuts = Set[String]() // set of products that timed out while analysing some product
    for (prod,res) <- rep.products; r<-res do
      r match
        case Result.OK(msg)   => reqs += msg->( (prod->r) :: reqs.getOrElse(msg, Nil ))
        case Result.Fail(msg) if msg.startsWith("(Aborted) ") =>
                                 reqs += msg.drop(10)->( (s"(Aborted) $prod"->r) :: reqs.getOrElse(msg, Nil ))
        case Result.Fail(msg) => reqs += msg->( (prod->r) :: reqs.getOrElse(msg, Nil ))
        case to: Result.TO =>
          timeOuts ::= (prod->to)
          for prop <- to.missing.tail do miss += prop -> (miss.getOrElse(prop,Set())+prod)
          //reqs += to.prop ->( (prod->r) :: reqs.getOrElse(to.prop, Nil ))
    val allReqs:List[(String,List[(String,Result)])] = // req -> (prod x result)*
      reqs.toList.sortWith(_._1<_._1):::
      (for (r,ps)<-miss yield r -> ps.map(_->Result.TO(Nil)).toList).toList
//      (timeOuts.filter(p => !reqs.contains(p._1)).map(kv=>kv._1->List(kv._2)))
    (for (req,res)<-allReqs yield
        val to: List[(String,Result.TO)] = timeOuts.filter(_._2.missing.head==req)
        val unverified: List[String] =
          for (prod,Result.TO(missing)) <- timeOuts if missing.drop(1).contains(req) yield prod
        reallyTimeOuts = reallyTimeOuts ++ to.map(_._1).toSet
        s"<li>$req\n  <ul>${printResultsProd(res,to,unverified,rep.timeout)}</ul></li>").mkString("<ul>","\n\n","</ul>")
//    (for (p,to)<-timeOuts yield
//      s"<li class=\"timeout\">\n  Product $p: missing ${to.missing.size}; Failed on \"${to.missing.head}\"</li>"
//      ).mkString(s"<h3>Timed out after ${rep.timeout}s:</h3><ul>","\n","</ul>") +
//    (for (prop,prods)<-miss yield
//      s"  <li>$prop ${prods.map(p=>s"<li>${mbBold(p,reqs,reallyTimeOuts)}</li>").mkString("<ul>","\n","</ul>")}</li>"
//      ).mkString(s"<h3>Properties not verifed after timeouts (${rep.timeout}s):</h3><ul>","\n","</ul>")

  private def mbBold(prod: String, reqs: Map[String, _], to: Set[String]): String =
    if (reqs contains prod) || (to contains prod) then prod else s"<strong>$prod</strong>"

  private def printResultsProd(res: List[(String, Report.Result)],
                               to:List[(String,Result.TO)],
                               unverified:List[String],
                               total:Int): String =
    // 1. OK and Fails (TO should not occur here)
    (for r<-res.reverse yield r._2 match
      case Result.OK(msg) => s"   <li class=\"ok\"> ${r._1} </li>"
      case Result.Fail(msg) => s"   <li class=\"fail\"> ${r._1} </li>"
      case Result.TO(missing) => "" // s"   <li class=\"timeout\"> Missing ${missing.size-1} other property(ies). </li>"
    ).mkString("\n") +
    // 2. Products that caused the timeout
    (for (prod,Result.TO(miss))<-to yield
        s"<li class=\"timeout\">\n  $prod: timout after ${total}s; missing ${miss.size} requirement(s) for this product</li>").mkString("\n") +
    // 3. Products that should have been checked, but never got the chance
    (for s <- unverified yield
      s"<li class=\"waiting\">\n  $s: Not verified because it timed out while checking another property.</li>").mkString("\n")



  ///////////////////////////
  // generating final HTML //
  ///////////////////////////

  def getHtmlByProd(rep: Report): String = getHtml(rep.name, printProducts(rep))
  def genHtmlByReq(rep:Report): String = getHtml(rep.name,printReqs(rep))
  def genHtmlByBoth(rep:Report):String =
    getHtml(rep.name,
      "<h1> Grouped by Requirement </h1>"+
        printReqs(rep) +
        "<hr><h1> Grouped by Product </h1>"+
        printProducts(rep))

  def getHtml(name: String, core: String): String =
    s"""
       |<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |  <meta charset="utf-8">
       |  <meta http-equiv="X-UA-Compatible" content="IE=edge">
       |  <meta name="viewport" content="width=device-width, initial-scale=1">
       |
       |  <title>Uppex - report</title>
       |
       |  <meta name="description" content="Uppex report">
       |</head>
       |
       |<body>
       |
       |<style type="text/css">
       |  @import url("https://fonts.googleapis.com/css?family=Open+Sans:400italic,700italic,400,700");
       |  html {
       |      font-family: sans-serif;
       |      -ms-text-size-adjust: 100%;
       |      -webkit-text-size-adjust:100%
       |  }
       |  body {
       |      margin:0
       |  }
       |  article, aside, details, figcaption, figure, footer, header, hgroup, main, menu, nav, section, summary {
       |      display:block
       |  }
       |      a {
       |      background-color:transparent
       |  }
       |  a:active, a:hover {
       |      outline:0
       |  }
       |  abbr[title] {
       |      border-bottom:1px dotted
       |  }
       |  b, strong {
       |      font-weight:bold
       |  }
       |  dfn {
       |      font-style:italic
       |  }
       |  h1 {
       |      font-size: 2em;
       |      margin:0.67em 0
       |  }
       |  mark {
       |      background: #ff0;
       |      color:#000
       |  }
       |  small {
       |      font-size:80%
       |  }
       |  table {
       |      border-collapse: collapse;
       |      border-spacing:0
       |  }
       |  td, th {
       |      padding:0
       |  }
       |  h1, .h1 {
       |      font-size:36px;
       |      margin-top: 55px !important;
       |  }
       |  h2, .h2 {
       |      font-size:30px
       |  }
       |  h3, .h3 {
       |      font-size:24px
       |  }
       |  h4, .h4 {
       |      font-size:18px
       |  }
       |  h5, .h5 {
       |      font-size:14px
       |  }
       |  h6, .h6 {
       |      font-size:12px
       |  }
       |  h1, h2, h3, h4, h5, h6, .h1, .h2, .h3, .h4, .h5, .h6 {
       |      font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
       |      font-weight: 500;
       |      line-height: 1.1;
       |      color:#2d2d2d
       |  }
       |  h1 small, h2 small, h3 small, h4 small, h5 small, h6 small, .h1 small, .h2 small, .h3 small, .h4 small, .h5 small, .h6 small, h1 .small,
       |  h2 .small, h3 .small, h4 .small, h5 .small, h6 .small, .h1 .small, .h2 .small, .h3 .small, .h4 .small, .h5 .small, .h6 .small {
       |      font-weight: normal;
       |      line-height: 1;
       |      color:#999999
       |  }
       |  h1, .h1, h2, .h2, h3, .h3 {
       |      margin-top: 20px;
       |      margin-bottom:10px
       |  }
       |  h1 small, .h1 small, h2 small, .h2 small, h3 small, .h3 small, h1 .small, .h1 .small, h2 .small, .h2 .small, h3 .small, .h3 .small {
       |      font-size:65%
       |  }
       |  h4, .h4, h5, .h5, h6, .h6 {
       |      margin-top: 10px;
       |      margin-bottom:10px
       |  }
       |  h4 small, .h4 small, h5 small, .h5 small, h6 small, .h6 small, h4 .small, .h4 .small, h5 .small, .h5 .small, h6 .small, .h6 .small {
       |      font-size:75%
       |  }
       |  p {
       |      margin:0 0 10px
       |  }
       |  h1, h2, h3, h4, h5, h6, .h1, .h2, .h3, .h4, .h5, .h6 {
       |  font-family: "Open Sans","Helvetica Neue",Helvetica,Arial,sans-serif;
       |  font-weight: 500;
       |  line-height: 1.1;
       |  color: #2d2d2d;
       |  }
       |  body {
       |  font-family: "Open Sans","Helvetica Neue",Helvetica,Arial,sans-serif;
       |  font-size: 14px;
       |  line-height: 1.42857143;
       |  color: #666666;
       |  }
       |</style>
       |
       |<style type="text/css">
       |.splash {
       |  position: relative;
       |  background: #418f49!important; /*url(../../extra/img/background.png) no-repeat center;*/
       |  -webkit-background-size: cover;
       |  -moz-background-size: cover;
       |  -o-background-size: cover;
       |  background-size: cover;
       |  width: 100%;
       |  height: 150px;
       |  top:0px;
       |  margin-top: -20px;
       |  text-align: center;
       |}
       |
       |.intro-title {
       |  text-align: center;
       |}
       |
       |.strong {
       |  font-weight: bold;
       |  margin-bottom: 8px;
       |}
       |
       |h5.extraspace {
       |  padding-top: 15px;
       |}
       |p.extraspace {
       |  padding-top: 15px;
       |  padding-bottom: 15px;
       |}
       |
       |#myTabContent {
       |  min-height: 50vh;
       |  margin-bottom: 100px;
       |}
       |
       |li.ok {
       |  list-style-type: '✅';
       |  padding-inline-start: 1ch;
       |}
       |li.fail {
       |  list-style-type: '❌';
       |  padding-inline-start: 1ch;
       |  font-weight: bold;
       |}
       |li.timeout{
       |  list-style-type: '⏱';
       |  padding-inline-start: 1ch;
       |}
       |li.waiting{
       |  /* list-style-type: '⏸'; */
       |  padding-inline-start: 1ch;
       |  color: #b5b5b5;
       |}
       |
       |
       |.well {
       |  border-radius: 15px;
       |}
       |.well>ul {
       |  margin-bottom: 0px;
       |}
       |
       |.publications>li {
       |  margin-left: 0px;
       |}
       |
       |/* Twitter tweaks */
       |.twitter-timeline {
       |  font-family: "Roboto","Helvetica Neue",Helvetica,Arial,sans-serif;
       |/*   margin-top: 15px; */
       |}
       |h4.pulldown {
       |  padding-top: 00px;
       |}
       |
       |/* fitting the main text properly */
       |.row {
       |  padding-left: 15px;
       |  padding-right: 15px;
       |}
       |
       |.body {
       |  color: #5c5c5c;
       |  font-size: 14px;
       |}
       |
       |#myTabContent {
       |  position:relative;
       |  top:40px;
       |/*   max-width:750px; */
       |}
       |
       |.tab-pane {
       |  margin-right: auto;
       |  margin-left: auto;
       |
       |  max-width:750px;
       |}
       |#home {
       |  max-width:950px;
       |}
       |
       |.panel-footer {
       |  margin-top: 45px;
       |  text-align: center;
       |}
       |/*html {*/
       |  /*background-color: #f5f5f5;*/
       |/*}*/
       |
       |/* fixing program looks */
       |.panel-heading>.container-fluid {
       |  padding-left: 0px;
       |}
       |
       |a {
       |  color: #198b3b;
       |}
       |a:hover,a:focus {
       |  color: #105425;
       |}
       |
       |
       |h5 {
       |  font-size: 18px;
       |}
       |
       |
       |/* Getting the title image right */
       |.blue-rect {
       |  background-color: rgba(19, 63,75, 0.4);
       |  padding-top: 1px;
       |  padding-bottom: 1px;
       |}
       |
       |.splash-title {
       |  font-weight: bold;
       |  text-align: right;
       |  color: white;
       |  margin-right: 25px;
       |  padding-top: 10px;
       |}
       |
       |.splash-subtitle {
       |  font-weight: bold;
       |  text-align: left;
       |  color: white;
       |  margin-left: 25px;
       |  font-size:10px;
       |  margin-top: 8px;
       |  margin-bottom: 8px;
       |}
       |
       |.img-text-text {
       |  float: left;
       |  width: 75%;
       |  padding: 0 10px 10px 10px;
       |}
       |
       |.img-text-img {
       |  float: right;
       |  width: 40%;
       |  padding: 0px 10px 10px 0px;
       |}
       |.img-text-text {
       |  float: left;
       |  min-width: 75%;
       |  max-width: 75%;
       |}
       |
       |@media (min-width: 350px) {
       |  .splash-title {
       |    margin-right: 25px;
       |  }
       |  .splash-subtitle {
       |    margin-left: 25px;
       |    font-size:16px;
       |  }
       |}
       |
       |@media (min-width: 580px) {
       |  .splash-title {
       |    margin-right: 40px;
       |  }
       |  .splash-subtitle {
       |    margin-left: 40px;
       |    font-size:20px;
       |  }
       |}
       |
       |@media (min-width: 768px) {
       |  .splash-title {
       |    margin-right: 50px;
       |  }
       |  .splash-subtitle {
       |    margin-left: 50px;
       |    font-size:24px;
       |  }
       |  #arcalogo {
       |    max-width:70px;
       |  }
       |}
       |</style>
       |
       |<div class="splash">
       |  <h2 class="splash-title">
       |    Uppex
       |  </h2>
       |  <div class="blue-rect">
       |  <h4 class="splash-subtitle">Verification Report: $name</h4>
       |  <h4 class="splash-subtitle"> ${
      //    DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss").format(Calendar.getInstance().getTime)
      //    Calendar.getInstance().formatted("yyyy/MM/dd HH:mm:ss")
      new SimpleDateFormat("yyyy/MM/dd HH:mm:ss").format(Calendar.getInstance().getTime)
    }</h4>
       |  </div>
       |</div>
       |
       |
       |<div id="myTabContent" class="container">
       |<div class="row">
       |
       |
       |<div class="tab-pane fade active in" id="events">
       |
       |$core
       |
       |</div>
       |</div>
       |
       |</body>
       |</html>
       |""".stripMargin
}
