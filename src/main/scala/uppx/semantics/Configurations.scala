package uppx.semantics

import Configurations.Products

case class Configurations(products: Products, annotations: Annotations, xmlBlocks: Annotations):
  def withProduct(p:String): Configurations =
    Configurations(products+(""->products.getOrElse(p,Set())),annotations,xmlBlocks)

object Configurations:
  type Products = Map[String,Set[String]]

