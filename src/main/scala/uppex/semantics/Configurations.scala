package uppex.semantics

import Configurations.Products

case class Configurations(products: Products, annotations: Annotations, xmlBlocks: Annotations, featModel: Option[FeatureModel.FM]):
  def withProduct(p:String): Configurations =
    Configurations(products+(""->products.getOrElse(p,Map())),annotations,xmlBlocks,featModel)

object Configurations:
  type FProd = Map[String,Any] // set of features (names) with a value
  type Products = Map[String,FProd]

