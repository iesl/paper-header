package edu.umass.cs.iesl.paperheader.model

import cc.factorie.app.nlp.Token
import cc.factorie.variable.{CategoricalLabeling, CategoricalVariable, CategoricalDomain}

/**
 * Created by kate on 11/14/15.
 */
object HeaderDomain extends CategoricalDomain[String]

case class HeaderTag(token: Token, initialCategory: String) extends CategoricalVariable(initialCategory) {
  def domain = HeaderDomain
}
class GoldHeaderTag(token: Token, label: String) extends HeaderTag(token, label) with CategoricalLabeling[String]
