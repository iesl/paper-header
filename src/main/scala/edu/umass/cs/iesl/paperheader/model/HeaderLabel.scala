package edu.umass.cs.iesl.paperheader.model

/**
 * Created by kate on 1/26/16.
 */

import cc.factorie.variable.{CategoricalDomain, LabeledCategoricalVariable}
import cc.factorie.app.nlp.Token

object HeaderLabelDomain extends CategoricalDomain[String]

abstract class HLabel(labelname: String) extends LabeledCategoricalVariable(labelname)
class HeaderLabel(labelname: String, val token: Token) extends HLabel(labelname) {
  def domain = HeaderLabelDomain
}