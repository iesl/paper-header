package edu.umass.cs.iesl.paperheader.tagger

/**
 * Created by kate on 10/28/15.
 */

import cc.factorie.variable._
import cc.factorie.app.nlp.Token
import cc.factorie.app.nlp.ner.BIO

object HeaderDomain extends CategoricalDomain[String] {
  this ++= Vector(
    "keyword",
    "author",
    "abstract",
    "title",
    "affiliation",
    "address",
    "date",
    "email"
  )
  freeze()
}

object BIOHeaderDomain extends CategoricalDomain[String] with BIO {
  this ++= encodedTags(HeaderDomain.categories)
  freeze()
}

class BIOHeaderTag(val token: Token, label: String) extends CategoricalVariable(label) {
  def domain = BIOHeaderDomain
}

class LabeledBIOHeaderTag(t: Token, l: String) extends BIOHeaderTag(t,l) with CategoricalLabeling[String]