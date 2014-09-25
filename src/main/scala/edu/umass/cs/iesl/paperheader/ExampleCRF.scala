package edu.umass.cs.iesl.paperheader

/**
 * Created by kate on 9/25/14.
 */

import cc.factorie._
import cc.factorie.variable._
import cc.factorie.model._
import cc.factorie.la
import cc.factorie.optimize._

object ExampleCRF {

  object TokenDomain extends CategoricalDomain[String]
  class Token(s:String) extends CategoricalVariable(s){ def domain = TokenDomain }
  object LabelDomain extends CategoricalDomain[String]
  class Label(s:String, val token:Token) extends CategoricalVariable(s) with CategoricalLabeling[String] { def domain = LabelDomain }
  class LabelSeq extends scala.collection.mutable.ArrayBuffer[Label]

  val data = List("See/V Spot/N run/V", "Spot/N is/V a/DET big/J dog/N")
  val labelSeqs = for (sentence <- data) yield new LabelSeq ++= sentence.split(" ").map(s => {
    val a = s.split("/")
    new Label(a(1), new Token(a(0)))
  })

  val model = new Model with Parameters {
    val markov = new DotFamilyWithStatistics2[Label, Label] {
      val weights = Weights(new la.DenseTensor2(LabelDomain.size, LabelDomain.size))
    }
    val observ = new DotFamilyWithStatistics2[Label, Token] {
      val weights = Weights(new la.DenseTensor2(LabelDomain.size, TokenDomain.size))
    }
    def factors(labels:Iterable[Var]) = labels match {
      case labels:LabelSeq => labels.map(label => new observ.Factor(label, label.token)) ++ labels.sliding(2).map(window => new markov.Factor(window.head, window.last))
    }
  }

//  val trainer = new


}
