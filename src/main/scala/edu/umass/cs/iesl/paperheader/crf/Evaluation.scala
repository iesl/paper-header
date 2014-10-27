package edu.umass.cs.iesl.paperheader.crf

/**
 * Created by kate on 9/30/14.
 */

import cc.factorie.app.nlp._
import cc.factorie.variable.CategoricalDomain
import scala.collection.mutable.{HashMap, ListBuffer}

object Eval {

  val predictedCorrect = new HashMap[String, Int]() //"A" -- true positives
  val predicted = new HashMap[String, Int]()
  val gold = new HashMap[String, Int]()

  var overallF1 = 0.0

  def apply(domain:CategoricalDomain[String], tokenLabels:Seq[LabeledBioHeaderTag]): Double = {
    predictedCorrect.clear()
    predicted.clear()
    gold.clear()
    overallF1 = 0.0
    val overallWordAcc = tokenLabels.map(t => if (t.valueIsTarget) 1 else 0).sum.toDouble / tokenLabels.length

    domain.categories.foreach(c => {
      val base = if (c.length > 1) c.substring(2) else "O"
      if (base != "O") {
        predicted(base) = 0; gold(base) = 0; predictedCorrect(base) = 0
      }
    })

    tokenLabels.foreach(label => {
      val base = if (label.categoryValue.length > 1) label.categoryValue.substring(2) else "O"
      val targetBase = if (label.target.categoryValue.length > 1) label.target.categoryValue.substring(2) else "O"
      if (base != "O") {
        predicted(base) += 1
        if (label.valueIsTarget) predictedCorrect(base) += 1
      }
      gold(targetBase) += 1
    })

    val f1s = new ListBuffer[Double]()
    gold.keys.foreach(k => {
      val prec = if (gold(k) == 0) 1.0 else predictedCorrect(k).toDouble / gold(k)
      val rec = if (predicted(k) == 0) 1.0 else predictedCorrect(k).toDouble / predicted(k)
      val f1 = if (prec + rec == 0.0) 0.0 else 2.0 * prec * rec / (prec + rec)
      println(s"$k\tf1=$f1 (p=$prec r=$rec)")
      f1s += f1
    })
    var acc = 0.0
    f1s.foreach(acc += _)
    overallF1 = acc / f1s.length

    println(s"OVERALL AVG'ED F1 = $overallF1")
    println(s"OVERALL\tacc=$overallWordAcc")


    overallF1
  }

}

