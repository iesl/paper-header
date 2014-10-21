package edu.umass.cs.iesl.paperheader

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
    val overallWordAcc = tokenLabels.map(t => if(t.valueIsTarget) 1 else 0).sum.toDouble / tokenLabels.length

    domain.categories.foreach(c => {
      val base = if (c.length > 1) c.substring(2) else "O"
      if (base != "O"){ predicted(base) = 0; gold(base) = 0; predictedCorrect(base) = 0 }
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
      val f1 = if (prec + rec == 0.0) 0.0 else 2.0*prec*rec / (prec+rec)
      println(s"$k\tf1=$f1 (p=$prec r=$rec)")
      f1s += f1
    })
    var acc = 0.0
    f1s.foreach(acc += _)
    overallF1 = acc / f1s.length

    println(s"OVERALL AVG'ED F1 = $overallF1")
    println(s"OVERALL\tacc=$overallWordAcc")


    overallF1

//    println("MY EVAL:")
//    domain.categories.filter(cat => cat.length > 1).map(_.substring(2)).foreach(cat => correctGuessTable(cat) = 0) //init table
//    domain.categories.filter(cat => cat.length > 1).map(_.substring(2)).foreach(cat => allGuessTable(cat) = 0) //init table
//
//    tokenLabels.foreach(label => {
//      goldTotal += 1
//      val baseLabel = if (label.categoryValue.length > 1) label.categoryValue.substring(2) else "O"
//      if (label.valueIsTarget) {
//        guessCorrect += 1
//        if (baseLabel.length > 1) correctGuessTable(baseLabel) += 1
//      }
//      if (baseLabel.length > 1) allGuessTable(baseLabel) += 1
//      guessTotal += 1
//    })
//
//    domain.categories.filter(cat => cat.length > 1).map(_.substring(2)).foreach(cat => answerTable(cat) = 0) //init table
//    tokenLabels.foreach(label => {
//      if (label.target.categoryValue.length > 1) answerTable(label.target.categoryValue.substring(2)) += 1
//    })
//
//
//    println(s"OVERALL : f1=$of1 acc=$oacc")// (p=$op r=$or")
//    answerTable.keys.foreach(label => {
//      println(s"$label : f1=${f1(label)} acc=${acc(label)} (p=${precision(label)} r=${recall(label)} correct=${correctGuessTable(label)} gold=${answerTable(label)})")
//    })
//
//    of1
  }
//
//  def recall(label:String): Double = if (allGuessTable(label) == 0) 0.0 else correctGuessTable(label).toDouble/allGuessTable(label)
//  def precision(label:String): Double = if (answerTable(label) == 0) 0.0 else correctGuessTable(label).toDouble/answerTable(label)
//  def f1(label:String): Double = {
//    val p = precision(label)
//    val r = recall(label)
//    if (p + r == 0) 0.0 else (2.0 * p * r)/(p + r)
//  }
//
//  def acc(label:String): Double = if (answerTable(label) == 0) 0.0 else correctGuessTable(label).toDouble/answerTable(label)
//
//  def of1: Double = {
//    val or = guessCorrect.toDouble / guessTotal
//    val op = guessCorrect.toDouble / goldTotal
//    if (or + op == 0) 0.0 else (2.0 * op * or)/(op + or)
//  }
//
//  def oacc: Double = guessCorrect.toDouble / goldTotal

}

