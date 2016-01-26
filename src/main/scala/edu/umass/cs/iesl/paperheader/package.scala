package edu.umass.cs.iesl

import cc.factorie.util.DefaultCmdOptions
import cc.factorie.app.nlp.SharedNLPCmdOptions
/**
 * Created by kate on 1/26/16.
 */
package object paperheader {

  class HeaderTaggerOpts extends DefaultCmdOptions with SharedNLPCmdOptions {
    val taggerType = new CmdOption[String]("tagger-type", "default", "STRING", "tagger type: grobid|default|combined")

    val optimizer = new CmdOption[String]("optimizer", "adagrad", "STRING", "optimization function: adagrad|lbfgs")
    val learningRate = new CmdOption("learning-rate", 0.35548827391837345, "FLOAT", "Adagrad learning rate.")
    val delta = new CmdOption("delta", 1.9033917145173614E-6, "FLOAT", "Adagrad delta (ridge).")
    val l1 = new CmdOption("l1", 0.1, "FLOAT", "l1 regularizer strength")
    val l2 = new CmdOption("l2", 0.1, "FLOAT", "l2 regularizer strength")
    val numIterations = new CmdOption("num-iterations", 5, "INT", "Number of training iterations")

    val trimBelow = new CmdOption[Int]("trim-below", 0, "INT", "trim features occurring fewer than this many times")
    val segmentScheme = new CmdOption[String]("segment-scheme", "BILOU", "STRING", "scheme for representing token segments: BILOU|BIO")

  }

}
