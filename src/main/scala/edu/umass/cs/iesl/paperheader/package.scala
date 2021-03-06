package edu.umass.cs.iesl

import cc.factorie.app.nlp.SharedNLPCmdOptions
import cc.factorie.util.{DefaultCmdOptions, ModelProviderCmdOptions}
/**
 * Created by kate on 1/26/16.
 */
package object paperheader {

  class HeaderTaggerOpts extends DefaultCmdOptions with SharedNLPCmdOptions with ModelProviderCmdOptions {
    val trainFile = new CmdOption("train-file", "", "STRING", "UMass formatted training file.")
    val devFile = new CmdOption("dev-file", "", "STRING", "UMass formatted dev file.")
    val testFile = new CmdOption("test-file", "", "STRING", "UMass formatted test file.")
    val taggerType = new CmdOption[String]("tagger-type", "default", "STRING", "tagger type: grobid|default|combined")

    /*
     * If dataType is set to "iesl", the tagger will expect a tab-separated filed that looks like what is included in
     *    paper-header/data/fullpaper-headers.tsv
     * with five columns: [IOB label] [token] [x-coord] [y-coord] [font size]
     * Otherwise, if dataType is set to "grobid", the tagger will expect a tab-separated file that looks like
     *    paper-header/data/grobid-headers-train.tsv
     * (see edu.umass.cs.iesl.load.Grobid for details)
     */
    val dataType = new CmdOption[String]("data-type", "iesl", "STRING", "dataset id: iesl|grobid")

    val saveModel = new CmdOption[Boolean]("save-model", true, "BOOLEAN", "whether or not to save the model")
    val modelFile = new CmdOption[String]("model-file", "bibie.factorie", "STRING", "file to save model to or load model from")
    val logFile = new CmdOption[String]("log-file", "training.log", "STRING", "where to log messages")
    val lexicons = new LexiconsProviderCmdOption("lexicons")

    /*

    Training hyperparameters

     */

    val optimizer = new CmdOption[String]("optimizer", "adagrad", "STRING", "optimization function: adagrad|lbfgs")
    val learningRate = new CmdOption("learning-rate", 0.35548827391837345, "FLOAT", "Adagrad learning rate.")
    val delta = new CmdOption("delta", 1.9033917145173614E-6, "FLOAT", "Adagrad delta (ridge).")
    val l1 = new CmdOption("l1", 0.1, "FLOAT", "l1 regularizer strength")
    val l2 = new CmdOption("l2", 0.1, "FLOAT", "l2 regularizer strength")
    val numIterations = new CmdOption("num-iterations", 5, "INT", "Number of training iterations")
    val trimBelow = new CmdOption[Int]("trim-below", 0, "INT", "trim features occurring fewer than this many times")
    val segmentScheme = new CmdOption[String]("segment-scheme", "BIO", "STRING", "scheme for representing token segments: BILOU|BIO")

  }

}
