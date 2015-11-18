package edu.umass.cs.iesl

import cc.factorie.app.nlp.SharedNLPCmdOptions

/**
 * Created by kate on 11/14/15.
 */
package object paperheader {

  final val TAGGER_TYPE_DEFAULT = "default"
  final val TAGGER_TYPE_GROBID = "grobid"
  final val TAGGER_TYPE_COMBINED = "combined"

  class HeaderTaggerOpts extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions {
    /* data */
    val trainFile = new CmdOption("train-file", "", "STRING", "Filename(s) from which to read training data")
    val trainDir = new CmdOption("train-dir", "", "STRING", "directory of train files")
    val devFile = new CmdOption("dev-file", "", "STRING", "filename of dev set")
    val devDir = new CmdOption("dev-dir", "", "STRING", "directory of dev files")
    val testFile = new CmdOption("test-file", "", "STRING", "Filename(s) from which to read test data")
    val testDir = new CmdOption("test-dir", "", "STRING", "directory of test files")
    val dataSet = new CmdOption("data-set", "", "STRING", "which data set to use (grobid, fullpaper)")

    /* hyperparameters */
    val l1 = new CmdOption("l1", 1e-5, "FLOAT", "L1 regularizer for AdaGradRDA training.")
    val l2 = new CmdOption("l2", 1e-5, "FLOAT", "L2 regularizer for AdaGradRDA training.")
    val learningRate = new CmdOption("learning-rate", 0.1, "FLOAT", "base learning rate")
    val delta = new CmdOption("delta", 0.1, "FLOAT", "learning rate delta")
    val numIterations = new CmdOption("num-iterations", 5, "INT", "Number of training iterations")
    val optimizer = new CmdOption[String]("optimizer", "adagrad", "STRING", "adagrad|lbfgs")

    /* stacked model params */
    val embeddingDim = new CmdOption("embedding-dim", 100, "INT", "embedding dim")
    val scale = new CmdOption("scale", 10.0, "FLOAT", "scale")
    val useOffsetEmbedding = new CmdOption[Boolean]("use-offset-embedding", true, "BOOLEAN", "use offset embedding")

    /* serialization */
    val saveModel = new CmdOption("save-model", true, "BOOLEAN", "serialize the model?")
    val modelFile = new CmdOption("model-file", "HeaderTagger.factorie", "STRING", "filename of serialized model")
    val outputTagged = new CmdOption("output-tagged", "", "STRING", "tagged file output filename")

    /* misc other knobs */
    val rootDir = new CmdOption("root-dir", "", "STRING", "project root")
    val outputDir = new CmdOption("output-dir", "", "STRING", "directory to write evaluations to")
    val writeEvals = new CmdOption("write-evals", false, "BOOLEAN", "write evaluations to separate files?")
    val useGrobidFeatures = new CmdOption("use-grobid-features", false, "BOOLEAN", "use grobid features?")
    val bilou = new CmdOption("bilou", false, "BOOLEAN", "use bilou encoding?")
    val nThreads = new CmdOption("threads", 1, "INT", "Number of threads to use during training")
    val hyperparamEval = new CmdOption("hyperparam-eval", "dev", "STRING", "On what to eval hyperparams (train|dev)")

    val taggerType = new CmdOption[String]("tagger-type", "default", "STRING", "default|grobid|combined")

  }
}
