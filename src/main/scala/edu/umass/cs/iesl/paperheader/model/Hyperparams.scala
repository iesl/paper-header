package edu.umass.cs.iesl.paperheader.model

import edu.umass.cs.iesl.paperheader._

/**
 * Created by kate on 11/14/15.
 */
case class Hyperparams(opts: HeaderTaggerOpts) {
  val l1 = opts.l1.value
  val l2 = opts.l2.value
  val learningRate = opts.learningRate.value
  val delta = opts.delta.value
  val iters = opts.numIterations.value
  val optimizer = opts.optimizer.value

  val embeddingDim: Int = opts.embeddingDim.value
  val scale: Double = opts.scale.value
  val useOffsetEmbedding: Boolean = opts.useOffsetEmbedding.value

  val eval = opts.hyperparamEval.value
  override def toString(): String = s"Hyperparams(l1=$l1 l2=$l2 rate=$learningRate delta=$delta)"
}
