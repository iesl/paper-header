package edu.umass.cs.iesl.paperheader.model

import edu.umass.cs.iesl.paperheader._
/**
 * Created by kate on 1/26/16.
 */
case class Hyperparams(opts: HeaderTaggerOpts) {
  val optimizer: String = opts.optimizer.value
  val learningRate = opts.learningRate.value
  val delta = opts.delta.value
  val l1 = opts.l1.value
  val l2 = opts.l2.value
  val numIterations = opts.numIterations.value
  val trimBelow: Int = opts.trimBelow.value
  val segmentScheme: String = opts.segmentScheme.value

}
