package edu.umass.cs.iesl.paperheader.tagger

import java.io.{BufferedInputStream, BufferedOutputStream}
import cc.factorie._
import cc.factorie.model._
import cc.factorie.optimize._
import cc.factorie.app.nlp._
import cc.factorie.util.{JavaHashSet, JavaHashMap, BinarySerializer}
import cc.factorie.variable._
import cc.factorie.app.chain._
import edu.umass.cs.iesl.paperheader.load._
import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.util.Random._
import cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions
import cc.factorie.app.chain.SegmentEvaluation

/**
 * Created by kate on 9/25/14.
 */

class GrobidTagger(val url:java.net.URL=null, useFormatting:Boolean=false) extends DocumentAnnotator {

  object FeatureDomain extends CategoricalVectorDomain[String]
  class FeatureVariable(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
    override def skipNonCategories = true
  }
  class GrobidTaggerCRFModel extends ChainModel[BilouGrobidTag, FeatureVariable, Token](
    BilouGrobidTagDomain,
    FeatureDomain,
    l => l.token.attr[FeatureVariable],
    l => l.token,
    t => t.attr[BilouGrobidTag]
  )

  val model = new GrobidTaggerCRFModel
  val objective = cc.factorie.variable.HammingObjective

  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = s"${token.attr[BilouGrobidTag].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])//, classOf[FormatInfo])
  def postAttrs: Iterable[Class[_]] = List(classOf[BilouGrobidTag])
  def process(document:Document): Document = {
    if (document.tokenCount == 0) return document
//    if (!document.tokens.head.attr.contains(classOf[BilouGrobidTag]))
//      document.tokens.map(token => token.attr += new BilouGrobidTag(token, "I-abstract"))
    val alreadyHadFeatures = document.hasAnnotation(classOf[FeatureVariable])
    if (!alreadyHadFeatures) addFeatures(document)
    process1(document)
    document.attr.+=(new GrobidTagSpanBuffer ++= document.sections.flatMap(section => BilouGrobidTagDomain.spanList(section)))
    document
  }

  def process1(doc:Document): Unit = {
    if (doc.tokens.size > 0) {
//      if (!doc.tokens.head.attr.contains(classOf[BilouGrobidTag])) {
//        doc.tokens.foreach(t => t.attr += new BilouGrobidTag(t, "I-abstract"))
//      }
      doc.tokens.foreach(tok => if (!tok.attr.contains(classOf[BilouGrobidTag])) tok.attr += new BilouGrobidTag(tok, "I-abstract"))
      val vars = doc.tokens.map(_.attr[BilouGrobidTag]).toSeq
      model.maximize(vars)(null)
//      for (sentence <- doc.sentences if sentence.tokens.size > 0) {
//        val vars = sentence.tokens.map(_.attr[BilouGrobidTag]).toSeq
//        model.maximize(vars)(null)
//      }
    }
  }


  def addFeatures(doc:Document): Unit = {
    doc.annotators(classOf[FeatureVariable]) = GrobidTagger.this.getClass
    val vf = (t: Token) => t.attr[FeatureVariable]
    val tokenSeq = doc.tokens.toSeq
    tokenSeq.foreach(t => t.attr += new FeatureVariable(t))
    tokenSeq.foreach(t => {
      vf(t) ++= TokenFeatures(t)
    })
    LexiconTagger.tagText(tokenSeq, vf)
//    for (sentence <- doc.sentences) {
//      addNeighboringFeatureConjunctions(sentence.tokens.toIndexedSeq, vf, List(0), List(1), List(2), List(-1), List(-2))
//      val sfeats = SentenceFeatures(sentence)
//      if (sfeats.length > 0) sentence.tokens.foreach(t => vf(t) ++= sfeats)
////      // ambiguity classes
////      for (token <- sentence.tokens) {
////        val amb0 = WordData.ambiguityClasses.getOrElse(token.lemmaStr, null)
////        if (amb0 != null) vf(token) += s"AMB@0=$amb0"
////        for (i <- List(1, 2)) {
////          val next = token.next(i)
////          if (next != null) {
////            val amb = WordData.ambiguityClasses.getOrElse(token.lemmaStr, null)
////            if (amb != null) vf(token) += s"AMB@$i=$amb"
////          }
////        }
////      }
//    }
  }

  def train(trainDocs:Seq[Document], testDocs:Seq[Document], params: HyperParams)(implicit random:scala.util.Random): Double = {
    def labels(docs:Seq[Document]): Seq[LabeledBilouGrobidTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledBilouGrobidTag])).toSeq

//    println("computing WordData...")
//    WordData.computeWordFormsByDocFreq(trainDocs)
//    WordData.computeAmbiguityClasses(trainDocs)

    println("adding training features...")
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()
    testDocs.foreach(addFeatures)

    val trainLabels = labels(trainDocs)
    val testLabels = labels(testDocs)

    def evaluate(): Unit = {
      trainDocs.par.foreach(process)
      println("Train accuracy (overall): "+objective.accuracy(trainLabels))
      println("Training:")
      println(new SegmentEvaluation[LabeledBilouGrobidTag]("(B|U)-", "(I|L)-", BilouGrobidTagDomain, trainLabels.toIndexedSeq))
      if (testDocs.nonEmpty) {
        testDocs.par.foreach(process)
        println("Test  accuracy (overall): "+objective.accuracy(testLabels))
        println("Testing:")
        println(new SegmentEvaluation[LabeledBilouGrobidTag]("(B|U)-", "(I|L)-", BilouGrobidTagDomain, testLabels.toIndexedSeq))
      }
      else println(model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat/model.parameters.tensors.sumInts(_.length)+" sparsity (model1)")
    }

//    val vars = for (td <- trainDocs; sentence <- td.sentences if sentence.length > 0) yield sentence.tokens.map(_.attr[LabeledBilouGrobidTag])
//    trainDocs.flatMap(_.tokens).foreach(t => assert(t.attr[FeatureVariable] != null && t.attr[FeatureVariable].activeCategories.length > 0))
    val vars = for (td <- trainDocs if td.tokens.size > 0) yield td.tokens.map(_.attr[LabeledBilouGrobidTag])
    val examples = vars.map(v => new model.ChainLikelihoodExample(v.toSeq))
    val optimizer = new AdaGradRDA(l1=params.l1, l2=params.l2, delta=params.delta, rate=params.learningRate, numExamples=examples.length)
    println(s"training using ${examples.length} examples...")
//    examples.foreach(ex => assert(ex != null, "null example"))
    Trainer.onlineTrain(model.parameters, examples, optimizer=optimizer, evaluate=evaluate, useParallelTrainer=false)
    trainDocs.foreach(process)
    testDocs.foreach(process)

    println("FINAL:")
    val eval = new SegmentEvaluation[LabeledBilouGrobidTag]("(B|U)-", "(I|L)-", BilouGrobidTagDomain, testLabels.toIndexedSeq)
    println(eval)
    eval.f1
  }

  if (url != null) {
    deSerialize(url.openConnection.getInputStream)
    FeatureDomain.freeze()
    println("Found model")
  }
  else {
    println("model not found")
  }

  def serialize(stream: OutputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.serialize(model, is)
//    BinarySerializer.serialize(WordData.ambiguityClasses, is)
    is.close()
  }

  def deSerialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.deserialize(model, is)
//    BinarySerializer.deserialize(WordData.ambiguityClasses, is)
    is.close()
  }
}

object GrobidTaggerTrainer extends cc.factorie.util.HyperparameterMain {
  import edu.umass.cs.iesl.paperheader.load.GrobidMung
  def stats(docs: Seq[Document]): Unit = {
    import scala.collection.mutable.HashMap
    val table = new HashMap[String,Int]()
    GrobidTagDomain.categories.foreach(c => table(c) = 0)
    var tokenCount = 0
    for (doc <- docs; token <- doc.tokens) {
      val label = token.attr[LabeledBilouGrobidTag].categoryValue.substring(2)
      table(label) += 1
      tokenCount += 1
    }
    table.foreach { case (label, count) => println(s"$label\t$count\t${count.toDouble / tokenCount.toDouble}")}
  }
  def evaluateParameters(args:Array[String]): Double = {
    implicit val random = new scala.util.Random(0)
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val tagger = new GrobidTagger

    val trainDir = opts.trainDir.value
    val allFiles = new GrobidMung().mungFiles(trainDir)
    //80-20 split
    val trainPart = (allFiles.length.toDouble * 0.8).floor.toInt
    val trainDocs = allFiles.take(trainPart)
    val devDocs = allFiles.drop(trainPart)

    stats(trainDocs)

    val hyperparams = HyperParams(opts)

    val result = tagger.train(trainDocs, devDocs, hyperparams)

    if (opts.serialize.value){
      val fname = if (opts.saveModel.wasInvoked) opts.saveModel.value else "HeaderTagger.factorie"
      println(s"serializing model to: $fname")
      tagger.serialize(new FileOutputStream(fname))
    }

    result
  }
}

object GrobidTaggerTester {
  def main(args: Array[String]): Unit = {
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val tagger = new GrobidTagger(url=new java.net.URL(opts.model.value))
    val docs = new GrobidMung().mungFiles(opts.testDir.value)
    val labels = docs.flatMap(_.tokens).map(_.attr[LabeledBilouGrobidTag])
    docs.foreach(tagger.process)
    val eval = new SegmentEvaluation[LabeledBilouGrobidTag]("(B|U)-", "(I|L)-", BilouGrobidTagDomain, labels.toIndexedSeq)
    println(eval)
  }
}


