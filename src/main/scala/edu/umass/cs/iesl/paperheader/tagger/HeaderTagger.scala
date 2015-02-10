package edu.umass.cs.iesl.paperheader.tagger

import java.io.{BufferedInputStream, BufferedOutputStream}
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.util.BinarySerializer
import cc.factorie.variable._
import cc.factorie.app.chain._
import scala.collection.mutable
import java.io._
import scala.io.Source
import scala.util.matching.Regex
import cc.factorie.app.strings._
import cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions
import cc.factorie.app.chain.SegmentEvaluation

/**
 * Created by kate on 9/25/14.
 */

class HeaderTagger(val url:java.net.URL=null) extends DocumentAnnotator {
  object FeatureDomain extends CategoricalVectorDomain[String]
  class FeatureVariable(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
    override def skipNonCategories = true
  }
  class HModel extends ChainModel[BioHeaderTag, FeatureVariable, Token](
    BioHeaderTagDomain,
    FeatureDomain,
    l => l.token.attr[FeatureVariable],
    l => l.token,
    t => t.attr[BioHeaderTag]
  )
  val model = new HModel
  val objective = cc.factorie.variable.HammingObjective


  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = s"${token.attr[BioHeaderTag].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])//, classOf[FormatInfo])
  def postAttrs: Iterable[Class[_]] = List(classOf[BioHeaderTag])
  def process(document:Document): Document = {
    if (document.tokenCount == 0) return document
    val alreadyHadFeatures = document.hasAnnotation(classOf[FeatureVariable])
    if (!alreadyHadFeatures) addFeatures(document)
    val lineBuffer = document.attr[LineBuffer]
    lineBuffer.blocks.foreach(line => {
      val labels = line.tokens.map(_.attr[BioHeaderTag])
      model.maximize(labels)(null)
    })
    if (!alreadyHadFeatures) {
      document.annotators.remove(classOf[FeatureVariable]); for (token <- document.tokens) token.attr.remove[FeatureVariable]
    }
    document.attr.+=(new HeaderTagSpanBuffer ++= document.sections.flatMap(section => BioHeaderTagDomain.spanList(section)))
    document
  }

  def lines(d:Document): LineBuffer = if (d.attr[LineBuffer] ne null) d.attr[LineBuffer] else null.asInstanceOf[LineBuffer]
  def tokens(d:Document): Seq[Token] = d.sections.flatMap(_.tokens).toSeq
  var printCount = 0

  def addFeatures(doc:Document): Unit = {
    doc.annotators(classOf[FeatureVariable]) = HeaderTagger.this.getClass
    val tokenSeq = tokens(doc)
    val vf = (t:Token) => t.attr[FeatureVariable]
    tokenSeq.foreach(token => {
      val feats = new FeatureVariable(token)
      feats ++= Features(token)
      token.attr += feats
    })
    BibtexDate.tagText(tokenSeq, vf, "BIBDATE")
    lexicon.iesl.Month.tagText(tokenSeq ,vf,"MONTH")
    lexicon.iesl.Day.tagText(tokenSeq ,vf,"DAY")
    lexicon.wikipedia.Location.tagText(tokenSeq, vf, "WIKI-LOCATION")
    lexicon.iesl.Country.tagText(tokenSeq,vf, "COUNTRY")
    lexicon.iesl.City.tagText(tokenSeq,vf, "CITY")
    lexicon.iesl.USState.tagText(tokenSeq,vf, "USSTATE")
    lexicon.iesl.PlaceSuffix.tagText(tokenSeq, vf, "PLACE-SUFFIX")
    lexicon.wikipedia.Organization.tagText(tokenSeq, vf, "WIKI-ORG")
    Affiliation.tagText(tokenSeq, vf, "BIBAFFILIATION")
    BibtexAuthor.tagText(tokenSeq, vf, "BIBAUTHOR")
    lexicon.iesl.PersonFirst.tagText(tokenSeq,vf,"PERSON-FIRST")
    lexicon.iesl.PersonFirstHigh.tagText(tokenSeq,vf,"PERSON-FIRST-HIGH")
    lexicon.iesl.PersonFirstHighest.tagText(tokenSeq,vf,"PERSON-FIRST-HIGHEST")
    lexicon.iesl.PersonFirstMedium.tagText(tokenSeq,vf,"PERSON-FIRST-MEDIUM")
    lexicon.iesl.PersonLast.tagText(tokenSeq,vf,"PERSON-LAST")
    lexicon.iesl.PersonLastHigh.tagText(tokenSeq,vf,"PERSON-LAST-HIGH")
    lexicon.iesl.PersonLastHighest.tagText(tokenSeq,vf,"PERSON-LAST-HIGHEST")
    lexicon.iesl.PersonLastMedium.tagText(tokenSeq,vf,"PERSON-LAST-MEDIUM")
    lexicon.iesl.PersonHonorific.tagText(tokenSeq,vf,"PERSON-HONORIFIC")
    //TODO layout/formatting features
    doc.attr[LineBuffer].blocks.foreach(line => {
      val ypos = line.ypos
      line.tokens.foreach(t => t.attr[FeatureVariable] += "YPOS=" + ypos)
    })
    doc.attr[LineBuffer].blocks.foreach(line => {
      addNeighboringFeatureConjunctions(line.tokens.toIndexedSeq, vf, List(0), List(1), List(2), List(-1), List(-2))
    })
    tokenSeq.foreach(t => {
      val feats = vf(t)
      if (t.string.length > 5) {
        feats += "P="+cc.factorie.app.strings.prefix(Features.lemma(t), 4)
        feats += "S="+cc.factorie.app.strings.suffix(Features.lemma(t), 4)
      }
      feats ++= t.prevWindow(4).map(t2 => "PREVWINDOW="+Features.lemma(t2))
      feats ++= t.nextWindow(4).map(t2 => "NEXTWINDOW="+Features.lemma(t2))
    })
  }

  def train(trainDocs:Seq[Document], testDocs:Seq[Document], l1:Double=0.1, l2:Double=0.1, lr:Double=0.1, delta:Double=0.1)(implicit random:scala.util.Random): Double = {
    def labels(docs:Seq[Document]): Seq[LabeledBioHeaderTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledBioHeaderTag])).toSeq
    println("adding training features...")
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()
    testDocs.par.foreach(addFeatures)
    val trainLabels = labels(trainDocs)
    val testLabels = labels(testDocs)
    val lineBuffers = trainDocs.map(doc => doc.attr[LineBuffer])
    val lines = lineBuffers.flatMap(buf => buf.blocks)
    val examples = lines.map(line => new model.ChainLikelihoodExample(line.tokens.map(_.attr[LabeledBioHeaderTag])))
    val optimizer = new optimize.AdaGradRDA(delta=delta, rate=lr, l1=l1, l2=l2, numExamples=examples.length)
    def evaluate(): Unit = {
      trainDocs.foreach(process)
      println("")
      println("Train accuracy (overall): "+objective.accuracy(trainLabels))
      println("Training:")
      println(new SegmentEvaluation[LabeledBioHeaderTag]("(B|I)-", "I-", BioHeaderTagDomain, trainLabels.toIndexedSeq))
      if (testDocs.nonEmpty) {
        testDocs.par.foreach(process)
        println("Test  accuracy (overall): "+objective.accuracy(testLabels))
        println("Testing:")
        println(new SegmentEvaluation[LabeledBioHeaderTag]("(B|I)-", "I-", BioHeaderTagDomain, testLabels.toIndexedSeq))
      }
      println(model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat/model.parameters.tensors.sumInts(_.length)+" sparsity")
    }
    println("training...")
    optimize.Trainer.onlineTrain(model.parameters, examples, optimizer=optimizer, evaluate=evaluate)
    testDocs.foreach(process)
    new SegmentEvaluation[LabeledBioHeaderTag]("(B|I)-", "I-", BioHeaderTagDomain, testLabels.toIndexedSeq).f1
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
    is.close()
  }

  def deSerialize(stream: InputStream) {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.deserialize(model, is)
    is.close()
  }
}

class HeaderTaggerOpts extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions {
  val saveModel = new CmdOption("save-model", "HeaderTagger.factorie", "STRING", "Filename for the model (saving a trained model or reading a running model.")
  val serialize = new CmdOption("serialize", false, "BOOLEAN", "Whether to serialize at all")
  val train = new CmdOption("train", "", "STRING", "Filename(s) from which to read training data")
  val test = new CmdOption("test", "", "STRING", "Filename(s) from which to read test data")
  val l1 = new CmdOption("l1", 1.424388380418031E-5, "FLOAT", "L1 regularizer for AdaGradRDA training.")
  val l2 = new CmdOption("l2", 0.06765909781125444, "FLOAT", "L2 regularizer for AdaGradRDA training.")
  val learningRate = new CmdOption("learning-rate", 0.8515541191715452, "FLOAT", "base learning rate")
  val delta = new CmdOption("delta", 0.1, "FLOAT", "learning rate delta")
  val nerModel = new CmdOption("ner-model", "", "STRING", "path to serialized named entity tagger")
  // TODO parse docs before processing too
  val parserModel = new CmdOption("parser", "", "STRING", "path to serialized parser")
}

object HeaderTaggerTrainer extends cc.factorie.util.HyperparameterMain {
  def evaluateParameters(args:Array[String]): Double = {
    import cc.factorie.app.nlp.ner.{LabeledBilouConllNerTag, BilouConllNerDomain}
    println("got args: " + args.mkString(" "))
    implicit val random = new scala.util.Random(0)
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val tagger = new HeaderTagger
    assert(opts.train.wasInvoked)

    // load training data
    val allDocs = LoadTSV(opts.train.value)
    val trainPortionToTake = if(opts.trainPortion.wasInvoked) opts.trainPortion.value.toDouble  else 0.75
    // FIXME tokenCount should be > 0 for all docs (bug in LoadTSV, shouldnt filter here)
    val trainDocs = allDocs.take((allDocs.length*trainPortionToTake).floor.toInt)
    val testDocs = allDocs.drop(trainDocs.length)
    // print some statistics
    println(s"using ${trainDocs.length} training docs with ${trainDocs.map(_.tokenCount).sum} tokens total")
    println(s"using ${testDocs.length} training docs with ${testDocs.map(_.tokenCount).sum} tokens total")
    println(s"using hyperparams: l1=${opts.l1.value} , l2=${opts.l2.value} , lr=${opts.learningRate.value}, delta=${opts.delta.value}")

//    if (opts.nerModel.wasInvoked) {
//      println("ner tagging documents...")
//      val ner = new cc.factorie.app.nlp.ner.ConllChainNer(url=new java.net.URL("file://"+opts.nerModel.value))
//      trainDocs.foreach(ner.process)
//      testDocs.foreach(ner.process)
//    }

    // training
    val result = tagger.train(trainDocs, testDocs, l1=opts.l1.value, l2=opts.l2.value, lr=opts.learningRate.value)
    println(s"FINAL RESULT: f1 = $result")

    assert(result != 1.0)
    if (opts.serialize.value){
      val fname = if (opts.saveModel.wasInvoked) opts.saveModel.value else "HeaderTagger.factorie"
      println(s"saving model to: $fname")
      tagger.serialize(new FileOutputStream(fname))
    }
    result
  }
}

object HeaderTaggerOptimizer {
  def main(args: Array[String]): Unit = {
    import cc.factorie.util.{HyperParameter, LogUniformDoubleSampler, HyperParameterSearcher}
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    opts.serialize.setValue(false)
    val l1 = HyperParameter(opts.l1, new LogUniformDoubleSampler(1e-5, 10))
    val l2 = HyperParameter(opts.l2, new LogUniformDoubleSampler(1e-5, 10))
    val lr = HyperParameter(opts.learningRate, new LogUniformDoubleSampler(1e-4, 10))
    val delta = HyperParameter(opts.delta, new LogUniformDoubleSampler(1e-4, 10))
    val qs = new cc.factorie.util.QSubExecutor(8, "edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTrainer")
    val optimizer = new HyperParameterSearcher(opts, Seq(l1, l2, lr, delta), qs.execute, 100, 90, 60)
    val result = optimizer.optimize()
    println("Got results: " + result.mkString(" "))
    println("Best l1: " + opts.l1.value + " best l2: " + opts.l2.value + " best lr: " + opts.learningRate.value + " best delta: " + opts.delta.value)
    println("Running best configuration...")
    opts.serialize.setValue(true)
    import scala.concurrent.duration._
    import scala.concurrent.Await
    Await.result(qs.execute(opts.unParse.toArray), 1.hours)
    println("Done.")
  }
}

//TODO pass data/model as opts not hard-coded
object HeaderTaggerTester {
  def main(args: Array[String]): Unit = {
    val modelPath = "/home/kate/research/paper-header/model/HeaderTagger.factorie"
    val tagger = new HeaderTagger(url = new java.net.URL("file://" + modelPath))
    val dataPath = "/home/kate/research/paper-header/data/fullpaper-headers.tsv"
    val allDocs = LoadTSV(dataPath)
    val trainPortion = 0.8
    val trainDocs = allDocs.take((allDocs.length*trainPortion).floor.toInt)
    val testDocs = allDocs.drop(trainDocs.length)
    val labels = new scala.collection.mutable.ListBuffer[LabeledBioHeaderTag]()
    testDocs.foreach(doc => {
      val tokens = doc.sentences.flatMap(_.tokens)
      labels ++= tokens.map(_.attr[LabeledBioHeaderTag])
      tagger.process(doc)
    })
    Eval(BioHeaderTagDomain, labels)
  }
}

object BibtexAuthor extends lexicon.TriePhraseLexicon("bibtex-author") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_author_full"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource\n") } }
}

object BibtexDate extends lexicon.TriePhraseLexicon("bibtex-date") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_date"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("could not find resource") } }
}

object Note extends lexicon.TriePhraseLexicon("note") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_note"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource") } }
}

object Affiliation extends lexicon.TriePhraseLexicon("affiliation") {
  val reader = Source.fromURL(getClass.getResource("/bibtex-lexicons/lexicon_affiliation"))
  try { for (line <- reader.getLines(); entry <- line.trim.split("\t")) this += entry } catch { case e:java.io.IOException => { throw new Error("Could not find resource") } }
}

