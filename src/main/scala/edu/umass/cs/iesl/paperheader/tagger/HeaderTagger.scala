package edu.umass.cs.iesl.paperheader.tagger

import java.io.{BufferedInputStream, BufferedOutputStream}
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.util.BinarySerializer
import cc.factorie.variable._
import cc.factorie.app.chain._
import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.io.Source
import scala.util.matching.Regex
import cc.factorie.app.strings._
import cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions
import cc.factorie.app.chain.SegmentEvaluation

/**
 * Created by kate on 9/25/14.
 */

class HeaderTagger(val url:java.net.URL=null, useFormatting:Boolean=false) extends DocumentAnnotator {
  object FeatureDomain extends CategoricalVectorDomain[String]
  class FeatureVariable(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
    override def skipNonCategories = true
  }
  class HeaderTaggerCRFModel extends ChainModel[BioHeaderTag, FeatureVariable, Token](
    BioHeaderTagDomain,
    FeatureDomain,
    l => l.token.attr[FeatureVariable],
    l => l.token,
    t => t.attr[BioHeaderTag]
  )
  val model = new HeaderTaggerCRFModel
  val objective = cc.factorie.variable.HammingObjective


  /* DocumentAnnotator methods */
  def tokenAnnotationString(token:Token): String = s"${token.attr[BioHeaderTag].categoryValue}"
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])//, classOf[FormatInfo])
  def postAttrs: Iterable[Class[_]] = List(classOf[BioHeaderTag])
  def process(document:Document): Document = {
    if (document.tokenCount == 0) return document
    val alreadyHadFeatures = document.hasAnnotation(classOf[FeatureVariable])
    if (!alreadyHadFeatures) addFeatures(document)
    val labels = document.sections.flatMap(_.tokens).toSeq.map(_.attr[BioHeaderTag])
    model.maximize(labels)(null)
//    val lineBuffer = document.attr[LineBuffer]
//    lineBuffer.blocks.foreach(line => {
//      val labels = line.tokens.map(_.attr[BioHeaderTag])
//      model.maximize(labels)(null)
//    })
    if (!alreadyHadFeatures) {
      document.annotators.remove(classOf[FeatureVariable]); for (token <- document.tokens) token.attr.remove[FeatureVariable]
    }
    document.attr.+=(new HeaderTagSpanBuffer ++= document.sections.flatMap(section => BioHeaderTagDomain.spanList(section)))
    document
  }
  
  def processUsingModel(document: Document, m: HeaderTaggerCRFModel): Document = {
    if (document.tokenCount == 0) return document
    val alreadyHadFeatures = document.hasAnnotation(classOf[FeatureVariable])
    if (!alreadyHadFeatures) addFeatures(document)
    val labels = document.sections.flatMap(_.tokens).toSeq.map(_.attr[BioHeaderTag])
    m.maximize(labels)(null)
    if (!alreadyHadFeatures) {
      document.annotators.remove(classOf[FeatureVariable]); for (token <- document.tokens) token.attr.remove[FeatureVariable]
    }
    document.attr.+=(new HeaderTagSpanBuffer ++= document.sections.flatMap(section => BioHeaderTagDomain.spanList(section)))
    document
  }

  def lines(d:Document): LineBuffer = if (d.attr[LineBuffer] ne null) d.attr[LineBuffer] else null.asInstanceOf[LineBuffer]
  def tokens(d:Document): Seq[Token] = d.sections.flatMap(_.tokens).toSeq

//  var printIt = true
//  def addTokenFeatures(token: Token): Unit = {
//    def ngramWindow(offset: Int): Seq[Token] = if (offset < 0) token.prevWindow(offset) ++ Seq(token) else token.nextWindow(offset) ++ Seq(token)
//    val feats = new FeatureVariable(token)
//    feats ++= TokenFeatures(token)
//    feats ++= LexiconTagger.getLexiconTags(token, 0)
//    feats ++= LexiconTagger.getLexiconTags(ngramWindow(-1), Seq(-1, 0))
//    feats ++= LexiconTagger.getLexiconTags(ngramWindow(-2), Seq(-2, -1, 0))
//    feats ++= LexiconTagger.getLexiconTags(ngramWindow(1), Seq(0, 1))
//    feats ++= LexiconTagger.getLexiconTags(ngramWindow(2), Seq(0, 1, 2))
//    token.attr += feats
//  }

  def addFeatures(doc:Document): Unit = {
    doc.annotators(classOf[FeatureVariable]) = HeaderTagger.this.getClass
    val vf = (t: Token) => t.attr[FeatureVariable]
    val tokenSeq = tokens(doc)
    tokenSeq.foreach(t => {
      t.attr += new FeatureVariable(t)
      vf(t) ++= TokenFeatures(t)
    })
    addNeighboringFeatureConjunctions(tokenSeq.toIndexedSeq, (t: Token) => t.attr[FeatureVariable], List(0), List(1), List(2), List(-1), List(-2))
    LexiconTagger.tagText(tokenSeq, vf)
    
//    val vf = (t:Token) => t.attr[FeatureVariable]
//    tokenSeq.foreach(token => {
//      val feats = new FeatureVariable(token)
//      feats ++= Features(token)
//      token.attr += feats
//    })
//    tokenSeq.foreach(tok => vf(tok) ++= LexiconTagger.getLexiconTags(tok))
//    tokenSeq.sliding(2).foreach(toks => toks.foreach(t => vf(t) ++= LexiconTagger.getLexiconTags(toks)))
//    tokenSeq.sliding(3).foreach(toks => toks.foreach(t => vf(t) ++= LexiconTagger.getLexiconTags(toks)))

    //    BibtexDate.tagText(tokenSeq, vf, "BIBDATE")
//    lexicon.iesl.Month.tagText(tokenSeq ,vf,"MONTH")
//    lexicon.iesl.Day.tagText(tokenSeq ,vf,"DAY")
//
//    lexicon.wikipedia.Location.tagText(tokenSeq, vf, "WIKI-LOCATION")
////    lexicon.wikipedia.LocationAndRedirect.tagText(tokenSeq, vf, "WIKI-LOCATION-REDIRECT")
//    lexicon.iesl.Country.tagText(tokenSeq,vf, "COUNTRY")
//    lexicon.iesl.City.tagText(tokenSeq,vf, "CITY")
//    lexicon.iesl.USState.tagText(tokenSeq,vf, "USSTATE")
//    lexicon.iesl.PlaceSuffix.tagText(tokenSeq, vf, "PLACE-SUFFIX")
//    lexicon.iesl.Continents.tagText(tokenSeq, vf, "CONTINENT")
//
//    lexicon.wikipedia.Organization.tagText(tokenSeq, vf, "WIKI-ORG")
//    lexicon.wikipedia.Business.tagText(tokenSeq, vf, "WIKI-BUSINESS")
//    Affiliation.tagText(tokenSeq, vf, "BIBAFFILIATION")
//    
//    BibtexAuthor.tagText(tokenSeq, vf, "BIBAUTHOR")
//    lexicon.iesl.JobTitle.tagText(tokenSeq, vf, "JOB-TITLE")
//    lexicon.iesl.PersonFirst.tagText(tokenSeq,vf,"PERSON-FIRST")
//    lexicon.iesl.PersonFirstHigh.tagText(tokenSeq,vf,"PERSON-FIRST-HIGH")
//    lexicon.iesl.PersonFirstHighest.tagText(tokenSeq,vf,"PERSON-FIRST-HIGHEST")
//    lexicon.iesl.PersonFirstMedium.tagText(tokenSeq,vf,"PERSON-FIRST-MEDIUM")
//    lexicon.iesl.PersonLast.tagText(tokenSeq,vf,"PERSON-LAST")
//    lexicon.iesl.PersonLastHigh.tagText(tokenSeq,vf,"PERSON-LAST-HIGH")
//    lexicon.iesl.PersonLastHighest.tagText(tokenSeq,vf,"PERSON-LAST-HIGHEST")
//    lexicon.iesl.PersonLastMedium.tagText(tokenSeq,vf,"PERSON-LAST-MEDIUM")
//    lexicon.iesl.PersonHonorific.tagText(tokenSeq,vf,"PERSON-HONORIFIC")
//
//    Note.tagText(tokenSeq, vf, "BIBNOTE")
//    Publication.tagText(tokenSeq, vf, "PUBLICATION")
//    Title.tagText(tokenSeq, vf, "TITLE")
//    lexicon.wikipedia.Book.tagText(tokenSeq, vf, "WIKI-BOOK")
//    
//    if (useFormatting) addFormattingFeatures(doc)
//
////    doc.attr[LineBuffer].blocks.foreach(line => {
////      addNeighboringFeatureConjunctions(line.tokens.toIndexedSeq, vf, List(0), List(1), List(2), List(-1), List(-2))
////    })
//    
////    tokenSeq.grouped(10).foreach(g => addNeighboringFeatureConjunctions(g.toIndexedSeq, vf, List(0), List(1), List(2), List(-1), List(-2)))
//    addNeighboringFeatureConjunctions(tokenSeq.toIndexedSeq, vf, List(0), List(1), List(2), List(-1), List(-2))
//
//    tokenSeq.foreach(t => {
//      val feats = vf(t)
//      if (t.string.length > 5) {
//        feats += "P="+cc.factorie.app.strings.prefix(Features.lemma(t), 4)
//        feats += "S="+cc.factorie.app.strings.suffix(Features.lemma(t), 4)
//      }
//      feats ++= t.prevWindow(4).map(t2 => "PREVWINDOW="+Features.lemma(t2))
//      feats ++= t.nextWindow(4).map(t2 => "NEXTWINDOW="+Features.lemma(t2))
//    })
//
//    if (printIt) {
//      tokenSeq.take(20).foreach(t => {
//        println(t.string)
//        println(vf(t))
//        println()
//      })
//    }
//    printIt = false
  }
  def addFormattingFeatures(doc:Document): Unit = {
    val vf = (t:Token) => t.attr[FeatureVariable]
    /* Line spacing */
    doc.attr[LineBuffer].blocks(0).tokens.foreach(t => {
      vf(t) += "YDIFF=0"
      //      vf(t) += "XDIFF=0"
    })
    doc.attr[LineBuffer].blocks.sliding(2).foreach(pair => {
      if (pair.length == 2) {
        val line1: Line = pair(0)
        val line2: Line = pair(1)
        val verticalDiff = Math.abs(line1.ypos - line2.ypos)
        line2.tokens.foreach(t => vf(t) += s"YDIFF=$verticalDiff")
        //        val xDiff = Math.abs(line1.tokens(0).attr[FormatInfo].xpos - line2.tokens(0).attr[FormatInfo].xpos)
        //        line2.tokens.foreach(t => vf(t) += s"XDIFF=$xDiff")
      }
    })
  }

  def train(trainDocs:Seq[Document], testDocs:Seq[Document], l1:Double=0.1, l2:Double=0.1, lr:Double=0.1, delta:Double=0.1)(implicit random:scala.util.Random): Double = {
    def labels(docs:Seq[Document]): Seq[LabeledBioHeaderTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledBioHeaderTag])).toSeq
    if (useFormatting) FormatData.calculateMaxDims(trainDocs)
    println("adding training features...")
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()
    testDocs.par.foreach(addFeatures)
    
    /* print some features for debugging */
    trainDocs(0).sections.flatMap(_.tokens).take(5).foreach(t => {
      val feats = t.attr[FeatureVariable]
      println(s"${feats.activeCategories.mkString(", ")}")
      print("")
    })
    trainDocs(1).sections.flatMap(_.tokens).take(5).foreach(t => {
      val feats = t.attr[FeatureVariable]
      println(s"${feats.activeCategories.mkString(", ")}")
      print("")
    })
    
    
    val trainLabels = labels(trainDocs)
    val testLabels = labels(testDocs)
    // examples by document
    val examples = trainDocs.map(doc => {
      val tokens = doc.sections.flatMap(_.tokens)
      new model.ChainLikelihoodExample(tokens.map(_.attr[LabeledBioHeaderTag]))
    })
    val optimizer = new optimize.AdaGradRDA(delta=delta, rate=lr, l1=l1, l2=l2, numExamples=examples.length)
    def evaluate(): Unit = {
      trainDocs.foreach(process)
      println("")
      println("Train accuracy (overall): "+objective.accuracy(trainLabels))
      println("Training:")
      println(new SegmentEvaluation[LabeledBioHeaderTag]("(B|I)-", "I-", BioHeaderTagDomain, trainLabels.toIndexedSeq))
//      Eval(BioHeaderTagDomain, trainLabels)
      if (testDocs.nonEmpty) {
        testDocs.par.foreach(process)
        println("Test  accuracy (overall): "+objective.accuracy(testLabels))
        println("Testing:")
        println(new SegmentEvaluation[LabeledBioHeaderTag]("(B|I)-", "I-", BioHeaderTagDomain, testLabels.toIndexedSeq))
//        Eval(BioHeaderTagDomain, testLabels)
      }
      println(model.parameters.tensors.sumInts(t => t.toSeq.count(x => x == 0)).toFloat/model.parameters.tensors.sumInts(_.length)+" sparsity")
    }
    println("training...")
    optimize.Trainer.onlineTrain(model.parameters, examples, optimizer=optimizer, evaluate=evaluate)
    testDocs.foreach(process)
    new SegmentEvaluation[LabeledBioHeaderTag]("(B|I)-", "I-", BioHeaderTagDomain, testLabels.toIndexedSeq).f1
  }

  def trainKFold(trainDocs:Seq[Document], testDocs:Seq[Document], 
                 k: Int = 10, l1:Double=0.1, l2:Double=0.1, lr:Double=0.1, delta:Double=0.1, iters:Int=5)(implicit random: scala.util.Random): Double = {
    def labels(docs:Seq[Document]): Seq[LabeledBioHeaderTag] = docs.flatMap(doc => doc.tokens.map(_.attr[LabeledBioHeaderTag])).toSeq
    trainDocs.foreach(addFeatures)
    FeatureDomain.freeze()
    testDocs.foreach(addFeatures)
    val allTestLabels = labels(testDocs)
    val kPortion = (trainDocs.length / k).toDouble
    val groups = trainDocs.grouped(kPortion.floor.toInt).toIndexedSeq
    val models = new ArrayBuffer[HeaderTaggerCRFModel]()
    val accuracies = Array.fill(groups.length-1)(0.0)
    for (i <- 0 until groups.length-1) {
      val tmpModel = new HeaderTaggerCRFModel
      val heldout = groups(i)
      val testLabels = labels(heldout)
      val rest = (0 until i-1).map(j => groups(j)) ++ (i+1 until groups.length).map(j => groups(j))
      val restDocs = rest.flatten
      val trainLabels = labels(restDocs)
      val examples = restDocs.map(doc => new tmpModel.ChainLikelihoodExample(doc.sections.flatMap(_.tokens).map(_.attr[LabeledBioHeaderTag])))
      val optimizer = new optimize.AdaGradRDA(delta=delta, rate=lr, l1=l1, l2=l2, numExamples=examples.length)
      def evaluate(): Unit = {
        restDocs.foreach(doc => processUsingModel(doc, tmpModel))
        println("Train accuracy (overall): "+objective.accuracy(trainLabels))
        println(new SegmentEvaluation[LabeledBioHeaderTag]("B-", "I-", BioHeaderTagDomain, trainLabels.toIndexedSeq))
        heldout.par.foreach(doc => processUsingModel(doc, tmpModel))
        println("Test  accuracy (overall): "+objective.accuracy(testLabels))
        println("Testing:")
        println(new SegmentEvaluation[LabeledBioHeaderTag]("B-", "I-", BioHeaderTagDomain, testLabels.toIndexedSeq))
      }
      println(s"training (iter=$i)...")
      optimize.Trainer.onlineTrain(tmpModel.parameters, examples, optimizer=optimizer, evaluate=evaluate, maxIterations=iters)
      heldout.par.foreach(doc => processUsingModel(doc, tmpModel))
      accuracies(i) = new SegmentEvaluation[LabeledBioHeaderTag]("B-", "I-", BioHeaderTagDomain, testLabels.toIndexedSeq).f1
      models += tmpModel
    }
    val bestModel: HeaderTaggerCRFModel = models.toSeq.zip(accuracies).maxBy(_._2)._1
    testDocs.foreach(doc => processUsingModel(doc, bestModel))
    new SegmentEvaluation[LabeledBioHeaderTag]("B-", "I-", BioHeaderTagDomain, allTestLabels.toIndexedSeq).f1
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
  val useFormatting = new CmdOption("use-formatting", false, "BOOLEAN", "use formatting features")
}

object HeaderTaggerTrainer extends cc.factorie.util.HyperparameterMain {
  def evaluateParameters(args:Array[String]): Double = {
    implicit val random = new scala.util.Random(0)
    val opts = new HeaderTaggerOpts
    opts.parse(args)
    val tagger = new HeaderTagger
    assert(opts.train.wasInvoked && opts.test.wasInvoked)
    val trainDocs = LoadTSV(opts.train.value, withFormatting = opts.useFormatting.value)
    val testDocs = LoadTSV(opts.test.value, withFormatting = opts.useFormatting.value)

    // print some statistics
    println(s"using ${trainDocs.length} training docs with ${trainDocs.map(_.tokenCount).sum} tokens total")
    println(s"using ${testDocs.length} training docs with ${testDocs.map(_.tokenCount).sum} tokens total")
    println(s"using hyperparams: l1=${opts.l1.value} , l2=${opts.l2.value} , lr=${opts.learningRate.value}, delta=${opts.delta.value}")

        val result = tagger.train(trainDocs, testDocs, l1=opts.l1.value, l2=opts.l2.value, lr=opts.learningRate.value)
//    val result = tagger.trainKFold(trainDocs, testDocs, l1=opts.l1.value, l2=opts.l2.value, lr=opts.learningRate.value)

    println(s"FINAL RESULT: f1 = $result")

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
    //    val delta = HyperParameter(opts.delta, new LogUniformDoubleSampler(1e-4, 10))
    val qs = new cc.factorie.util.QSubExecutor(8, "edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTrainer")
    val optimizer = new HyperParameterSearcher(opts, Seq(l1, l2, lr), qs.execute, 100, 90, 60)
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
    val opts = new HeaderTaggerOpts
    val phroot = System.getenv("PH_ROOT")
    val modelPath = phroot+"/model/HeaderTagger.factorie"
    val tagger = new HeaderTagger(url = new java.net.URL("file://" + modelPath))
    val dataPath = phroot+"/data/fullpaper-headers.tsv"
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

