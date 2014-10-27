package edu.umass.cs.iesl.paperheader.process

import edu.umass.cs.iesl.paperheader.crf
import cc.factorie.app.nlp.{Token, Document, DocumentAnnotator}

/**
 * Created by kate on 10/27/14.
 */

trait Component extends DocumentAnnotator {
  def prereqAttrs: Iterable[Class[_]] = null
  def postAttrs: Iterable[Class[_]] = null
  def process1(doc: Document): Document
  override def process(doc: Document): Document = { throw new IllegalStateException("In order to make everything minimally compatible with DocumentAnnotators, you should be calling process1()") }
  def init(): Unit = ()
  def tokenAnnotationString(t: Token): String = ""
}

trait ChainedComponent extends Component {
  val components: Seq[Component]
  private lazy val processPipeline: (Document) => Document =
    components.map(c => (d: Document) => c.process1(d)).reduceLeft(_ andThen _)
  def process1(doc: Document): Document = processPipeline(doc)
  private def initPipeline() = components.foreach(c => c.init())
  override def init(): Unit = initPipeline()
}


object HeaderTaggerComponent extends Component {
  println("\n\n trying to load HeaderTaggerComponent...")
  val t = new crf.HeaderTagger()
  val p = t.getClass.getResource("/crf/HeaderTagger.factorie")
  print(s"path to HeaderTagger model: ${p.toString}")
  lazy val tagger = new crf.HeaderTagger(url=p)
  def process1(doc: Document): Document = { tagger.process(doc) }
}