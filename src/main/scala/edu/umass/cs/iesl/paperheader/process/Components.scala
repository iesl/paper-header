//package edu.umass.cs.iesl.paperheader.process
//
//import edu.umass.cs.iesl.paperheader.tagger._
//import cc.factorie.app.nlp.{Token, Document, DocumentAnnotator}
//
///**
// * Created by kate on 10/27/14.
// */
//
//trait Component extends DocumentAnnotator {
//  def prereqAttrs: Iterable[Class[_]] = null
//  def postAttrs: Iterable[Class[_]] = null
//  def process1(doc: Document): Document
//  override def process(doc: Document): Document = { throw new IllegalStateException("In order to make everything minimally compatible with DocumentAnnotators, you should be calling process1()") }
//  def init(): Unit = ()
//  def tokenAnnotationString(t: Token): String = ""
//}
//
//trait ChainedComponent extends Component {
//  val components: Seq[Component]
//  private lazy val processPipeline: (Document) => Document =
//    components.map(c => (d: Document) => c.process1(d)).reduceLeft(_ andThen _)
//  def process1(doc: Document): Document = processPipeline(doc)
//  private def initPipeline() = components.foreach(c => c.init())
//  override def init(): Unit = initPipeline()
//}
//
//object HeaderTaggerComponent extends Component {
//  val modelPath = System.getenv("PH_ROOT") + "/model/HeaderTagger.factorie"
//  lazy val tagger = new HeaderTagger(url=new java.net.URL("file://"+modelPath))
////  val t = new HeaderTagger()
////  val p = t.getClass.getResource("/crf/HeaderTagger.factorie")
////  lazy val tagger = new HeaderTagger(url=p)
//  def process1(doc: Document): Document = { tagger.process(doc) }
//}