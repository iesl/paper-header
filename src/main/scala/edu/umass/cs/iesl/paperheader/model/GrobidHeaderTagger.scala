package edu.umass.cs.iesl.paperheader.model

import java.io._

import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.util.BinarySerializer
import edu.umass.cs.iesl.paperheader.load.PreFeatures

/**
 * Created by kate on 1/26/16.
 */
class GrobidHeaderTagger extends AbstractHeaderTagger {

  def this(url: java.net.URL) = {
    this()
    deserialize(url.openConnection().getInputStream)
    log.info(s"deserialized model from ${url.getPath}")
  }

  def this(path: String) = this(new File(path).toURL())

  override def addFeatures(document: Document): Unit = {
    val vf = (t: Token) => t.attr[HeaderFeatures]
    document.tokens.foreach { token =>
      val features = new HeaderFeatures(token)
      features ++= token.attr[PreFeatures].features
      token.attr += features
    }
  }

  def serialize(stream: OutputStream): Unit = {
    import cc.factorie.util.CubbieConversions._
    log.info(s"label domain size: ${HeaderLabelDomain.size}")
    log.info(s"feature domain size: ${FeatureDomain.dimensionDomain.size}")
    //    log.info(s"model sparsity: ${model.sparsity}")
    log.info(s"model sparsity: ${sparsity}")
    val is = new DataOutputStream(new BufferedOutputStream(stream))
    BinarySerializer.serialize(HeaderLabelDomain, is)
    BinarySerializer.serialize(FeatureDomain.dimensionDomain, is)
    BinarySerializer.serialize(model, is)
    is.close()
  }

  def deserialize(stream: InputStream): Unit = {
    import cc.factorie.util.CubbieConversions._
    val is = new DataInputStream(new BufferedInputStream(stream))
    BinarySerializer.deserialize(HeaderLabelDomain, is)
    HeaderLabelDomain.freeze()
    BinarySerializer.deserialize(FeatureDomain.dimensionDomain, is)
    FeatureDomain.freeze()
    BinarySerializer.deserialize(model, is)
    is.close()
    log.info(s"label domain size: ${HeaderLabelDomain.size}")
    log.info(s"feature domain size: ${FeatureDomain.dimensionDomain.size}")
//    log.info(s"model sparsity: ${model.sparsity}")
    log.info(s"model sparsity: ${sparsity}")
  }
}
