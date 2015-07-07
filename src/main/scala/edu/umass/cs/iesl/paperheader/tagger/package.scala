package edu.umass.cs.iesl.paperheader

/**
 * @author Kate Silverstein 
 *         created on 4/21/15
 */
import cc.factorie.app.nlp.Token

package object tagger {
  implicit class TokenExtras(t: Token) {
    def lemmaStr: String = cc.factorie.app.strings.simplifyDigits(t.string).toLowerCase
  }

  // seed rng with constant for repeatability
  implicit val random = new scala.util.Random(0)
}
