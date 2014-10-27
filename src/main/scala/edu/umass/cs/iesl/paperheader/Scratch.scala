package edu.umass.cs.iesl.paperheader

import cc.factorie.util.ClasspathURL
import scala.io.Source
import edu.umass.cs.iesl.paperheader.crf._

/**
 * Created by kate on 10/22/14.
 */

//object HeaderTaggerCRF extends HeaderTagger(url=ClasspathURL[HeaderTagger](".factorie")){

object Scratch {
  def main(args: Array[String]): Unit = {
    println("hi from scratch")

    val t = HeaderTaggerCRF

//    val t = new HeaderTaggerCRF2()
//    println(t.getClass().getName())
//    println(t.getClass().getResource("/crf/HeaderTagger.factorie").toString())


//    t.printURL()
    //Source.fromURL(getClass.getResource("bibtex-lexicons/lexicon_note"))
//    val t = new HeaderTagger()
//    println("instantiated HeaderTagger")
//    val thing = t.getClass.getResource("/dummy.txt")
//    if (thing eq null) println("could not find resource")
//    else {
//      println("resource found")
//      println(thing.toString())
//      Source.fromURL(thing).getLines().foreach(println)
//    }

//    if (thing != null) Source.fromURL(t.getClass().getResource("dummy.txt")).getLines().foreach(println)
//    else println("resource not found")

//    println(u.toString())
  }
}
