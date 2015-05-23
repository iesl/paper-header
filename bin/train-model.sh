#!/bin/bash

memSize="10G"

CP=$PHROOT/target/paper_header-0.1-SNAPSHOT.jar:/home/kate/.m2/repository/cc/factorie/factorie_2.11/1.2-SNAPSHOT/factorie_2.11-1.2-SNAPSHOT.jar:/home/kate/.m2/repository/cc/factorie/app/nlp/all-models/1.0.0/all-models-1.0.0.jar:/home/kate/.m2/repository/com/fasterxml/jackson/core/jackson-annotations/2.3.0/jackson-annotations-2.3.0.jar:/home/kate/.m2/repository/com/fasterxml/jackson/core/jackson-core/2.3.1/jackson-core-2.3.1.jar:/home/kate/.m2/repository/com/fasterxml/jackson/core/jackson-databind/2.3.1/jackson-databind-2.3.1.jar:/home/kate/.m2/repository/com/thoughtworks/paranamer/paranamer/2.6/paranamer-2.6.jar:/home/kate/.m2/repository/commons-codec/commons-codec/1.2/commons-codec-1.2.jar:/home/kate/.m2/repository/commons-httpclient/commons-httpclient/3.1/commons-httpclient-3.1.jar:/home/kate/.m2/repository/commons-lang/commons-lang/2.6/commons-lang-2.6.jar:/home/kate/.m2/repository/commons-logging/commons-logging/1.0.4/commons-logging-1.0.4.jar:/home/kate/.m2/repository/info/bliki/wiki/bliki-core/3.0.19/bliki-core-3.0.19.jar:/home/kate/.m2/repository/org/apache/commons/commons-compress/1.8/commons-compress-1.8.jar:/home/kate/.m2/repository/org/jblas/jblas/1.2.3/jblas-1.2.3.jar:/home/kate/.m2/repository/org/json4s/json4s-ast_2.11/3.2.9/json4s-ast_2.11-3.2.9.jar:/home/kate/.m2/repository/org/json4s/json4s-core_2.11/3.2.9/json4s-core_2.11-3.2.9.jar:/home/kate/.m2/repository/org/json4s/json4s-jackson_2.11/3.2.9/json4s-jackson_2.11-3.2.9.jar:/home/kate/.m2/repository/org/jsoup/jsoup/1.8.2/jsoup-1.8.2.jar:/home/kate/.m2/repository/org/mongodb/mongo-java-driver/2.11.1/mongo-java-driver-2.11.1.jar:/home/kate/.m2/repository/org/scala-lang/scala-compiler/2.11.2/scala-compiler-2.11.2.jar:/home/kate/.m2/repository/org/scala-lang/scala-library/2.11.2/scala-library-2.11.2.jar:/home/kate/.m2/repository/org/scala-lang/scala-reflect/2.11.2/scala-reflect-2.11.2.jar:/home/kate/.m2/repository/org/scala-lang/scalap/2.11.0/scalap-2.11.0.jar:/home/kate/.m2/repository/org/scala-lang/modules/scala-parser-combinators_2.11/1.0.2/scala-parser-combinators_2.11-1.0.2.jar:/home/kate/.m2/repository/org/scala-lang/modules/scala-xml_2.11/1.0.2/scala-xml_2.11-1.0.2.jar:/home/kate/.m2/repository/org/scalatest/scalatest_2.11/2.2.1/scalatest_2.11-2.2.1.jar:/home/kate/.m2/repository/org/tukaani/xz/1.5/xz-1.5.jar:/home/kate/.m2/repository/org/xerial/snappy/snappy-java/1.1.1.3/snappy-java-1.1.1.3.jar


#CP="$PHROOT/target/paper_header-0.1-SNAPSHOT-jar-with-dependencies.jar"


java -Xmx${memSize} -cp $CP edu.umass.cs.iesl.paperheader.tagger.TrainHeaderTagger \
--save-model="true" \
--model-file="$PHROOT/headerCRF.factorie" \
--output-dir="$PHROOT/results" \
--data-set="grobid" \
--use-grobid-features="true" \
--train-file="$PHROOT/grobid/trainfile.data" \
--train-portion="0.8"
