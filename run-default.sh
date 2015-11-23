#!/bin/bash

taggerType="default"

root="/home/kate/AI2/clean/paper-header"
testFile="$root/data/fullpaper-test.tsv"

modelFile="$root/HeaderTagger_default.factorie"

mem="5G"
facJar="/home/kate/research/factorie/target/factorie_2.11-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar"
CP="$root/target/paper_header-0.1-SNAPSHOT.jar:$facJar"

java -Xmx$mem -cp $CP edu.umass.cs.iesl.paperheader.App \
--tagger-type=$taggerType \
--test-file=$testFile \
--model-file=$modelFile


