#!/bin/bash

taggerType="default"

root="/home/kate/AI2/clean/paper-header"
trainFile="$root/data/fullpaper-train.tsv"
devFile="$root/data/fullpaper-dev.tsv"
testFile="$root/data/fullpaper-test.tsv"

saveModel="true"
modelFile="$root/HeaderTagger_default.factorie"

mem="5G"
facJar="/home/kate/research/factorie/target/factorie_2.11-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar"
CP="$root/target/paper_header-0.1-SNAPSHOT.jar:$facJar"

java -Xmx$mem -cp $CP edu.umass.cs.iesl.paperheader.model.HeaderTaggerTrainer \
--tagger-type=$taggerType \
--train-file=$trainFile \
--dev-file=$devFile \
--test-file=$testFile \
--save-model=$saveModel \
--model-file=$modelFile


