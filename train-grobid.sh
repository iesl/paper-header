#!/bin/bash

taggerType="combined"

root="/home/kate/AI2/clean/paper-header"
trainFile="$root/grobid-data/header.train"
testFile="$root/grobid-data/header.test"
optimizer="adagrad"

saveModel="true"
modelFile="$root/HeaderTagger_combined.factorie"

mem="7G"
facJar="/home/kate/research/factorie/target/factorie_2.11-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar"
CP="$root/target/paper_header-0.1-SNAPSHOT.jar:$facJar"

java -Xmx$mem -cp $CP edu.umass.cs.iesl.paperheader.model.HeaderTaggerTrainer \
--tagger-type=$taggerType \
--train-file=$trainFile \
--test-file=$testFile \
--optimizer=$optimizer \
--save-model=$saveModel \
--model-file=$modelFile



