#!/bin/bash

taggerType="grobid"

root="/home/kate/AI2/clean/paper-header"
trainFile="$root/grobid-data/header.train"
devFile="$root/grobid-data/header.test"
optimizer="adagrad"

saveModel="true"
modelFile="$root/HeaderTagger_grobid.factorie"

mem="5G"
facJar="/home/kate/research/factorie/target/factorie_2.11-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar"
CP="$root/target/paper_header-0.1-SNAPSHOT.jar:$facJar"

java -Xmx$mem -cp $CP edu.umass.cs.iesl.paperheader.model.HeaderTaggerTrainer \
--tagger-type=$taggerType \
--train-file=$trainFile \
--dev-file=$devFile \
--optimizer=$optimizer \
--save-model=$saveModel \
--model-file=$modelFile



