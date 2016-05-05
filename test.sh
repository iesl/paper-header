#!/bin/bash

######
# Use this script as a template for testing models. More info on options can be found in
# edu.umass.cs.iesl.paperheader.HeaderTaggerOpts
######

mem="8G"
root="$PWD"

# set these to the appropriate paths
testFile="$root/data/header.test"

# where to serialize the model
modelFile="$root/trained-model/HeaderTagger.factorie"

# valid data types: grobid|iesl (see data/README.md for info on how to obtain the data)
dataType="grobid"

# valid tagger types: default|grobid|combined
taggerType="default"

### set up the classpath ###
# factorieJar should equal /path/to/factorie/target/factorie_{VERSION}-nlp-jar-with-dependencies.jar
factorieJar=""
CP="$factorieJar:$root/target/paper_header-0.1-SNAPSHOT.jar"

java -Xmx$mem -cp $CP edu.umass.cs.iesl.paperheader.HeaderTaggerRunner \
--test-file=$testFile \
--tagger-type=$taggerType \
--data-type=$dataType \
--model-file=$modelFile
