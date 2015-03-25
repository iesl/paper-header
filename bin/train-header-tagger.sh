#!/bin/bash

train="--train=$PH_ROOT/data/fullpaper-headers.train"
grobid="--grobid-data=$PH_ROOT/grobid-full"
dev="--dev=$PH_ROOT/data/fullpaper-headers.dev"
serialize="--serialize=true"
#
#CP="target/paper_header-0.1-SNAPSHOT-jar-with-dependencies.jar"
#
#$PH_ROOT/bin/run_class.sh "edu.umass.cs.iesl.paperheader.crf.HeaderTaggerTrainer" $train $grobid $dev $serialize

sbt -mem 15000 "runMain edu.umass.cs.iesl.paperheader.crf.HeaderTaggerTrainer $train $grobid $dev $serialize"