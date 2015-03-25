#!/bin/bash

train="--train=$PH_ROOT/data/fullpaper-headers.train"
dev="--dev=$PH_ROOT/data/fullpaper-headers.dev"
grobid="--grobid-data=$PH_ROOT/grobid-full"
serialize="--serialize=true"

CP="target/paper_header-0.1-SNAPSHOT-jar-with-dependencies.jar"

#java -cp $CP "edu.umass.cs.iesl.paperheader.crf.HeaderTaggerTrainer" $train $dev
#$PH_ROOT/bin/run_class.sh "edu.umass.cs.iesl.paperheader.crf.HeaderTaggerOptimizer" $train $dev

sbt -mem 15000 "runMain edu.umass.cs.iesl.paperheader.crf.HeaderTaggerOptimizer $train $grobid $dev $serialize"