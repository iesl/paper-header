#!/bin/bash

train="--train=$PH_ROOT/data/fullpaper-headers.train"
dev="--dev=$PH_ROOT/data/fullpaper-headers.dev"

CP="target/paper_header-0.1-SNAPSHOT-jar-with-dependencies.jar"

$PH_ROOT/bin/run_class.sh "edu.umass.cs.iesl.paperheader.crf.HeaderTaggerOptimizer" $train $dev