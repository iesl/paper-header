#!/bin/bash

test="--test=$PH_ROOT/data/fullpaper-headers.test"
model="--save-model=$PH_ROOT/HeaderTagger.factorie"

sbt -mem 15000 "runMain edu.umass.cs.iesl.paperheader.crf.HeaderTaggerTester $test $model"

#$PH_ROOT/bin/run_class.sh "edu.umass.cs.iesl.paperheader.crf.HeaderTaggerTester" $test $model
