#!/bin/bash

memSize="10G"

$PH_ROOT/bin/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.paperheader.tagger.TrainGreedyHeaderTagger \
--save-model="true" \
--model-file="$PH_ROOT/headerGreedy.factorie" \
--output-dir="$PH_ROOT/results-train-greedy" \
--data-set="grobid" \
--use-grobid-features="true" \
--train-file="$PH_ROOT/grobid-header-train.data" \
--test-file="$PH_ROOT/grobid-header-test.data" \
--train-portion="1.0" \
--learning-rate=0.09219901498820374 \
--delta=0.32016219985580924 \
--l2=9.999720032042075E-4 \
--l1=0.7588351657253833
