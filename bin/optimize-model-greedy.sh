#!/bin/bash

memSize="2G"

$PH_ROOT/bin/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.paperheader.tagger.OptimizeGreedyCitationModel \
--model-file="$PH_ROOT/headerCRF-greedy-optimize.factorie" \
--output-dir="$PH_ROOT/results-optimize-greedy" \
--data-set="grobid" \
--use-grobid-features="true" \
--train-file="$PH_ROOT/grobid-header-train.data" \
--test-file="$PH_ROOT/grobid-header-test.data" \
--train-portion="1.0"
