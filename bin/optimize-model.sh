#!/bin/bash

memSize="2G"

$PH_ROOT/bin/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.paperheader.tagger.OptimizeCitationModel \
--model-file="$PH_ROOT/headerCRF.factorie" \
--output-dir="$PH_ROOT/results-train" \
--data-set="grobid" \
--use-grobid-features="false" \
--train-file="$PH_ROOT/grobid-header-train.data" \
--train-portion="0.8"
