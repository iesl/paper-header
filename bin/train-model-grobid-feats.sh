#!/bin/bash

memSize="10G"

$PH_ROOT/bin/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.paperheader.tagger.TrainHeaderTagger \
--save-model="true" \
--model-file="$PH_ROOT/headerCRF-grobid-feats.factorie" \
--output-dir="$PH_ROOT/results-grobid-feats-train" \
--data-set="grobid" \
--use-grobid-features="true" \
--train-file="/iesl/canvas/ksilvers/data/grobid/trainfile.data" \
--train-portion="0.8"
