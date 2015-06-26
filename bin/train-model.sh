#!/bin/bash

memSize="10G"

$PH_ROOT/bin/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.paperheader.tagger.TrainHeaderTagger \
--save-model="true" \
--model-file="$PH_ROOT/headerCRF.factorie" \
--output-dir="$PH_ROOT/results-train" \
--data-set="grobid" \
--use-grobid-features="false" \
--train-file="$PH_ROOT/grobid-header-train.data" \
--train-portion="1.0" \
--learning-rate=0.3782168715643856 \
--delta=0.0208848390678699 \
--l2=0.15237729551609597 \
--l1=0.05038601046083946
