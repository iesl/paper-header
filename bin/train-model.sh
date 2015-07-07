#!/bin/bash

memSize="10G"

$PH_ROOT/bin/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.paperheader.tagger.TrainHeaderTagger \
--save-model="true" \
--model-file="$PH_ROOT/headerCRF.factorie" \
--output-dir="$PH_ROOT/results-train" \
--data-set="grobid" \
--use-grobid-features="false" \
--train-file="$PH_ROOT/grobid-header-train.data" \
--test-file="$PH_ROOT/grobid-header-test.data" \
--train-portion="0.1" \
--test-portion="0.1" \
--num-iterations=1 \
--learning-rate=0.26378465383998206 \
--delta=0.18193727392701542 \
--l1=0.016884445594878783 \
--l2=3.9482163864368205E-6
#--learning-rate=0.3782168715643856 \
#--delta=0.0208848390678699 \
#--l2=0.15237729551609597 \
#--l1=0.05038601046083946
