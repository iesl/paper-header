#!/bin/bash

memSize="10G"

$PH_ROOT/bin/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.paperheader.tagger.TrainHeaderTagger \
--save-model="true" \
--model-file="$PH_ROOT/headerCRF-grobid-feats.factorie" \
--output-dir="$PH_ROOT/results-grobid-feats-train" \
--data-set="grobid" \
--use-grobid-features="true" \
--train-file="$PH_ROOT/grobid-header-train.data" \
--train-portion="1.0" \
--learning-rate=0.26378465383998206 \
--delta=0.18193727392701542 \
--l2=3.9482163864368205E-6 \
--l1=0.016884445594878783
#--learning-rate=0.4310789722667176 \
#--delta=1.5064769136913537E-4 \
#--l2=0.01858534359928488 \
#--l1=1.3177574250747779
