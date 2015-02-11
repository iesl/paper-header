#!/bin/bash

MEMORY=20g

data=$PH_ROOT/data/fullpaper-headers-modified.tsv
train="--train=$data"
ner="--ner-model=$PH_ROOT/model/ConllChainNer.factorie"

$PH_ROOT/bin/run_class.sh -Xmx$MEMORY edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerOptimizer $train
