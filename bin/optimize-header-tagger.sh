#!/bin/bash

MEMORY=20g

data=$PH_ROOT/data/fullpaper-headers-modified.tsv
train="--train=$data"
ner="--ner-model=$PH_ROOT/model/ConllChainNer.factorie"

l1val=`$PH_ROOT/bin/get_config.sh l1`
l2val=`$PH_ROOT/bin/get_config.sh l2`
rateval=`$PH_ROOT/bin/get_config.sh rate`

$PH_ROOT/bin/run_class.sh -Xmx$MEMORY edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerOptimizer $train $ner \
"--l1=$l1val" \
"--l2=$l2val" \
"--learning-rate=$rateval"