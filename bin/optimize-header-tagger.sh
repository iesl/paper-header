#!/bin/bash

MEMORY=20g

data=$PH_ROOT/data/fullpaper-headers.tsv
train="--train=$data"

$PH_ROOT/bin/run_class.sh -Xmx$MEMORY edu.umass.cs.iesl.paperheader.crf.HeaderTaggerOptimizer $train
