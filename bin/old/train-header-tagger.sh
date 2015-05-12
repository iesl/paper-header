#!/bin/bash

MEMORY=3g

#data="--data=data/fullpaper-headers.tsv"
#save="--serialize=true"

dataDir="--data-dir=$PH_ROOT/grobid-data"
grobid="--grobid=true"
save="--serialize=true"
saveAs="--save-model=HeaderTaggerGrobid.factorie"

l1=`$PH_ROOT/bin/get_config.sh l1`
l2=`$PH_ROOT/bin/get_config.sh l2`
rate=`$PH_ROOT/bin/get_config.sh rate`
delta=`$PH_ROOT/bin/get_config.sh delta`

$PH_ROOT/bin/run_class.sh -Xmx$MEMORY edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTrainer $dataDir $save $saveAs $grobid \
"--l1=$l1" \
"--l2=$l2" \
"--learning-rate=$rate" \
"--delta=$delta"