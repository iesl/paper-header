#!/bin/bash

echo "environment vars:"
echo "PH_ROOT = $PH_ROOT"
echo "FACJAR = $FACJAR"
#echo "GROBID_DATA = $GROBID_DATA"

grobidData="--data-dir=/iesl/canvas/ksilvers/data/grobid-data"
fpData="--data=$PH_ROOT/data/fullpaper-headers.tsv"
dataset="--data-set=all"
save="--serialize=true"
saveAs="--save-model=HeaderTaggerAll.factorie"

CP=$FACJAR:"paper_header-0.1-SNAPSHOT.jar"

memory="16g"

l1="--l1=0.8072815676366317"
l2="--l2=0.2797113430916389"
rate="--learning-rate=0.14359866541624697"
delta="--delta=0.1"

java -cp $CP -Xmx$memory "edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTrainer" $grobidData $fpData $dataset $save $saveAs $l1 $l2 $rate $delta