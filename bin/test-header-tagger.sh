#!/bin/bash

echo "environment vars:"
echo "PH_ROOT = $PH_ROOT"
echo "FACJAR = $FACJAR"
#echo "GROBID_DATA = $GROBID_DATA"

l1="--l1=0.8072815676366317"
l2="--l2=0.2797113430916389"
rate="--learning-rate=0.14359866541624697"
delta="--delta=0.0037537188611347044"

grobidData="--data-dir=$PH_ROOT/grobid-test"
fpData="--data=$PH_ROOT/data/fullpaper-headers.tsv"
dataSet="--data-set=fp"
model="--model=$PH_ROOT/HeaderTaggerGrobid.factorie"

CP=$FACJAR:"target/paper_header-0.1-SNAPSHOT.jar"

memory="8g"

java -cp $CP -Xmx$memory "edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTester" $dataSet $grobidData $fpData $model

