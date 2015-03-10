#!/bin/bash

echo "environment vars:"
echo "PH_ROOT = $PH_ROOT"
echo "FACJAR = $FACJAR"
#echo "GROBID_DATA = $GROBID_DATA"

l1="--l1=0.8072815676366317"
l2="--l2=0.2797113430916389"
rate="--learning-rate=0.14359866541624697"
delta="--delta=0.1"

data="--data-dir=$PH_ROOT/grobid-data-clean"
save="--serialize=true"
saveAs="--save-model=HeaderTaggerGrobid.factorie"
dataSet="--data-set=grobid"

CP=$FACJAR:"$PH_ROOT/target/paper_header-0.1-SNAPSHOT.jar"

memory="20g"

java -cp $CP -Xmx$memory "edu.umass.cs.iesl.paperheader.tagger.SimpleHeaderTrainer" $data $dataSet


