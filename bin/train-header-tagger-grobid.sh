#!/bin/bash

echo "environment vars:"
echo "PH_ROOT = $PH_ROOT"
echo "FACJAR = $FACJAR"

rate="--learning-rate=0.1712456329990091"
delta="--delta=0.030480270587152233"

save="--serialize=true"
saveAs="--save-model=HeaderTaggerGrobid.factorie"

trainDir="--train-dir=$PH_ROOT/grobid-data/train"
devDir="--dev-dir=$PH_ROOT/grobid-data/dev"
testDir="--test-dir=$PH_ROOT/grobid-data/test"
dataSet="--data-set=grobid"

CP=$FACJAR:"$PH_ROOT/target/paper_header-0.1-SNAPSHOT.jar"

memory="15g"

java -cp $CP -Xmx$memory "edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTrainer" $trainDir $devDir $testDir $dataSet $save $saveAs $rate $delta
