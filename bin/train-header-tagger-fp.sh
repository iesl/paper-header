#!/bin/bash

echo "environment vars:"
echo "PH_ROOT = $PH_ROOT"
echo "FACJAR = $FACJAR"

rate="--learning-rate=0.1712456329990091"
delta="--delta=0.030480270587152233"

save="--serialize=true"
saveAs="--save-model=HeaderTaggerFP.factorie"

trainDir="--train=$PH_ROOT/data/fullpaper-train.tsv"
devDir="--dev=$PH_ROOT/data/fullpaper-dev.tsv"
testDir="--test=$PH_ROOT/data/fullpaper-test.tsv"
dataSet="--data-set=fp"

CP=$FACJAR:"$PH_ROOT/target/paper_header-0.1-SNAPSHOT.jar"

memory="15g"

java -cp $CP -Xmx$memory "edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTrainer" $trainDir $devDir $testDir $dataSet $save $saveAs $rate $delta
