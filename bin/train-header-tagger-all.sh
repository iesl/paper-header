#!/bin/bash

echo "environment vars:"
echo "PH_ROOT = $PH_ROOT"
echo "FACJAR = $FACJAR"

rate="--learning-rate=0.1712456329990091"
delta="--delta=0.030480270587152233"

save="--serialize=true"
saveAs="--save-model=HeaderTaggerAll.factorie"

# fullpaper-headers
train="--train=$PH_ROOT/data/fullpaper-train.tsv"
dev="--dev=$PH_ROOT/data/fullpaper-dev.tsv"
test="--test=$PH_ROOT/data/fullpaper-test.tsv"
# grobid
trainDir="--train-dir=$PH_ROOT/grobid-data/train"
devDir="--dev-dir=$PH_ROOT/grobid-data/dev"
testDir="--test-dir=$PH_ROOT/grobid-data/test"
dataSet="--data-set=all"

CP=$FACJAR:"$PH_ROOT/target/paper_header-0.1-SNAPSHOT.jar"

memory="15g"

java -cp $CP -Xmx$memory "edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTrainer" $trainDir $devDir $testDir $train $test $dev $dataSet $save $saveAs $rate $delta
