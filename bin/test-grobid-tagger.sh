#!/bin/bash

#!/bin/bash

echo "environment vars:"
echo "PH_ROOT = $PH_ROOT"
echo "FACJAR = $FACJAR"
#echo "GROBID_DATA = $GROBID_DATA"

rate="--learning-rate=0.1712456329990091"
delta="--delta=0.030480270587152233"

testDir="--test-dir=/home/kate/eval/grobid/grobid-trainer/resources/dataset/header/evaluation/tei"

model="--model=file://$PH_ROOT/GrobidTagger.factorie"

CP=$FACJAR:"target/paper_header-0.1-SNAPSHOT.jar"

memory="8g"

java -cp $CP -Xmx$memory "edu.umass.cs.iesl.paperheader.tagger.GrobidTaggerTester" $model $testDir


#data="--data-dir=$GROBID_DATA"
#save="--serialize=true"
#saveAs="--save-model=HeaderTaggerGrobid.factorie"
#dataSet="--data-set=grobid"
#
#CP=$FACJAR:"$PH_ROOT/target/paper_header-0.1-SNAPSHOT.jar"
#
#memory="20g"
#
#java -cp $CP -Xmx$memory "edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTrainer" $data $dataSet $save $saveAs $l1 $l2 $rate $delta
