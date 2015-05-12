#!/bin/bash

#!/bin/bash

echo "environment vars:"
echo "PH_ROOT = $PH_ROOT"
echo "FACJAR = $FACJAR"

rate="--learning-rate=0.1712456329990091"
delta="--delta=0.030480270587152233"

htModel="--header-tagger-model=$PH_ROOT/HeaderTaggerGrobid.factorie"
data="--grobid-data-dir=$PH_ROOT/grobid-data-clean"

CP=$FACJAR:"$PH_ROOT/target/paper_header-0.1-SNAPSHOT.jar"

memory="20g"

java -cp $CP -Xmx$memory "edu.umass.cs.iesl.paperheader.tagger.AuthorCRFTrainer" $data $htModel $rate $delta

