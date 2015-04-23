#!/bin/bash

echo "environment vars:"
echo "PH_ROOT = $PH_ROOT"
echo "FACJAR = $FACJAR"

CP=$FACJAR:"$PH_ROOT/target/paper_header-0.1-SNAPSHOT.jar"

memory="15g"

train="--train=$PH_ROOT/data/fullpaper-headers.tsv"

java -cp $CP -Xmx$memory "edu.umass.cs.iesl.paperheader.tagger.OneTimeThing" $train
