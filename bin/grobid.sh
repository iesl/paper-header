#!/bin/bash

CP="$PH_ROOT/target/paper_header-0.1-SNAPSHOT.jar:$FACJAR"

memory="10g"

groot="/home/kate/eval/grobid/grobid-trainer/resources/dataset/header/corpus/tei"

java -cp $CP -Xmx$memory "edu.umass.cs.iesl.paperheader.load.GrobidMung" $groot
