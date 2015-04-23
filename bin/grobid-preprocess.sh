#!/bin/bash

tsvDir="$PH_ROOT/grobid-data/test"
boutDir="$PH_ROOT/grobid-bare"
outdir="$PH_ROOT/grobid-pdf"

CP="$PH_ROOT/target/paper_header-0.1-SNAPSHOT.jar:$FACJAR"

java -cp $CP -Xmx5g "edu.umass.cs.iesl.paperheader.load.GrobidProcess" $tsvDir $boutDir $outdir