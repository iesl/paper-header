#!/bin/bash

MEMORY=15g

data="--file=$PH_ROOT/data/fullpaper-headers-modified.tsv"
outfile="--out-file=$PH_ROOT/processed.tsv"

echo "processing $data ; output to $outfile"

$PH_ROOT/bin/run_class.sh -Xmx$MEMORY edu.umass.cs.iesl.paperheader.process.DocProcessor $data $outfile

