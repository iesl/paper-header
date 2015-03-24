#!/bin/bash

data="data/fullpaper-headers.tsv"

$PH_ROOT/bin/run_class.sh "edu.umass.cs.iesl.paperheader.utils.Mung" $data
