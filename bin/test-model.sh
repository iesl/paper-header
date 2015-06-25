#!/bin/bash

memSize="10G"

$PH_ROOT/bin/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.paperheader.tagger.TestHeaderTagger \
--model-file="file://$PH_ROOT/headerCRF.factorie" \
--output-dir="$PH_ROOT/results_test" \
--data-set="grobid" \
--use-grobid-features="false" \
--test-file="$PH_ROOT/grobid-header-test.data"
