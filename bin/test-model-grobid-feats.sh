#!/bin/bash

memSize="10G"

$PH_ROOT/bin/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.paperheader.tagger.TestHeaderTagger \
--model-file="file://$PH_ROOT/headerCRF-grobid-feats.factorie" \
--output-dir="$PH_ROOT/results_grobid_feats_test" \
--data-set="grobid" \
--use-grobid-features="true" \
--test-file="/iesl/canvas/ksilvers/data/grobid/testfile.data"
