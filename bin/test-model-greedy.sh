#!/bin/bash

memSize="10G"

$PH_ROOT/bin/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.paperheader.tagger.TestGreedyHeaderTagger \
--model-file="file://$PH_ROOT/headerGreedy.factorie" \
--output-dir="$PH_ROOT/results_test_greedy" \
--data-set="grobid" \
--use-grobid-features="true" \
--test-file="$PH_ROOT/grobid-header-test.data"
