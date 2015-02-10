#!/bin/bash

MEMORY=15g

$PH_ROOT/bin/run_class.sh -Xmx$MEMORY edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTester
