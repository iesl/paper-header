#!/bin/bash

MEMORY=15g

model="--model=HeaderTagger.factorie"

$PH_ROOT/bin/run_class.sh -Xmx$MEMORY edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTester $model
