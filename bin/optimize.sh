#!/bin/bash

memory=10g
data="--data-dir=/iesl/canvas/ksilvers/data/grobid-data"
saveAs="--save-model=HeaderTaggerGrobid.factorie"
grobid="--grobid=true"

facjar="/iesl/canvas/ksilvers/factorie/target/factorie_2.11-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar"
CP=$facjar:"paper_header-0.1-SNAPSHOT.jar"

java -cp $CP -Xmx$memory "edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerOptimizer" $data $saveAs $grobid