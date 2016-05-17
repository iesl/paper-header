#!/usr/bin/env bash

root="$PWD"
infile="$root/output.svg"

mem="8G"
factorieJar="/home/kate/research/factorie/target/factorie_2.11-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar"
CP="$factorieJar:$root/target/paper_header-0.1-SNAPSHOT.jar"

java -Xmx$mem -cp $CP edu.umass.cs.iesl.paperheader.load.RunWatr $infile
