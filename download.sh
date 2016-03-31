#!/bin/bash

root="$PWD"
echo $root
cd $root/data
wget https://s3.amazonaws.com/iesl-paperheader-data/grobid-header-data.tgz
tar xzvf grobid-header-data.tgz
cd -
cd $root/trained-model
wget https://s3.amazonaws.com/iesl-paperheader-models/HeaderTagger.tgz
tar xzvf HeaderTagger.tgz
wget https://s3.amazonaws.com/iesl-paperheader-models/brownBllipClusters.tgz
tar xzvf brownBllipClusters.tgz
echo "done"
