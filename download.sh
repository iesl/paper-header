#!/bin/bash

root="$PWD"
echo $root
cd $root/data
wget https://s3.amazonaws.com/grobid-header/grobid-header.tgz
tar xzvf grobid-header.tgz
cd -
cd $root/trained-model
wget https://s3.amazonaws.com/grobid-header/HeaderTagger.tgz
tar xzvf HeaderTagger.tgz
echo "done"
