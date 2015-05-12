#!/bin/bash

gjar="/home/kate/eval/grobid/grobid-core/target/grobid-core-0.3.3-SNAPSHOT.one-jar.jar"
ghome="-gH /home/kate/eval/grobid/grobid-home"
gp="-gP /home/kate/eval/grobid/grobid-home/config/grobid.properties"
input="-dIn $PH_ROOT/grobid-pdf"
output="-dOut $PH_ROOT/grobid-extr"

java -Xmx5g -jar $gjar $ghome $gp $input $output -exe processHeader