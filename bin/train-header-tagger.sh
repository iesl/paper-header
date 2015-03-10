#!/bin/bash

echo "environment vars:"
echo "PH_ROOT = $PH_ROOT"
echo "FACJAR = $FACJAR"
#echo "GROBID_DATA = $GROBID_DATA"

l1="--l1=0.8072815676366317"
l2="--l2=0.2797113430916389"

# as of 10 March:
#OVERALL  f1=0.858660 p=0.869040 r=0.848525 (tp=1294 fp=195 fn=231 true=1525 pred=1489) acc=0.980359 (42377/43226)
#abstract f1=0.978313 p=0.971292 r=0.985437 (tp=203 fp=6 fn=3 true=206 pred=209)
#address  f1=0.767184 p=0.800926 r=0.736170 (tp=173 fp=43 fn=62 true=235 pred=216)
#affiliation f1=0.766488 p=0.781818 r=0.751748 (tp=215 fp=60 fn=71 true=286 pred=275)
#author   f1=0.795411 p=0.825397 r=0.767528 (tp=208 fp=44 fn=63 true=271 pred=252)
#date     f1=0.916667 p=0.916667 r=0.916667 (tp=66 fp=6 fn=6 true=72 pred=72)
#email    f1=0.926667 p=0.914474 r=0.939189 (tp=139 fp=13 fn=9 true=148 pred=152)
#keyword  f1=0.867925 p=0.836364 r=0.901961 (tp=46 fp=9 fn=5 true=51 pred=55)
#phone    f1=0.700000 p=0.736842 r=0.666667 (tp=14 fp=5 fn=7 true=21 pred=19)
#title    f1=0.976445 p=0.966102 r=0.987013 (tp=228 fp=8 fn=3 true=231 pred=236)
#web      f1=0.571429 p=0.666667 r=0.500000 (tp=2 fp=1 fn=2 true=4 pred=3)

rate="--learning-rate=0.1712456329990091"
delta="--delta=0.030480270587152233"

data="--data-dir=$PH_ROOT/grobid-data-clean"
save="--serialize=true"
saveAs="--save-model=HeaderTaggerGrobid.factorie"
dataSet="--data-set=grobid"

CP=$FACJAR:"$PH_ROOT/target/paper_header-0.1-SNAPSHOT.jar"

memory="20g"

java -cp $CP -Xmx$memory "edu.umass.cs.iesl.paperheader.tagger.HeaderTaggerTrainer" $data $dataSet $save $saveAs $l1 $l2 $rate $delta

#--learning-rate=0.1712456329990091
#--model=HeaderTagger.factorie
#--delta=0.030480270587152233
#--data-set=grobid
#--serialize=false
#--l2=0.06765909781125444
#--save-model=HeaderTaggerGrobid.factorie
#--test-portion=1.0
#--train-portion=1.0
#--l1=1.424388380418031E-5
#--data-dir=/iesl/canvas/ksilvers/data/grobid-data
#--use-formatting=false