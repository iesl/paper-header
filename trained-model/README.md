Trained 18 March 2016 on CORA/GROBID. Get it using:

`wget https://s3.amazonaws.com/iesl-paperheader-models/HeaderTagger.tgz`

Notes:

* The uncompressed model is 144M
* It achieves the following performance on the CORA/GROBID test set:


OVERALL  f1=0.864734 p=0.852381 r=0.877451 (tp=179 fp=31 fn=25 true=204 pred=210) acc=0.966063 (4640/4803)

abstract f1=0.888889 p=0.869565 r=0.909091 (tp=20 fp=3 fn=2 true=22 pred=23)

address  f1=0.793651 p=0.806452 r=0.781250 (tp=25 fp=6 fn=7 true=32 pred=31)

affiliation f1=0.771429 p=0.750000 r=0.794118 (tp=27 fp=9 fn=7 true=34 pred=36)

author   f1=0.956522 p=0.942857 r=0.970588 (tp=33 fp=2 fn=1 true=34 pred=35)

copyright f1=0.000000 p=0.000000 r=1.000000 (tp=0 fp=2 fn=0 true=0 pred=2)

date     f1=0.857143 p=0.857143 r=0.857143 (tp=6 fp=1 fn=1 true=7 pred=7)

date-submission f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)

dedication f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)

degree   f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)

email    f1=0.979592 p=1.000000 r=0.960000 (tp=24 fp=0 fn=1 true=25 pred=24)

entitle  f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)

grant    f1=0.000000 p=1.000000 r=0.000000 (tp=0 fp=0 fn=2 true=2 pred=0)

intro    f1=0.857143 p=0.750000 r=1.000000 (tp=3 fp=1 fn=0 true=3 pred=4)

keyword  f1=1.000000 p=1.000000 r=1.000000 (tp=2 fp=0 fn=0 true=2 pred=2)

note     f1=0.428571 p=0.375000 r=0.500000 (tp=3 fp=5 fn=3 true=6 pred=8)

phone    f1=1.000000 p=1.000000 r=1.000000 (tp=3 fp=0 fn=0 true=3 pred=3)

pubnum   f1=0.857143 p=1.000000 r=0.750000 (tp=3 fp=0 fn=1 true=4 pred=3)

reference f1=0.000000 p=0.000000 r=1.000000 (tp=0 fp=1 fn=0 true=0 pred=1)

submission f1=1.000000 p=1.000000 r=1.000000 (tp=1 fp=0 fn=0 true=1 pred=1)

title    f1=0.981132 p=0.962963 r=1.000000 (tp=26 fp=1 fn=0 true=26 pred=27)

web      f1=1.000000 p=1.000000 r=1.000000 (tp=3 fp=0 fn=0 true=3 pred=3)
