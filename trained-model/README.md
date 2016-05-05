Trained 18 March 2016 on CORA/GROBID. Get it using:

`wget https://s3.amazonaws.com/iesl-paperheader-models/HeaderTagger.tgz`

Notes:

* The uncompressed model is 144M
* It achieves the following performance on the CORA/GROBID test set:

OVERALL  f1=0.862651 p=0.848341 r=0.877451 (tp=179 fp=32 fn=25 true=204 pred=211) acc=0.963148 (4626/4803)

abstract f1=0.933333 p=0.913043 r=0.954545 (tp=21 fp=2 fn=1 true=22 pred=23)

address  f1=0.812500 p=0.812500 r=0.812500 (tp=26 fp=6 fn=6 true=32 pred=32)

affiliation f1=0.882353 p=0.882353 r=0.882353 (tp=30 fp=4 fn=4 true=34 pred=34)

author   f1=0.916667 p=0.868421 r=0.970588 (tp=33 fp=5 fn=1 true=34 pred=38)

copyright f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)

date     f1=1.000000 p=1.000000 r=1.000000 (tp=7 fp=0 fn=0 true=7 pred=7)

date-submission f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)

dedication f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)

degree   f1=0.000000 p=0.000000 r=1.000000 (tp=0 fp=1 fn=0 true=0 pred=1)

email    f1=0.833333 p=0.869565 r=0.800000 (tp=20 fp=3 fn=5 true=25 pred=23)

entitle  f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)

grant    f1=1.000000 p=1.000000 r=1.000000 (tp=2 fp=0 fn=0 true=2 pred=2)

intro    f1=0.750000 p=0.600000 r=1.000000 (tp=3 fp=2 fn=0 true=3 pred=5)

keyword  f1=0.800000 p=0.666667 r=1.000000 (tp=2 fp=1 fn=0 true=2 pred=3)

note     f1=0.400000 p=0.500000 r=0.333333 (tp=2 fp=2 fn=4 true=6 pred=4)

other    f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)

phone    f1=0.800000 p=1.000000 r=0.666667 (tp=2 fp=0 fn=1 true=3 pred=2)

pubnum   f1=0.857143 p=1.000000 r=0.750000 (tp=3 fp=0 fn=1 true=4 pred=3)

reference f1=0.000000 p=0.000000 r=1.000000 (tp=0 fp=1 fn=0 true=0 pred=1)

submission f1=0.000000 p=0.000000 r=0.000000 (tp=0 fp=1 fn=1 true=1 pred=1)

title    f1=0.943396 p=0.925926 r=0.961538 (tp=25 fp=2 fn=1 true=26 pred=27)

web      f1=0.750000 p=0.600000 r=1.000000 (tp=3 fp=2 fn=0 true=3 pred=5)
