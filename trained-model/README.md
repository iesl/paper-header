Download a pre-trained model (trained on 2 February 2016) from Amazon S3:

`wget https://s3.amazonaws.com/iesl-paper-header/HeaderTagger.tgz`


Notes:

* The uncompressed model is 144M
* The model was trained on 2 February 2016
* It achieves the following performance on the CORA test set:

OVERALL  f1=0.946078 p=0.946078 r=0.946078 (tp=193 fp=11 fn=11 true=204 pred=204) acc=0.988549 (4748/4803)
abstract f1=1.000000 p=1.000000 r=1.000000 (tp=22 fp=0 fn=0 true=22 pred=22)
address  f1=0.906250 p=0.906250 r=0.906250 (tp=29 fp=3 fn=3 true=32 pred=32)
affiliation f1=0.941176 p=0.941176 r=0.941176 (tp=32 fp=2 fn=2 true=34 pred=34)
author   f1=0.970588 p=0.970588 r=0.970588 (tp=33 fp=1 fn=1 true=34 pred=34)
copyright f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)
date     f1=1.000000 p=1.000000 r=1.000000 (tp=7 fp=0 fn=0 true=7 pred=7)
date-submission f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)
dedication f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)
degree   f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)
email    f1=0.979592 p=1.000000 r=0.960000 (tp=24 fp=0 fn=1 true=25 pred=24)
entitle  f1=1.000000 p=1.000000 r=1.000000 (tp=0 fp=0 fn=0 true=0 pred=0)
grant    f1=0.500000 p=0.500000 r=0.500000 (tp=1 fp=1 fn=1 true=2 pred=2)
intro    f1=1.000000 p=1.000000 r=1.000000 (tp=3 fp=0 fn=0 true=3 pred=3)
keyword  f1=1.000000 p=1.000000 r=1.000000 (tp=2 fp=0 fn=0 true=2 pred=2)
note     f1=0.615385 p=0.571429 r=0.666667 (tp=4 fp=3 fn=2 true=6 pred=7)
phone    f1=1.000000 p=1.000000 r=1.000000 (tp=3 fp=0 fn=0 true=3 pred=3)
pubnum   f1=0.857143 p=1.000000 r=0.750000 (tp=3 fp=0 fn=1 true=4 pred=3)
reference f1=0.000000 p=0.000000 r=1.000000 (tp=0 fp=1 fn=0 true=0 pred=1)
submission f1=1.000000 p=1.000000 r=1.000000 (tp=1 fp=0 fn=0 true=1 pred=1)
title    f1=1.000000 p=1.000000 r=1.000000 (tp=26 fp=0 fn=0 true=26 pred=26)
web      f1=1.000000 p=1.000000 r=1.000000 (tp=3 fp=0 fn=0 true=3 pred=3)