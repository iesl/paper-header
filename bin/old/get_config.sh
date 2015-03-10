#!/bin/bash

PROPKEY=$1
DEFAULT_VALUE=$2

if [ -z "$PH_CONFIG" ]
then
PH_CONFIG=$PH_ROOT/config/ph.config
fi

conf=`cat $PH_CONFIG \
| awk -v pk=$PROPKEY '$1 == pk { print $2} '`

if [ -z "$conf" ]
then
conf=$DEFAULT_VALUE
fi

eval echo $conf