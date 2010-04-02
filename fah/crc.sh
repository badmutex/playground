#!/usr/bin/env bash

NAME=$1

FAHCLIENT='fah6'
ARGS='-betateam -oneunit'
USERNAME='nd_test1_bot'
CFGTEMPLATE='client.cfg.template'
GENCFG='SetClientConfig.py'

[ ! -d $NAME ] && mkdir -v $NAME

machineid=$(( $RANDOM % 16 + 1))
python $GENCFG $CFGTEMPLATE client.cfg $USERNAME $machineid
mv -v client.cfg $NAME

pushd $NAME

../$FAHCLIENT $ARGS

popd

tar czjf $NAME $NAME.tar.bz2
