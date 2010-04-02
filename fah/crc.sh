#!/usr/bin/env bash

NAME=$1

FAHCLIENT='./fah6'
ARGS='-betateam'
USERNAME='nd_bot'
CFGTEMPLATE='client.cfg.template'
GENCFG='SetClientConfig.py'

machineid=$(( $RANDOM % 16 + 1))
python $GENCFG $USERNAME $machineid

$FAHCLIENT $ARGS

tar czjf * $NAME.tar.bz2
