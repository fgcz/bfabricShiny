#!/bin/sh

rm -v *.key*
ssh-keygen -f ${PWD}/bfabricShiny.key -t rsa -m PEM -b 8192 
echo $?
ssh-keygen -f ${PWD}/bfabricShiny.key.pub -e -m PEM > bfabricShiny.key.pub.pem
echo $?


## test keys
R -q -e "PKI::PKI.load.key(file = 'bfabricShiny.key.pub.pem')"
R -q -e "PKI::PKI.load.key(file = 'bfabricShiny.key')"
