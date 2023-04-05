#!/bin/sh

ssh-keygen -f ${PWD}/bfabricShiny.key -t rsa -m PEM -b 8192 
echo $?
sleep 2
ssh-keygen -f ${PWD}/bfabricShiny.key.pub -e -m PEM > bfabricShiny.key.pub.pem
echo $?
sleep 2


## test keys
R -q -e "PKI::PKI.load.key(file = 'bfabricShiny.key.pub.pem')"
R -q -e "PKI::PKI.load.key(file = 'bfabricShiny.key')"
