#!/bin/sh
echo $( $(true) ) # not bag…
pwd=$(echo $(pwd)) #bag… can't put space here

echo $( [[ -n $1 ]] && echo Enable args || echo Not args)
echo $( (($#)) && echo Enable args || echo Not args)
echo $[9-$[4*$[1>0]]]
