#!/bin/bash

name=grepper

for ((i=0; i<22; i++))

do

grep 'Abstand =' abstand$i.out >> ergeb
grep -A 3 'TOTAL ENERGIES' abstand$i.out >> ergeb

done
