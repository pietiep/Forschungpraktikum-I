#!/bin/bash

dateiname=greper

for ((i=5; i<22; i++))
do
      qsub abstand$i.job
done



