#!/bin/bash

name=molpromaker

for ((i=5; i<13; i++))

do

rm -f abstand$i.com

cat abstand_8ang.com | head -83l >>abstand$i.com 

echo  Abstand = `bc <<< "scale=10; 7.8 + 0.1 * $i" ` >>abstand$i.com

cat abstand_8ang.com | tail -20l >>abstand$i.com

echo put,molden,abstand`bc <<< "scale=10; 7.8 + 0.1 * $i" `.mol >>abstand$i.com

done



for ((i=13; i<22; i++))

do

rm -f abstand$i.com

cat abstand_8ang.com | head -83l >>abstand$i.com

echo  Abstand = `bc <<< "scale=10; 3 + 0.5 * $i" ` >>abstand$i.com

cat abstand_8ang.com | tail -20l >>abstand$i.com

echo put,molden,abstand`bc <<< "scale=10; 3 + 0.5 * $i" `.mol >>abstand$i.com

done

