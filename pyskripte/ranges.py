import os
import shutil
import subprocess
from shutil import copyfile
from subprocess import call
from itertools import islice

myfile='rang.txt'
r=[]

for i in range(40):
      r.append(0)

for i in range(0,11):
      r[i]=-15+i

for i in range(1,11):
      r[10+i]=-5+i*0.25

for i in range(1,11):
      r[20+i]=-2.5+i*0.05

with open(myfile, 'w') as out_file:
      out_file.write(str(r))
