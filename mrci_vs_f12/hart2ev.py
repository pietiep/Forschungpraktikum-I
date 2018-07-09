import os
import shutil
import subprocess
from shutil import copyfile
from subprocess import call
from itertools import islice
import numpy as np

myfile='both.tab'

data = np.loadtxt(myfile, skiprows=4)

B = np.zeros((31,7))


for i in range(0,6):
      B[:,i] = data[:,i] - data[0,0]

for i in range(0,6):
      B[:,i] *=27.2107

B[:,6] = data[:,6]

np.savetxt("komplex.txt", B, fmt="%.8f") 
     
