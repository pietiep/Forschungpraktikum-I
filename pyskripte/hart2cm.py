import os
import shutil
import subprocess
from shutil import copyfile
from subprocess import call
from itertools import islice
import numpy as np

myfile='komplex.tab'

data = np.loadtxt(myfile, skiprows=4)

datama = np.asmatrix(data)

first = data[0:1]

first2 = first.reshape(4,1)




for i in range(0,3):
      data[1:,i] -= first2[i]

for i in range(0,3):
      data[0:,i] -= first2[i]

for i in range(0,3):
      data[:,i] *=219474.63

np.savetxt("komplex.txt", data, fmt="%.8f") 
     
