import os
import shutil
import subprocess
from shutil import copyfile
from subprocess import call
from itertools import islice
import numpy as np

myfile='komplex.tab'

data = np.loadtxt(myfile, skiprows=4)

B= np.zeros((31,4))


for i in range(0,3):
         B[:,i] =  data[:,i] - data[0,i]     #Ziehe erste Element jeder Spalte von den jeweiligen Elemten derselben Spalte ab. 


for i in range(0,3):
      B[:,i] *=27.2107          # 1 Hartree = 27.2107


B[:,3] = data[:,3]

np.savetxt("komplex.txt", B, fmt="%.8f") 
     
