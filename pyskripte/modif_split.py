import numpy as np
from decimal import *
import os
import sys

w = os.getcwd()

myfile = w+ '/komplex.tab'

print(myfile)
data = np.loadtxt('komplex.tab', skiprows=4)

a=data.shape
B=np.zeros(a)


target = 'phi_fest'

phi=[]
for i in range(50):
      phi.append(0)

for i in range(0,40):
      phi[i]=round(Decimal(50.5+i*3.2),10)

clean_phi=str(filter(None, phi))      #entfernt die Nullelemente

try:
      os.makedirs(target)
except OSError as err:
      print("OS Error: {0}".format(err))
      print("Nevertheless start spliting and coping Data")
for i in range(0,40):
      B = data[i*40:(i+1)*40, :]
 

      myoutfile= target+'/phi_%s.txt' %phi[i]

      np.savetxt(myoutfile,B, fmt="%.8f")
