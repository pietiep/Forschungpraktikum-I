import os
import shutil
import subprocess
from shutil import copyfile
from subprocess import call
from itertools import islice


molprofile="cl_me_km.com"

with open(molprofile,'r') as molin:
      data=molin.readlines()


matching = [s for s in data if "angle1 =" in s]         #grept mir die richtige Zeile raus


r1 = [i for i, x in enumerate(data) if x == matching[0]]   #list comprehension sucht index

matching = [s for s in data if "angle2 =" in s]


r2 = [i for i, x in enumerate(data) if x == matching[0]]   #list comprehension sucht index



theta=[]
phi=[]



for i in range(20):
      theta.append(0)

for i in range(0,10):
      theta[i]=180+i*10

clean_theta=str(filter(None, theta))     #entfernt die Nullelemente



for i in range(20):
      phi.append(0)

for i in range(0,10):
      phi[i]=70.5+i*10

clean_phi=str(filter(None, phi))      #entfernt die Nullelemente




data[int(r1[0])]="angle1 = %s \n" %clean_phi
data[int(r2[0])]="angle2 = %s \n" %clean_theta


with open(molprofile,'w') as molout:
      molout.writelines(data)
