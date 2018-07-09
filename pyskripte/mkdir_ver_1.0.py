import os
import shutil
import subprocess
from subprocess import call
import shutil
import time



w = os.getcwd()
scr = w + "/cl_me_km.com"

scr2 = w + "/cl_me_km.job"

data = 0

for n in range(1,7):
      
      ang = 2.7+n*0.1
      
      command = "%s_ang_on_face" %ang
      os.makedirs(command)
      print "Creating Folder %s" %command
      
      dest = w +"/%s_ang_on_face/" %ang
      
      shutil.copy2(scr,dest)
      print "Coping to '%s'." %(dest)
      
      shutil.copy2(scr2,dest)
      print "Coping to '%s'." %(dest)
      
      
      myfile =w +'/%s_ang_on_face/cl_me_km.com' %ang
      
      with open (myfile,'r') as molin:
            data=molin.readlines()
      
      matching = [s for s in data if 'r=' in s]        #Zeilenindizes 
             
      r1 = [i for i, x in enumerate(data) if x ==matching[0]] #Index von zu a"nderender Zeile
      
      
      data[r1[0]] = 'r= %s \n' %ang   #A"nderung der Zeile
#      print(ang)
#      print(data[r1[0]])
      with open(myfile,'w') as molout:
            molout.writelines(data)        #molfile neu u"berschrieben

