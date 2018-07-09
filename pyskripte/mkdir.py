import os
import shutil
import subprocess
from subprocess import call
import shutil
import time

scr="/raid/home/protassow/Forschungspraktikum2/chlor_methan_kom/multi/scan/grundzustand/winkel/fullvalence/3ang_on_face/cl_me_km.com"

scr2="/raid/home/protassow/Forschungspraktikum2/chlor_methan_kom/multi/scan/grundzustand/winkel/fullvalence/3ang_on_face/cl_me_km.job"

for n in range(1,6):

      ang=3-n*0.1

      command="%s_ang_on_face" %ang
      os.makedirs(command)
      print "Creating Folder %s" %command

      dest="/raid/home/protassow/Forschungspraktikum2/chlor_methan_kom/multi/scan/grundzustand/winkel/fullvalence/%s_ang_on_face/" %ang
     
      shutil.copy2(scr,dest)
      print "Coping '%s' to '%s'." %(scr,dest)

      shutil.copy2(scr2,dest)
      print "Coping '%s' to '%s'." %(scr2,dest)

    
      myfile=dest+'cl_me_km.com'

      with open (myfile,'r') as molin:
            data=molin.readlines()

      matching = [s for s in data if 'r=3' in s]        #Zeilenindizes 
             
      r1 = [i for i, x in enumerate(data) if x ==matching[0]] #Index von zu a"nderender Zeile

      data[r1[0]] = 'r=%s \n' %ang  #A"nderung der Zeile

      with open(myfile,'w') as molout:
            molout.writelines(data)        #molfile neu u"berschrieben


for n in range(1,6):

      ang=3+n*0.1

      command="%s_ang_on_face" %ang
      os.makedirs(command)
      print "Creating Folder %s" %command

      dest="/raid/home/protassow/Forschungspraktikum2/chlor_methan_kom/multi/scan/grundzustand/winkel/fullvalence/%s_ang_on_face/" %ang
     
      shutil.copy2(scr,dest)
      print "Coping '%s' to '%s'." %(scr,dest)

      shutil.copy2(scr2,dest)
      print "Coping '%s' to '%s'." %(scr2,dest)

    
      myfile=dest+'cl_me_km.com'

      with open (myfile,'r') as molin:
            data=molin.readlines()

      matching = [s for s in data if 'r=3' in s]
             
      r1 = [i for i, x in enumerate(data) if x ==matching[0]]

      data[r1[0]] = 'r=%s \n' %ang

      with open(myfile,'w') as molout:
            molout.writelines(data)



