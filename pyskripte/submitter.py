import os
import shutil
import subprocess
from subprocess import call


for n in range(1,6):
      
      ang=3-n*0.1
      
      dest="/raid/home/protassow/Forschungspraktikum2/chlor_methan_kom/multi/scan/grundzustand/winkel/fullvalence/%s_ang_on_face/" %ang
      
      job = dest+'cl_me_km.job'
      os.chdir(dest)
      process = subprocess.Popen(['qsub %s' %job], shell=True)

for n in range(1,6):
      
      ang=3+n*0.1

      dest="/raid/home/protassow/Forschungspraktikum2/chlor_methan_kom/multi/scan/grundzustand/winkel/fullvalence/%s_ang_on_face/" %ang

      job = dest+'cl_me_km.job'
      os.chdir(dest)
      process = subprocess.Popen(['qsub %s' %job], shell=True)
