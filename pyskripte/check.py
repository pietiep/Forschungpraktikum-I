import os
import shutil
import subprocess
from shutil import copyfile
from subprocess import call
from itertools import islice
import time
from datetime import datetime

for n in range(1,17280):
      process = subprocess.Popen(["qstat -u protassow"], shell=True)
      t = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
      print "Time:" + str(t)
      time.sleep(5)
      
for n in range(1,17280):
      process = subprocess.Popen(["qstat -u protassow>>log"], shell=True)
      t = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
      print "Time:" + str(t)
      time.sleep(5)


