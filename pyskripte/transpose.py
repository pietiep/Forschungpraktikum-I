import os
import shutil
import subprocess
from shutil import copyfile
from subprocess import call
from itertools import islice

myfileold="energy.txt"

myfilenew="bla2.txt"

with open(myfileold,'r') as in_f, open(myfilenew, 'w') as out_file:
      for line in ([i.rstrip()] + map(str.rstrip, islice(in_f,3)) for i in in_f):
            out_file.write("\t".join(line)+ "\n")


