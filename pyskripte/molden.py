import os
import glob
import subprocess
import shutil
import re


def file_gr():
      file_list = [d for d in glob.glob('*.mol')]
      return sorted(file_list)

def sort_human(l):
    convert = lambda text: float(text) if text.isdigit() else text
    alphanum = lambda key: [ convert(c) for c in
    re.split('([-+]?[0-9]*\.?[0-9]*)', key) ]
    l.sort( key=alphanum )
    return l


counter = 1

for files in sort_human(file_gr()):
    scr = files 
    dest = "komplex%s.mol" % counter
    counter += 1
    shutil.copy2(scr,dest)
    print "Coping '%s' to '%s'." %(scr,dest)
