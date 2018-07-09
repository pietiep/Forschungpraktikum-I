import os
import re
import decimal

paras = 'distanz'
starts = 2
ends =  3
points = 10
descend = True

data = []

def molprofile():
      kind = ".com"
      files_list = [d for d in os.listdir('.') if re.match(kind, d[-len(kind):])] #Molprofile
      global data
      with open(files_list[0],'r') as f:
            data = f.readlines()

molprofile()

def index_f(parameter):
      """Takes *argv = tuple and scanes choosen file after the Elements of the tuple and returns them and their index"""

      matching = [s for s in data if s.startswith(parameter)]                                   # when line s contains one of the tuple's elements startswith is true
      index_m = [[str(s) for s, x in enumerate(data) if x == item] for item in matching]      # gets the index
      return int(index_m[0][0])



def ranges(starts, ends, points, descend):
      diff = ends - starts
      fac = float(diff)/float(points)
      return sorted([round(decimal.Decimal(i * fac + starts),2) for i in
      range(points+1)], reverse=descend)

def change_line(paras, indi, scan):
    print(data[indi])
    data[indi] = paras + ' = ' + str(ranges(starts, ends, points, descend)) + '\n'
    print(data[indi])
    with open ('h2s.com','w') as out:
        out.writelines(data)      

change_line(paras, index_f(paras), ranges(starts, ends, points, descend))

