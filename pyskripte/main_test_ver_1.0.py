import numpy as np
from blank import * 
#from blank import counter
#from blank import blanki
#from blank import getpand
#from blank import splitter
#from blank import plotter_Ani
import sys

path1 = "komplex.tab"
path2 = "komplex_3d.tab"
winkel = "PHI_W"
winkel2 ="THETA_W"
control = 0
#steuerung = input("Please choose one of the following options: 1 for insert a blank line after each Dataset for 3D plots in Gnuplot, 2 for split your files into separate files and save them in a newly created folder. \n")
#if steuerung == 2:
#      control = input("Please choose one of the following options: 1 for freezing " + winkel + ", 2 for freezing " + winkel2 + ".\n")

steuerung = 2
control = 2

with open(path1, 'r') as f:
      data = f.readlines()
data_np = np.loadtxt(path1, skiprows=4)

data_conv = hart2cm(data_np)
print(data_conv)

dim_s = counter(data, path1, winkel)        # Gets the number of the same AND differen values for an angle

if steuerung == 1:
      blanki(data, path2, dim_s)            # Inserts blank line
      print("Blanki is executed.")

elif steuerung == 2:
      if control == 1:
            angle = winkel                   #fest
            angle2 = winkel2                 # nicht fest
      elif control == 2:
            angle = winkel2
            angle2 = winkel
      df = getpand(data, data_conv)
      df_np = np.array(df[angle])
      plot_para = list(splitter(df, angle ,dim_s, angle2)) # Splits Data to separate files
      plot_para.append(angle2)                     # angle is freezed angle
      plotter_Ani(plot_para,dim_s, angle)
 
elif control >= 3 and steuerung >= 3:
      sys.exit("Wrong input")
            
            
      
