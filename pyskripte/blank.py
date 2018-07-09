import pandas as pd
import numpy as np
import sys
import os
import matplotlib.pyplot as plt
from pylab import figure, axes, pie, title, show
import Gnuplot 

#########################################
#         Bestimmung von dim            #
#########################################

def counter(data, path, winkel):
      data_np = np.loadtxt(path, skiprows=4)                                            # Numpyarray
#      with open(path,'r') as f:
#            data = f.readlines()                                                       # Liste
      labels = data[3].split()                                                          # 1. Zeile von data in Elemente aufgeteilt und in neuer Liste gespeichert
      df =  pd.DataFrame(data_np)                                                       # Pandas Dataframe aus Numpyarray
      df.columns = labels                                                               # Benennung der Spalten des Pandas Dataframes
      same_ent = (df[[winkel]] == df.at[0,winkel]).sum()                                # Summe von True = 1, .sum() Methode erzeugt Pandas.Serie
      dim = same_ent.iat[0]                                                             # at --> index-Nummerierung; iat --> index-Labeling 
      rest = df.shape[0]%dim                                                            # df.shape[0] = Gesamtzahl der Zeilen
      if rest != 0:
            sys.exit("Error in counter-Funktion, check blank.py")
      dim_ge = df.shape[0]/dim
      return dim, dim_ge

      
##########################################
# Erzeugt nach dim Zeilen eine Leerzeile #
##########################################

def blanki(data,path,dim_s):
      del data[0:4]
      print(data)
      B = []
      dim = dim_s[0]
      dim_ge = dim_s[1]
 
      B[0:dim] = data[0:dim]
      
      for i in range(dim_ge):
            B.append(' \n')                                                             # Insert of blank line.
            B.extend(data[(i+1)*dim:(i+1)*dim+dim])
      
      with open(path,'w') as out:
            out.writelines(B)
      return


#######get Pandas Dataframe from Molpro-output#########
def getpand(data,path):
      labels = data[3].split()
      data_np = np.loadtxt(path,skiprows=4)
      df = pd.DataFrame(data_np)
      df.columns = labels
      return df
      
##############################################################
# Erzeugt Ordner winkel'_fest' mit jeweiligen 2D - Schnitten #
##############################################################
def splitter(df, angle, dim_s):
      lable_list = list(df)                                                             #Labels
      col_num = lable_list.index(angle)                                                 #Spaltenzahl bzgl des Labels
      target = angle + '_fest'                                                           
      dim = dim_s[0]
      dim_ge = dim_s[1]
      index_na = np.unique(np.array(df[angle]))                                         # Contains numbers of the angles, which will label the files.
      if np.shape(index_na) != dim_ge:
            sys.exit("Error in splitter: Dimension unequel to index_na Array!") 
      try:
            os.makedirs(target)
      except OSError as err:
            print("OS Error: {0}".format(err))
            print("Nevertheless start splitting and coping Data")
      data_np = df.as_matrix()                                                          # Converts Pandas Dataframe into Numpy-Matrix-Array
      data_np = data_np[np.argsort(data_np[:,col_num])]                                 # Sorts the whole matrix concerning the column: 'col_num'
      data_min = (np.amin(data_np[:,0:3]))
      data_max = (np.amax(data_np[:,0:3]))
      out_file_list = []
      for i in range(dim_ge):
            B = data_np[i * dim : (i+1) * dim, :]                                       # Every dim-th array is Save in a separate file.
#            df_wink = pd.DataFrame(B)
#            df_wink.columns = lable_list
            out_file = target + '/' + target.lower() + '_%s.txt' %index_na[i]
            np.savetxt(out_file,B,fmt="%.8f")
            print(out_file)
            
#            df_wink.to_csv(out_file, index=False, sep='\t')
            out_file_list.append(out_file)
      return  out_file_list, lable_list, data_min, data_max


#############################################################
#                  Aufrufen von Gnuplotpy                   #
#############################################################
def plotter_Ani(out_file_list, dim_s, angle2):
      dim_ge = dim_s[1]
      angle = out_file_list[-1]
      del out_file_list[-1]
      data_max = out_file_list[-1]
      del out_file_list[-1]
      data_min = out_file_list[-1]
      del out_file_list[-1]
      print(data_min, data_max)
      name_col = out_file_list[1][:]
      col_num = name_col.index(angle) + 1
      del out_file_list[1][:]
      g = Gnuplot.Gnuplot()
      ranges = 'set yrange[%s:%s]' %(str(data_min), str(data_max))
      g(ranges)
      g.xlabel(angle2)
      g.ylabel('V [Hartree]')
      g('set terminal gif animate delay 50')
      g('set xtic scale 1 font ",10"')
      g('set grid')
      fileout='set out "myAni.gif"'
      g(fileout)
      for entry in out_file_list[0]:                                                  # out_file_list[0]: index necessary, because it's a list of list
            gnu_para = str(col_num) + ':1'
            databuff1 = Gnuplot.File(entry, using=gnu_para, title=angle)
            gnu_para = str(col_num) + ':2'
            databuff2 = Gnuplot.File(entry, using=gnu_para, title=angle)
            gnu_para = str(col_num) + ':3'
            databuff3 = Gnuplot.File(entry, using=gnu_para, title=angle)
            g.plot(databuff1, databuff2, databuff3)
      g('unset output')
             


