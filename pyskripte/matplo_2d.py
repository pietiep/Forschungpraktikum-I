import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

def folder_gr():
      folder_list = [d for d in os.listdir(".") if os.path.isdir(d)] 
      return sorted(folder_list)

v = folder_gr()[0] + '/komplex.tab'
print(v)
df = pd.read_table(v, skiprows=3, sep= "\s+")

def hart_2cm(df):
    minimum = df[['MRCI1', 'MRCI2', 'MRCI3']].min().min()
    df.loc[:,['MRCI1', 'MRCI2', 'MRCI3']] -=  minimum
    df.loc[:,['MRCI1', 'MRCI2', 'MRCI3']] *=  219474.63

hart_2cm(df)
print(df)

def counter(df, winkel):
    return (df[winkel] == df[winkel][0]).sum()

dim_1 = int(counter(df, 'THETA_W'))
dim_2 = int(counter(df, 'THETA_W'))

X = np.array(df.PHI_W).reshape(dim_1,dim_2)
Y = np.array(df.THETA_W).reshape(dim_1,dim_2)
Z = np.array(df.MRCI1).reshape(dim_1,dim_2)



threedee = plt.figure().add_subplot(111, projection='3d')
threedee.plot_wireframe(X, Y, Z, rstride=1, cstride=1)
threedee.scatter(df.PHI_W, df.THETA_W, df.MRCI1)
threedee.scatter(df.PHI_W, df.THETA_W, df.MRCI2)
threedee.scatter(df.PHI_W, df.THETA_W, df.MRCI3)
#threedee.set_xlabel('PHI_W')
#threedee.set_ylabel('THETA_W')
#threedee.set_zlabel('MRCI1')
plt.show()
