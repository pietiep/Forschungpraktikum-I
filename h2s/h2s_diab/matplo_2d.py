import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

def folder_gr():
      folder_list = [d for d in os.listdir(".") if os.path.isdir(d)] 
      return sorted(folder_list)

df = pd.read_table('h2s.tab', skiprows=3 , sep= "\s+")
def hart_2cm(df):
#    minimum = df[['STATE1', 'STATE2', 'STATE3']].min().min()
    df.loc[:,['E1']] -=  df.E1.min()
    df.loc[:,['E2']] -=  df.E2.min()
    df.loc[:,['E1', 'E2']] *=  219474.63

hart_2cm(df)


df = df.sort_values(by=["RD"])
print(df)
ax = df.plot(x='RD', y=['E1', 'E2'])
df.plot(x='RD',y='E1', kind='scatter', ax=ax)
df.plot(x='RD',y='E2', kind='scatter', ax=ax)
plt.savefig("komplex_old_geom.pdf", dpi=150)
plt.show()
