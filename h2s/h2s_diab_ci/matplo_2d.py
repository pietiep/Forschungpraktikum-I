import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

def folder_gr():
      folder_list = [d for d in os.listdir(".") if os.path.isdir(d)] 
      return sorted(folder_list)

df = pd.read_table('h2s_ci_orbcorr.tab', skiprows=3 , sep= "\s+")
def hart_2cm(df):
#    df_ref = pd.read_table('komplex_ref.tab', skiprows=3, sep ='\s+')
#    print(df_ref)
    minimum = df[['E1', 'E2', 'H11', 'H22']].min().min()
#    df.loc[:,['E1']] -=  df_ref.E1.values
#    df.loc[:,['E2']] -=  df_ref.E2.values
#    df.loc[:,['H11']] -=  df_ref.H11.values
#    df.loc[:,['H22']] -=  df_ref.H22.values
    df.loc[:,['E1', 'E2', 'H11', 'H22']] -=  minimum
    df.loc[:,['E1', 'E2', 'H11', 'H22']] *=  219474.63

hart_2cm(df)


df = df.sort_values(by=["DISTANZ"])
print(df)
#ax = df.plot(x='DISTANZ', y=['STATE1', 'STATE2', 'STATE3'])
ax = df.plot(x='DISTANZ', y=['E1', 'E2', 'H11', 'H22'])
df.plot(x='DISTANZ',y='E1', kind='scatter', ax=ax)
df.plot(x='DISTANZ',y='E2', kind='scatter', ax=ax)
df.plot(x='DISTANZ',y='H11', kind='scatter', ax=ax)
df.plot(x='DISTANZ',y='H22', kind='scatter', ax=ax)
plt.savefig("komplex_old_geom.pdf", dpi=150)
plt.show()
