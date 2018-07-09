import numpy as np
from matplotlib import pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.animation
import pandas as pd
from molden import sort_human
from molden import file_gr


bubble_size = [32,32,32,32,77,99] 
bubble_size = pd.DataFrame(bubble_size) * 5
bubble_color = ['y', 'y', 'y', 'y', 'k', 'g']

files = sort_human(file_gr())
print(len(files))

df = pd.concat(pd.read_table(file_, header=None, sep= "\s+", index_col=0,
skiprows=399, nrows=12) for file_ in files)

del df[1]
del df[2]
df.columns = ['x','y','z']
df = df[df.index != 'X']
t = np.array([np.ones(6)*i for i in range(len(files))]).flatten()
df['time'] = t

def update_graph(num):
    data=df[df['time']==num]
    graph._offsets3d = (data.x, data.y, data.z)
#    print('3D Test, time={}'.format(num))


fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
title = ax.set_title('3D Test')

data=df[df['time']==0]
plt.axis('off')
graph = ax.scatter(data.x, data.y, data.z, s=bubble_size, c=bubble_color)
ani = matplotlib.animation.FuncAnimation(fig, update_graph, len(files), interval=100,blit=False)
plt.show()
