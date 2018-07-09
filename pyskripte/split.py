import numpy as np

myfile='komplex.tab'


data = np.loadtxt(myfile, skiprows=4)

a=data.shape
#B=np.zeros((40,a[1]))
B=np.zeros(a)
print(a)
#for j in range(0,41):
#      B[j,:] = data[j,:]
#      C = np.squeeze(B)
#

print(np.divide(a[0], 39))

for i in range(0,a[0]/39):
      B = data[i*39:(i+1)*39, :]
 

      myoutfile='phi_%s.txt' %(0.5+i*9)

      np.savetxt(myoutfile,B, fmt="%.8f")
