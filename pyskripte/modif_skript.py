import Gnuplot
from decimal import *
import numpy as np
#Instantiate Gnuplot object (erzeugen)
g = Gnuplot.Gnuplot()

g.title("Title")
g.xlabel('Theta')
g.ylabel('y')
g('set terminal gif animate delay 50')

g('set xtic rotate by -45 scale 1 font ",10"')
g('set grid')

data = np.loadtxt('../komplex.tab', skiprows=4)



value1 = np.min(data[:,0:3])
value2 = np.max(data[:,0:3])

print(value1,value2)

ranger = 'set yrange [%s:%s]' %(str(value1),str(value2))

g(ranger)

fileout='set out "myAni.gif"'
g(fileout)
 
phi=[]
for i in range(50):
      phi.append(0)

for i in range(0,40):
      phi[i]=round(Decimal(50.5+i*3.2),10)

clean_phi=str(filter(None, phi))      #entfernt die Nullelemente

for i in range(0,40):
      

      filename1='phi_%s.txt' %phi[i]
      

      databuff = Gnuplot.File(filename1, using='6:1', title=phi)
      g.plot(databuff)
#      databuff = Gnuplot.File(filename1, using='6:2',with_='line', title=phi)
#      g.replot(databuff)
#      databuff = Gnuplot.File(filename1, using='6:3',with_='line', title=phi)
#      g.replot(databuff)

g('unset output')
