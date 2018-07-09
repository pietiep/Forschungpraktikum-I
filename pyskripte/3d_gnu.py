import os
import Gnuplot as gp
import numpy as np

#Instantiate Gnuplot object (erzeugen)
g = gp.Gnuplot()

w=os.getcwd()     #get actual path


g.title("Title")
g.xlabel('Theta')
g.ylabel('Phi')
g('set terminal gif animate delay 100')

g('set xtic rotate by -45 scale 1 font ",10"')
g('set ytic rotate by -45 scale 1 font ",10"')
g('set grid')
#g('set zrange [-499.76:-499.66]')
#g('set xrange [50:300]')
#g('set yrange [50:300]')

fileout='set out "myAni.gif"'
g(fileout)



g('set dgrid3d 30,30')
g('set hidden3d')
 
for i in range(0,11):
      r = 2.5+i*0.1
      
      v=w+"/%s_ang_on_face/komplex.tab" %r

      print(v)
      data= np.loadtxt(v, skiprows=4)
      

      databuff = gp.File(v, using='6:5:1',with_='line',title=str(r)+' 1')
      g.splot(databuff)
      databuff = gp.File(v, using='6:5:2', with_='line' ,title='2')
      g.replot(databuff)
      databuff = gp.File(v, using='6:5:3', with_='line' ,title='3')
      g.replot(databuff)

g('unset output')

