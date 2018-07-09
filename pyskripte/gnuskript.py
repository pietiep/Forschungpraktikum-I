import Gnuplot
from numpy import *


#Instantiate Gnuplot object (erzeugen)
g = Gnuplot.Gnuplot()

g.title("Title")
g.xlabel('x')
g.ylabel('y')
g('set auto x')
#g('set term pdf')
g('set terminal gif animate delay 100')

g('set xtic rotate by -45 scale 5 font ",2"')
g('set grid')


fileout='set out "myAni.gif"'
g(fileout)
 
for i in range(0,40):
      phi = 0.5+i*9
      
     # fileout='set out "output%s.pdf"' %phi

      filename1='phi_%s.txt' %phi
      


      databuff = Gnuplot.File(filename1, using='6:3',with_='line', title=phi)
      g.plot(databuff)

g('unset output')
