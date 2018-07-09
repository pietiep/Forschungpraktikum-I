nosym
memory,140,m

geom={ang
h1
x1,h1,a
h2,x1,a,h1,90
x2,h2,a,x1,90,h1,90
h3,x2,a,h2,90,x1,0
x3,h3,a,x2,90,x1,-90
h4,x3,a,h1,90,h3,90
x4,h1,0.5*a,x3,-90,x1,90
x5,x4,0.5*a,h1,90,x3,0
c1,x5,0.5*a,x4,90,h1,90
cl1,c1,r,h3,phi,h2,theta}

a=1.25624302
phi=180
theta=70.5


r=-2*a
basis=vqz-f12

i=0
do iph=1,9
  do ith=1,9
      phi(i)=180+iph*10
      theta(i)=70.5+ith*10
i=i+1

hf;
ehf(i)=energy
rccsd(t)-f12;
eccsd(i)=energc
eccsdt(i)=energy
enddo
enddo
{table, phi,theta,ehf,eccsd,eccsdt
head, phi,theta,scf,ccsd,ccsd(t)
save,chlor_methan,basis $basis}



