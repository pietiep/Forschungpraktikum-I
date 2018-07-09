***,H2S diabatic A2 states
basis=VDZ                               !use cc-pVDZ basis set
symmetry,x,planeyz                      !use Cs symmetry & fix orientation of the molecule
orient,noorient                         !dont allow automatic reorientation
geometry={s;h1,s,r1;h2,s,r2,h1,theta}   !Z-matrix geometry input
gprint,orbitals,civector                !global print options

text,reference calculation for C2V
theta=92.12,r1=2.3,r2=2.3               !reference geometry
{hf;occ,7,2;wf,18,1}                    !scf calculation for ground state
{multi;occ,9,2;closed,4,1;              !define active and inactive spaces
wf,18,2;state,2;                        !two A2 states (1B1 and 1A2 in C2v)
orbital,2140.2}                         !save orbitals to 2140.2
reforb=2140.2

text,calculations at displaced geometries
distanz = [3.0, 2.9, 2.8, 2.7, 2.6, 2.5, 2.4, 2.3, 2.2, 2.1, 2.0]
do i=1,#distanz                              !loop over displaced geometries
r2=distanz(i)                                !set r2 to current distance
{multi;occ,9,2;closed,4,1;              !same wavefunction definition as at reference geom.
wf,18,2;state,2;}
orbital,2141.2                          !save new orbitals to record
diab,reforb}                            !compute diabatic orbitals using reference orbitals
                                        !stored on record reforb
reforb=2141.2                           !set variable reforb to the new orbitals.
e1(i)=energy(1)                         !Save adiabatic energies
e2(i)=energy(2)
enddo


{table, e1, e2, distanz
head, e1, e2, distanz
title,Results, basis $basis
save, h2s.tab}
