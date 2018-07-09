***,h2s Diabatization
memory,3,m

gprint,orbitals,civector

symmetry,x
orient,noorient             !noorient should always be used for diabatization
geometry={
         s;
         h1,s,r1;
         h2,s,r2,h1,theta}

basis=avdz                  !This basis is too small for real application

r1=2.5                      !Reference geometry
theta=[92]

distanz = [3.0, 2.9, 2.8, 2.7, 2.6, 2.5, 2.4, 2.3, 2.2, 2.1, 2.0]

reforb=2140.2               !Orbital dumprecord at reference geometry
refci=6000.2                !MRCI record at reference geometry
savci=6100.2                !MRCI record at displaced geometries

text,compute wavefunction at reference geometry
r2=r1

{hf;occ,9,2;wf,18,2,4;
orbital,2100.2}

{multi;occ,9,2;closed,4,1;
wf,18,2;state,2;            !1B1 and 1A2 states
natorb,reforb               !Save reference orbitals on reforb
noextra}                    !Dont use extra symmetries

{ci;occ,9,2;closed,4,1;     !MRCI at reference geometry
wf,18,2,0;state,2;          !1B1 and 1A2 states
orbital,reforb              !Use orbitals from previous CASSCF
save,refci}                 !Save MRCI wavefunction

Text,Displaced geometries

do i=1,#distanz                   !Loop over different r values
data,truncate,savci+1       !truncate dumpfile after reference
r2=distanz(i)                     !Set current r2

{multi;occ,9,2;closed,4,1;
wf,18,2,0;state,2;          !Wavefunction definition
start,reforb                !Starting orbitals
orbital,3140.2;             !Dump record for orbitals
diab,reforb                 !Generate diabatic orbitals relative to reference geometry
noextra}                    !Dont use extra symmetries

{ci;occ,9,2;closed,4,1;
wf,18,2,0;state,2;          !1B1 and 1A2 states
orbital,diabatic            !Use diabatic orbitals
save,savci}                 !Save MRCI for displaced geometries

e1(i)=energy(1)             !Save adiabatic energies
e2(i)=energy(2)

{ci;trans,savci,savci       !Compute transition densities at R2
dm,7000.2}                  !Save transitiondensities on this record
{ci;trans,savci,refci;      !Compute transition densities between R2 and R1
dm,7100.2}                  !Save transition densities on this record

{ddr
density,7000.2,7100.2       !Densities for <R2||R2> and <R2||R1>
orbital,3140.2,2140.2       !Orbitals for <R2||R2> and <R2||R1>
energy,e1(i),e2(i)          !Adiabatic energies
mixing,1.2,2.2}             !Compute mixing angle and diabatic energies

mixci(i)=mixangci(1)        !Mixing angle obtainedfrom ci vectors only
h11ci(i)=hdiaci(1)          !Diabatic energies obtained from ci vectors only
h21ci(i)=hdiaci(2)
h22ci(i)=hdiaci(3)

mixtot(i)=mixang(1)         !Mixing angle from total overlap (including first-order correction)
h11(i)=hdia(1)              !Diabatic energies obtained from total overlap
h21(i)=hdia(2)
h22(i)=hdia(3)


{table,distanz,e1,e2,h11ci,h22ci,h21ci,mixci
title,Diabatic energies for H2S, obtained from CI-vectors
format,'(f10.2,5f14.8,f12.2)'
sort,1
save, h2s_ci.tab}

{table,distanz,e1,e2,h11,h22,h21,mixtot
title,Diabatic energies for H2S, obtained from CI-vectors and orbital correction
format,'(f10.2,5f14.8,f12.2)'
sort,1
save, h2s_ci_orbcorr.tab}

enddo                       !end loop over i

