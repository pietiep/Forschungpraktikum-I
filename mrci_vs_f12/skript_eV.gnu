unset log                              # remove any log-scaling |epstopdf --filter > 
unset label                            # remove any previous labels
reset

!set terminal postscript eps enhanced color solid defaultplex
!set output "energy.eps"

set terminal pdf enhanced
set output "energy.pdf"

set encoding iso_8859_1

set xlabel "r [\305]"
set ylabel "Energie [eV]"

!set xrange [4:10]
set style fill transparent solid 0.5 noborder
set style circle radius 0.05

name="komplex.txt" 
plot name u 7:1 w circles, name u 7:2 w circles, name u 7:3 w circles, name u 7:4 w circles, name u 7:5 w circles, name u 7:6 w circles

set output
