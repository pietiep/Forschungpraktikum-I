######################################################################
#                            GNU MAKEFILE                            #
######################################################################
# There may be some trickery involved in this file; most of which is #
# hopefully explained sufficiently.  This makefile is unlikely to    #
# work with non-GNU make utilities.                                  #
#                                                                    #
######################################################################

# declare shell explicitly
SHELL = /bin/sh

# get absolute file path of current base dir ../
# (without trailing /)
basedir := $(patsubst %/,%,$(dir $(CURDIR)))

# clear out suffix list
.SUFFIXES :
# declare suffixes allowed to be subject to implicit rules.
.SUFFIXES : .f .o

# declare targets not belonging to any file as such.
.PHONY : wipe unwipe clean profile test \
         slow_test move errs neat

#FC = g77
FC = ifort

#add lib-directory to make's search path
VPATH += ./lib

#FFLAGS = -O3 -openmp
#FFLAGS = -O0  -check bounds -debug all
#FFLAGS = -fast -check bounds
FFLAGS = -fast -openmp
#FFLAGS= -fast
#FFLAGS = -fast  -p
#FFLAGS = -fast -inline-forceinline
#FFLAGS = -fast -openmp

warn_flags = -warn all -warn notruncated_source -warn nounused
FFLAGS += $(warn_flags)

### Objects

# Objects solely relying on ANN parameters
ann_objects = backprop.o ff_neunet.o neuron_types.o \
              nnmqo.o axel.o
# Objects relying on both genetic & ANN params
genann_objects = geNNetic.o iNNput.o iNNterface.o puNNch.o \
                 mkNN.o error.o long_io.o maNNipulate.o

# Objects depending on model
mod_objects = nnmodel.o

# Objects belonging to internal library
lib_objects = linfit.o misc.o ran_gv.o choldc.o rs.o \
              qsort.o dmatrix.o

# Objects from hell; if it exists, don't compile it
hell_objects =  lib/random.o
hell_flags = -fast

# All objects (one would want to update)
objects = $(ann_objects) $(genann_objects) $(mod_objects) \
          $(lib_objects)

# Name of the main program
main = genANN

# Misc. files generated during compilation
trash = *__genmod* *~ *\# *.g

### Include files

ann_include = nncommon.incl nndbg.incl nnparams.incl
gen_include = common.incl params.incl
mod_include =

### Special file names

## 'Install' directory
install = $(basedir)/bin
# Directory of 'installed' program
main_inst = $(install)/$(main)

## Logfiles
# logfile
logfile = $(basedir)/makelog
# logfile in the same folder as the fits
fitlog=$(basedir)/nnfits/genloge.dat

## specific input file used
#in_file=gen_sine_input.in
#in_file=gen_sinc_input.in
in_file=sinetest.in
#in_file=sinctest.in

# define main goal of make
genANN : $(objects)
	$(FC) $(FFLAGS) $(objects) $(hell_objects) -o genANN

# add dependencies to include files
$(ann_objects) : $(ann_include) $(lib_objects)
$(genann_objects) : $(ann_include) $(gen_include) $(lib_objects)
$(mod_objects) :  $(ann_include) $(gen_include) $(mod_include) $(lib_objects)
# overrule standard compilation method for hellfiles.
$(hell_objects) : override FFLAGS=$(hell_flags)

# prefix '-': ignore failure state of this command.
clean :
	-rm -f $(objects) $(main) $(trash)

neat :
	-rm -f $(trash)

test :
	make
	make move
	date > $(logfile)
	time $(install)/$(main) $(install)/$(in_file) >> $(logfile)
	cp $(logfile) $(fitlog)
	date >> ../logs/performance.log

slow_test :
	make clean
	make
	make move
	date > $(logfile)
	time $(install)/$(main) $(install)/$(in_file) >> $(logfile)
	cp $(logfile) $(fitlog)
	date >> ../logs/performance.log

profile:
	make clean
	make test
	date > ../profile
	gprof $(main) gmon.out >> ../profile

# let make suffer for a bit; continue even if a compilation fails
errs:
	make clean
	-make -k
	make clean

# let the user suffer for a bit; compile hellfiles
hell : $(hell_objects)

move:
	cp $(objects) $(main) $(hell_objects) $(in_file) $(install)

wipe :
	for OBJ in $(hell_objects); \
	do \
	    mv $$OBJ $$OBJ~; \
	done

unwipe:
	for OBJ in $(hell_objects); \
	do \
	    mv $$OBJ~ $$OBJ; \
	done ; \
	make clean
