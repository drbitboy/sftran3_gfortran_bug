########################################################################
### Usage:
###
###   make run    ### G77 executable succeeds; GFORTRAN executable fails
###
###   make run MYFFLAGS=  ### Both G77 and GFORTRAN executables succeed.
###
########################################################################


### This MYFFLAGS assignment creates a CPP macro definition to make
### GFORTRAN produce correct code
### - Use [make MYFFLAGS= ...] to turn it off

MYFFLAGS=-DARGRTN=1

gfortran_FLAGS=


### FORTRAN flags

FFLAGS = -g -O0 -finit-local-zero -fno-automatic $(MYFFLAGS)


### I need these FLIBS options to link with G77 on my system;
### you may need to comment  mileage may vary

g77_FLIBS=-B/usr/lib/x86_64-linux-gnu -L/usr/lib/gcc/x86_64-linux-gnu/5


### Source files

SRCS=bbpas1.f batop2.f ncscan.f nncmpr.f


########################################################################
### Default target compiles G77 and GFORTRAN executables
all: y_g77 y_gfortran


### The [run] target compiles and runs both executables
run: all
	@echo ""
	-./y_g77.e
	@echo ""
	-./y_gfortran.e
	@echo ""


########################################################################
### These targets build the individual executables

y_g77:
	g77 -x f77-cpp-input $(FFLAGS) $(SRCS) -o $@.e $(g77_FLIBS)

y_gfortran:
	gfortran -cpp -std=legacy $(FFLAGS) $(gfortran_FFLAGS) $(SRCS) -o $@.e

########################################################################
