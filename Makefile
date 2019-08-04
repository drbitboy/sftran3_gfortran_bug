FFLAGS =  -g -O0 -finit-local-zero -fno-automatic
g77_FLIBS=-B/usr/lib/x86_64-linux-gnu -L/usr/lib/gcc/x86_64-linux-gnu/5

SRCS=bbpas1.f batop2.f ncscan.f nncmpr.f

all: y_g77 y_gfortran
#all: y_g77 y_fort77 y_gfortran y_f77 y_f95

run: all
	@echo ""
	-./y_g77.e
	@#@echo ""
	@#-./y_fort77.e
	@#@echo ""
	@#-./y_f77.e
	@echo ""
	-./y_gfortran.e
	@#@echo ""
	@#-./y_f95.e
	@echo ""

y_g77:
	g77 $(FFLAGS) $(SRCS) -o $@.e $(g77_FLIBS)

y_fort77:
	fort77 $(FFLAGS) $(SRCS) -o $@.e

y_f77:
	f77 -std=legacy $(FFLAGS) $(SRCS) -o $@.e

y_gfortran:
	gfortran -std=legacy $(FFLAGS) $(SRCS) -o $@.e

y_f95:
	f95 $(FFLAGS) $(SRCS) -o $@.e
