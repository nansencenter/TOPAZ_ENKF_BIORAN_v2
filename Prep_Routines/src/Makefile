MPI = NO
include make.inc

SHELL = /bin/bash
VPATH = .:TMP

EXEC_SAT=proc_satellite
EXEC_BOT=proc_bottle

all: $(EXEC_SAT) $(EXEC_BOT)

PROC_SAT_SRC_F90 = proc_satellite.F90
PROC_BOT_SRC_F90 = proc_bottle.F90
COMM_UTIL_SRC_f90 = utils.f90

PROC_SAT_OBJS = $(PROC_SAT_SRC_F90:.F90=.o)  $(COMM_UTIL_SRC_f90:.f90=.o) 
PROC_BOT_OBJS = $(PROC_BOT_SRC_F90:.F90=.o)  $(COMM_UTIL_SRC_f90:.f90=.o) 

proc_satellite.o: utils.o
proc_bottle.o: utils.o

$(EXEC_SAT): $(PROC_SAT_OBJS)
	@echo "->proc_sat"
	@cd ./TMP; $(LD) $(LINKFLAGS) -o ../$(EXEC_SAT) $(PROC_SAT_OBJS) $(LIBS)

$(EXEC_BOT): $(PROC_BOT_OBJS)
	@echo "->proc_bot"
	@cd ./TMP; $(LD) $(LINKFLAGS) -o ../$(EXEC_BOT) $(PROC_BOT_OBJS) $(LIBS)

%.o: %.F90
	@echo "  $*".F90
	@rm -f ./TMP/$*.f90
	@cat MODEL.CPP $< | $(CPP) $(CPPFLAGS)  > ./TMP/$*.f90
	@cd ./TMP; $(CF90) -c $(FFLAGS) $(F90FLG) -o $*.o $*.f90

%.o: %.f90
	@echo "  $*".f90
	@rm -f ./TMP/$*.f90
	@cp $*.f90 ./TMP/$*.f90
	@cd ./TMP; $(CF90) -c $(FFLAGS) $(F90FLG) -o $*.o $*.f90

clean:
	@rm -f TMP/*.o TMP/*.f90 TMP/*.h TMP/*.mod $(EXEC_SAT) $(EXEC_BOT)
