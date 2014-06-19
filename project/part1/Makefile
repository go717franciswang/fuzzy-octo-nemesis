
OBJECTS3 = functions.o quadrature.o test3.o
OBJ_COPYVALUES = copyvalue.o
FFLAGS = 
NUM_PROCS ?= 4   # default if not specified on command line or env variable

.PHONY: test test2 copyvalue clean 

test3: test3.exe
	mpiexec -n $(NUM_PROCS) test3.exe

copyvalue: copyvalue.exe
	mpiexec -n $(NUM_PROCS) copyvalue.exe

test3.exe: $(OBJECTS3)
	mpif90 $(FFLAGS) $(OBJECTS3) -o test3.exe

copyvalue.exe: $(OBJ_COPYVALUES)
	mpif90 $(FFLAGS) $(OBJ_COPYVALUES) -o copyvalue.exe

%.o : %.f90
	mpif90 $(FFLAGS) -c  $< 

clean:
	rm -f *.o *.exe *.mod

