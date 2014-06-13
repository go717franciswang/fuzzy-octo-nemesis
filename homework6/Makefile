
OBJECTS = functions.o quadrature.o test.o
OBJ_COPYVALUES = copyvalue.o
FFLAGS = 
NUM_PROCS ?= 4   # default if not specified on command line or env variable

.PHONY: test copyvalue clean 

test: test.exe
	mpiexec -n $(NUM_PROCS) test.exe

copyvalue: copyvalue.exe
	mpiexec -n $(NUM_PROCS) copyvalue.exe

test.exe: $(OBJECTS)
	mpif90 $(FFLAGS) $(OBJECTS) -o test.exe

copyvalue.exe: $(OBJ_COPYVALUES)
	mpif90 $(FFLAGS) $(OBJ_COPYVALUES) -o copyvalue.exe

%.o : %.f90
	mpif90 $(FFLAGS) -c  $< 

clean:
	rm -f *.o *.exe *.mod

