MODULES = quadrature.mod 
OBJECTS = quadrature.f90 test1.f90
OBJECTS2 = quadrature.f90 test2.f90
FFLAGS = -g
.PHONY: test1 test2 clean

test1: test1.exe
	./test1.exe

test2: test2.exe
	./test2.exe

test2_omp: quadrature_omp.f90 test2_omp.f90
	gfortran -fopenmp quadrature_omp.f90 test2_omp.f90 -o test2_omp.exe
	./test2_omp.exe

test1.exe: $(MODULES) $(OBJECTS)
	gfortran $(FFLAGS) $(OBJECTS) -o test1.exe

test2.exe: $(MODULES) $(OBJECTS2)
	gfortran $(FFLAGS) $(OBJECTS2) -o test2.exe

%.o : %.f90
	gfortran $(FFLAGS) -c $<

%.mod : %.f90
	gfortran $(FFLAGS) -c $<

clean:
	rm -f *.o *.exe *.mod

