MODULES = quadrature.mod 
OBJECTS = quadrature.f90 test1.f90
FFLAGS = -g
.PHONY: test1 clean

test1: test1.exe
	./test1.exe

test1.exe: $(MODULES) $(OBJECTS)
	gfortran $(FFLAGS) $(OBJECTS) -o test1.exe

%.o : %.f90
	gfortran $(FFLAGS) -c $<

%.mod : %.f90
	gfortran $(FFLAGS) -c $<

clean:
	rm -f *.o *.exe *.mod

