# $UWHPSC/codes/fortran/newton/Makefile

OBJECTS = functions.o newton.o test1.o
OBJECTS2 = intersections.o newton.o
MODULES = functions.mod newton.mod
MODULES2 = newton.mod

FFLAGS = -g

.PHONY: test1 clean intersections

intersections: intersections.exe
	./intersections.exe

test1: test1.exe
	./test1.exe

test1.exe: $(MODULES) $(OBJECTS)
	gfortran $(FFLAGS) $(OBJECTS) -o test1.exe

intersections.exe: $(MODULES2) $(OBJECTS2)
	gfortran $(FFLAGS) $(OBJECTS2) -o intersections.exe

%.o : %.f90
	gfortran $(FFLAGS) -c $< 

%.mod: %.f90
	gfortran $(FFLAGS) -c $<

clean:
	rm -f *.o *.exe *.mod

