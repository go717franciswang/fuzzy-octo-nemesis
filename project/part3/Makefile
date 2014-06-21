OBJECTS = random_util.o problem_description.o mc_walk.o laplace.o
FFLAGS =
.PHONY: test

test: test.exe
	./test.exe

test.exe: $(OBJECTS)
	gfortran $(FFLAGS) $(OBJECTS) -o test.exe

%.o: %.f90
	gfortran $(FFLAGS) -c $<

clean:
	rm *.exe *.o *.mod
