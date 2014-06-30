OBJECTS = random_util.o problem_description.o mc_walk.o laplace_mc.o
FFLAGS =
NUM_PROCS ?= 4
.PHONY: test

test: laplace_mc.exe
	mpiexec -n $(NUM_PROCS) laplace_mc.exe
	python plot_mc_laplace_error.py
	gnome-open laplace_mc_error.png

laplace_mc.exe: $(OBJECTS)
	mpif90 $(FFLAGS) $(OBJECTS) -o laplace_mc.exe

%.o: %.f90
	mpif90 $(FFLAGS) -c $<

clean:
	rm *.exe *.o *.mod
