OBJECTS = random_util.o problem_description.o mc_walk.o laplace_mc.o
FFLAGS =
.PHONY: test

test: laplace_mc.exe
	./laplace_mc.exe
	python plot_mc_laplace_error.py
	gnome-open laplace_mc_error.png

laplace_mc.exe: $(OBJECTS)
	gfortran $(FFLAGS) $(OBJECTS) -o laplace_mc.exe

%.o: %.f90
	gfortran $(FFLAGS) -c $<

clean:
	rm *.exe *.o *.mod
