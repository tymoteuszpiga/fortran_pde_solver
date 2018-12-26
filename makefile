#Defining variables
objects = main.o las_mod.o func.o finite_element_matrices.o
modules = las_mod.mod func.mod finite_element_matrices.mod
comp = gfortran
zero = -finit-local-zero -mcmodel=large
lapack = -L/usr/local/lib -llapack
#Makefile
all: $(objects)
	$(comp) -o all $(objects)  $(zero) $(lapack)
las_mod.mod: las_mod.o las_mod.f
	$(comp) -c las_mod.f $(zero)
las_mod.o: las_mod.f
	$(comp) -c las_mod.f $(zero)
func.mod: func.o func.f
	$(comp) -c func.f $(zero)
func.o: func.f
	$(comp) -c func.f $(zero)
finite_element_matrices.mod: finite_element_matrices.o finite_element_matrices.f
	$(comp) -c finite_element_matrices.f $(zero)
finite_element_matrices.o: finite_element_matrices.f
	$(comp) -c finite_element_matrices.f $(zero)
main.o: $(modules) main.f
	$(comp) -c main.f $(zero) $(lapack)
#Clean
clean:
	rm $(objects)
	rm $(modules)
#END
