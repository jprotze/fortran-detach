CC ?= gcc
CXX ?= g++
F90 ?= gfortran
MPICC ?= mpigcc
MPICXX ?= mpigxx
MPIF90 ?= mpif90

test: test-mpi-detach.f90 libdetach.so
	env MPICH_FC=$(F90) $(MPIF90) -fopenmp test-mpi-detach.f90 -L. -Wl,--rpath,. -ldetach -o test


libdetach.so: detach.cpp mpi-detach.h
	env MPICH_CXX=$(CXX) $(MPICXX) -DOMPI_SKIP_MPICXX=1 -shared -fPIC detach.cpp -o libdetach.so -g

