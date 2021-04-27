CC ?= gcc
CXX ?= g++
F90 ?= gfortran
MPICC ?= mpigcc
MPICXX ?= mpigxx
MPIF90 ?= mpif90

all: test-fortran test-c

test-fortran: test-detach-task.f90 c_detach_task.o
	$(F90) -fopenmp test-detach-task.f90 c_detach_task.o -o test-fortran

test-c: test-detach-task.c c_detach_task.o
	$(CC) -fopenmp test-detach-task.c c_detach_task.o -o test-c


c_detach_task.o: c_detach_task.c
	$(CC) -shared -fPIC c_detach_task.c -c

