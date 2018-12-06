VPATH = .:./support:./lib
main:serial parallel

serial:serial.c array.o display.o
	mpicc -g -I ./support -o serial ./lib/array.o ./lib/display.o serial.c
parallel: parallel.c array.o display.o mpi_setup.o
	mpicc -g -I ./support -o parallel ./lib/array.o ./lib/display.o ./lib/mpi_setup.o parallel.c
mpi_setup.o: mpi_setup.c array.o
	mpicc -g -c -I ./support -o ./lib/mpi_setup.o ./support/mpi_setup.c
array.o:array.c array.h
	mpicc -g -c -I ./support -o ./lib/array.o ./support/array.c
display.o:display.c display.h array.o
	mpicc -g -c -I ./support -o ./lib/display.o ./support/display.c
