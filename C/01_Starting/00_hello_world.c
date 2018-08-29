#include <mpi.h>
#include <stdio.h>
int main(int argc, char **argv)
{
  MPI_Init(&argc, &argv);
  printf("Multiprocessor code\n");
  MPI_Finalize();
}
