#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <mpi.h>
#define ITS 10000

void seed_rng(int rank){
  time_t t;
  srand((unsigned) time(&t) + rank);
}

double get_random_in_range(){
  return rand()%100000/99999.0 * 2.0 - 1.0;
}

int main(int argc, char **argv){
  int count = 0, index;
  double d1, d2;
  int rank, nproc;
  int *count_global;
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  seed_rng(rank);
  for (index=0;index<ITS;++index){
    /* random numbers between ±1*/
    d1=get_random_in_range();
    d2=get_random_in_range();
    if (d1*d1 + d2*d2 <= 1) count ++;
  }
  count_global = malloc(sizeof(int)*nproc);
  MPI_Gather(&count, 1, MPI_INT, count_global, 1, MPI_INT, 0, MPI_COMM_WORLD);
  if (rank == 0) {
    for (index = 0; index < nproc; ++index) {
      printf("%i,%i\n", count_global[index], ITS);
    }
  }
  MPI_Finalize();
}
