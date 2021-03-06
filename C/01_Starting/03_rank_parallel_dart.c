#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <mpi.h>
#define ITS 10000

void seed_rng(){
  time_t t;
  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  srand((unsigned) time(&t) + rank);
}

double get_random_in_range(){
  return rand()%100000/99999.0 * 2.0 - 1.0;
}

int main(int argc, char **argv){
  int count = 0, index;
  double d1, d2;
  MPI_Init(&argc, &argv);
  seed_rng();
  for (index=0;index<ITS;++index){
    /* random numbers between ±1*/
    d1=get_random_in_range();
    d2=get_random_in_range();
    if (d1*d1 + d2*d2 <= 1) count ++;
  }
  printf("%i,%i\n", count, ITS);
  MPI_Finalize();
}
