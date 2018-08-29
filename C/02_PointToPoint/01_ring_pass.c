#include <stdio.h>
#include <mpi.h>


int main(int argc, char **argv){
  int rank, nproc, rank_right, rank_left, rank_recv;
  MPI_Status stat;
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  rank_left = rank -1;
  /*Ranks run from 0 to nproc-1, so wrap the ends around to make a loop*/
  if(rank_left == -1) rank_left = nproc-1;
  rank_right = rank + 1;
  if(rank_right == nproc) rank_right = 0;

  MPI_Send(&rank, 1, MPI_INT, rank_right, 100, MPI_COMM_WORLD);
  MPI_Recv(&rank_recv, 1, MPI_INT, rank_left, 100, MPI_COMM_WORLD, &stat);

  printf("Rank %i has received value %i from rank %i\n", rank, rank_recv,
      rank_left);

  MPI_Finalize();
}
