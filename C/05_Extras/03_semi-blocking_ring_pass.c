#include <stdio.h>
#include <mpi.h>


int main(int argc, char **argv){
  int rank, nproc, rank_right, rank_left, rank_recv;
  int rank5, rank_recv5, rank_2left, rank_2right;

  MPI_Request requests[2];
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  //This distinguishes the two sends, provided  < 70 procs
  rank5 = 70+rank;

  rank_left = rank -1;
  /*Ranks run from 0 to nproc-1, so wrap the ends around to make a loop*/
  if(rank_left == -1) rank_left = nproc-1;
  rank_right = rank + 1;
  if(rank_right == nproc) rank_right = 0;

  rank_2left = rank_left -1;
  if(rank_2left == -1) rank_2left = nproc-1;
  rank_2right = rank_right + 1;
  if(rank_2right == nproc) rank_2right = 0;


  MPI_Isend(&rank, 1, MPI_INT, rank_right, 100, MPI_COMM_WORLD, &requests[0]);
  //Barrier to ensure we're really mixing the calls
  MPI_Barrier(MPI_COMM_WORLD);

  MPI_Isend(&rank5, 1, MPI_INT, rank_2right, 50, MPI_COMM_WORLD, &requests[1]);
  //Barrier to ensure we're really mixing the calls
  MPI_Barrier(MPI_COMM_WORLD);

  MPI_Recv(&rank_recv5, 1, MPI_INT, rank_2left, 50, MPI_COMM_WORLD,
      MPI_STATUS_IGNORE);

  MPI_Recv(&rank_recv, 1, MPI_INT, rank_left, 100, MPI_COMM_WORLD,
      MPI_STATUS_IGNORE);

  MPI_Waitall(2, requests, MPI_STATUSES_IGNORE);

  printf("Rank %i has received value %i from rank %i\n", rank, rank_recv,
      rank_left);
  printf("Rank %i has also received value %i from rank %i\n", rank, rank_recv5,
      rank_2left);


  MPI_Finalize();
}
