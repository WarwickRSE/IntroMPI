#ifndef MPI_SETUP_H
#define MPI_SETUP_H

#include<mpi.h>
#include"array.h"

//Information about local domain
int nx_local, ny_local;
int x_cell_min_local, x_cell_max_local;
int y_cell_min_local, y_cell_max_local;

//information about MPI objects
int rank, nproc;
int nprocs[2], coordinates[2];
int x_min_rank, x_max_rank, y_min_rank, y_max_rank;
MPI_Comm cart_comm;

void setup_mpi(int *argc, char *** argv);
void gather_to_zero(grid_type *src, grid_type *dest);

#endif
