#include <stdio.h>
#include <mpi.h>
#include "mpi_setup.h"
#include "array.h"
#include "defines.h"
#include "display.h"

void setup_mpi(int *argc, char *** argv)
{
  int periods[2] = {0,0};

  MPI_Init(argc, argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    //In general you'd want to deal with decompositions that don't match to your
    //number of cells. Here, we restrict the code to a number of processors
    //that smoothly subdivides the number of cells
    if (nproc != 1 && nproc !=2 && nproc !=4 && nproc != 8 && nproc !=16) {
      if (rank == 0) {
        printf("Demo code only works on 1, 2, 4, 8 or 16 processors");
        MPI_Abort(MPI_COMM_WORLD, 2);
      }
    }
    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Dims_create(nproc, 2, nprocs);

#ifndef NODISPLAY
    if (rank == 0) {
      printf("Processor decomposition is %3d %3d\n", nprocs[0], nprocs[1]);
      printf("Please press a key to continue\n");
      getchar();
    }
#endif
    MPI_Barrier(MPI_COMM_WORLD);

    //Divide the global size (nx x ny) per processor
    nx_local = nx / nprocs[0];
    ny_local = ny / nprocs[1];

    //Calculate your co-ordinates in a Cartesian grid
    coordinates[1] = rank/nprocs[0];
    coordinates[0] = rank - coordinates[1] * nprocs[0];

    //Calculate which rank is along each edge of your domain
    //NOTE at this stage you have not dealt with processors at the edge
    //of your logical processor decomposition
    x_max_rank = rank + 1;
    x_min_rank = rank - 1;
    y_max_rank = rank + nprocs[0];
    y_min_rank = rank - nprocs[0];

    //If this processor is at an edge then some of the neighbours
    //will be MPI_PROC_NULL
    if (coordinates[0] == 0) {
      x_min_rank = MPI_PROC_NULL;
    }
    if (coordinates[0] == nprocs[0]-1) {
      x_max_rank = MPI_PROC_NULL;
    }
    if (coordinates[1] == 0) {
      y_min_rank = MPI_PROC_NULL;
    }
    if (coordinates[1] == nprocs[1]-1) {
      y_max_rank = MPI_PROC_NULL;
    }

    //Calculate what fraction of the global array this processor has
    x_cell_min_local = nx_local * coordinates[0] + 1;
    x_cell_max_local = nx_local * (coordinates[0] + 1);
    y_cell_min_local = ny_local * coordinates[1] + 1;
    y_cell_max_local = ny_local * (coordinates[1] + 1);

}

void gather_to_zero(grid_type *dest, grid_type *src)
{
  grid_type red;
  allocate_grid(&red, 0, nx+1, 0, ny+1);
  zero_grid(dest);

  //Copy values for local part of array into copy of global array
  copy_subgrid(dest, src, 1, nx_local, 1, ny_local, x_cell_min_local,
      x_cell_max_local, y_cell_min_local, y_cell_max_local);

  //Use MPI_Reduce to get globally correct global array
  MPI_Reduce(dest->data, red.data,(nx+2) * (ny+2), MPI_FLOAT,
      MPI_MAX, 0, MPI_COMM_WORLD);
  copy_grid(dest, &red, 0, nx+1, 0, ny+1);
  deallocate_grid(&red);
}
