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

    if (rank == 0) {
      printf("Processor decomposition is %3d %3d\n", nprocs[0], nprocs[1]);
      printf("Please press a key to continue\n");
      getchar();
    }
    MPI_Barrier(MPI_COMM_WORLD);

    //Divide the global size (nx x ny) per processor
    nx_local = nx / nprocs[0];
    ny_local = ny / nprocs[1];

    MPI_Cart_create(MPI_COMM_WORLD, 2, nprocs, periods, 1,
        &cart_comm);

    //Rank in new communicator might be different
    MPI_Comm_rank(cart_comm, &rank);

    //Get the rank of the neighbouring processors in Cartesian communicator
    MPI_Cart_shift(cart_comm, 0, 1, &x_min_rank, &x_max_rank);
    MPI_Cart_shift(cart_comm, 1, 1, &y_min_rank, &y_max_rank);

    //Get my coordinates in Cartesian communicator
    MPI_Cart_coords(cart_comm, rank, 2, coordinates);

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
      MPI_MAX, 0, cart_comm);
  copy_grid(dest, &red, 0, nx+1, 0, ny+1);
  deallocate_grid(&red);
}
