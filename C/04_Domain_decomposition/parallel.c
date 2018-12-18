#include "array.h"
#include "display.h"
#include "mpi_setup.h"
#include "defines.h"
#include <stdio.h>
#include <stdlib.h>

void bcs(grid_type *data)
{

  float *src, *dest;
  int index;

  //Assign the boundary conditions. 1.0 along the left and bottom
  //10.0 along the right and top. Only assign if on an edge, detected
  //using MPI_PROC_NULL as a neighbour
  if (x_min_rank == MPI_PROC_NULL)
      assign_grid(data, 0, 0, 0, ny_local+1, 1.0);
  if (x_max_rank == MPI_PROC_NULL)
      assign_grid(data, nx_local+1, nx_local+1, 0, ny_local+1, 10.0);
  if (y_min_rank == MPI_PROC_NULL)
      assign_grid(data, 0, nx_local+1, 0, 0, 1.0);
  if (y_max_rank == MPI_PROC_NULL)
      assign_grid(data, 0, nx_local+1, ny_local+1, ny_local+1, 10.0);

  //Unlike in Fortran, can't use array subsections. Have to copy to temporaries
  src = (float*) malloc(sizeof(float)*(ny_local));
  dest = (float*) malloc(sizeof(float)*(ny_local));

  //Send left most strip of cells left and receive into right guard cells
  for (index = 1; index<=ny_local; ++index){
    src[index-1] = *(access_grid(data, 1, index ));
    //Copy existing numbers into dest because MPI_Sendrecv is a no-op if
    //one of the other ranks is MPI_PROC_NULL
    dest[index-1] = *(access_grid(data, nx_local + 1, index ));
  }

  MPI_Sendrecv(src, ny_local, MPI_FLOAT, x_min_rank,
      TAG, dest, ny_local, MPI_FLOAT, x_max_rank,
      TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

  for (index = 1; index<=ny_local; ++index){
    *(access_grid(data, nx_local + 1, index )) = dest[index-1];
  }

  //Send right most strip of cells right and receive into left guard cells
  for (index = 1; index<=ny_local; ++index){
    src[index-1] = *(access_grid(data, nx_local, index ));
    dest[index-1] = *(access_grid(data, 0, index ));
  }
  MPI_Sendrecv(src, ny_local, MPI_FLOAT,
      x_max_rank, TAG, dest, ny_local, MPI_FLOAT, x_min_rank,
      TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  for (index = 1; index<=ny_local; ++index){
    *(access_grid(data, 0, index )) = dest[index-1];
  }

  //Now allocate data for the other direction
  free(dest);
  free(src);
  src = (float*) malloc(sizeof(float)*(nx_local));
  dest = (float*) malloc(sizeof(float)*(nx_local));

  //Now equivalently in y
  for (index = 1; index<=nx_local; ++index){
    src[index-1] = *(access_grid(data, index, 1));
    dest[index-1] = *(access_grid(data, index, ny_local + 1));
  }
  MPI_Sendrecv(src, nx_local, MPI_FLOAT, y_min_rank,
      TAG, dest, nx_local, MPI_FLOAT, y_max_rank,
      TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

  for (index = 1; index<=nx_local; ++index){
    *(access_grid(data, index, ny_local + 1)) = dest[index-1];
  }

  for (index = 1; index<=nx_local; ++index){
    src[index-1] = *(access_grid(data, index, ny_local));
    dest[index-1] = *(access_grid(data, index, 0));
  }
  MPI_Sendrecv(src, nx_local, MPI_FLOAT, y_max_rank,
      TAG, dest, nx_local, MPI_FLOAT, y_min_rank,
      TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

  for (index = 1; index<=nx_local; ++index){
    *(access_grid(data, index, 0)) = dest[index-1];
  }
  free(src);
  free(dest);
}

int main(int argc, char ** argv)
{

  grid_type values, values_local, temp_local;
  int ix, iy, icycle;
  //Allocate a 2D array with indices that run 0->nx+1 and 0->ny+1
  //This replicates Fortran's arrays with variable starts and ends
  allocate_grid(&values, 0, nx+1, 0, ny+1);
  setup_mpi(&argc, &argv);

  allocate_grid(&values_local, 0, nx_local+1, 0, ny_local+1);
  allocate_grid(&temp_local, 0, nx_local+1, 0, ny_local+1);
  //Assign the value 5.5 to the whole grid
  assign_grid(&values_local, 0, nx_local+1, 0, ny_local+1, 5.5);
  assign_grid(&values, 0, nx+1, 0, ny+1, 5.5);

  bcs(&values_local);
  gather_to_zero(&values, &values_local);

#ifndef NODISPLAY
  if (rank == 0) {
    display_result(&values);
    printf("Please press a key to advance\n");
    getchar();
  }
#endif
  //To a C programmer, this looks backwards, but the array is using
  //Fortran ordering deliberately
  for (icycle=0;icycle<500;++icycle){
    for (iy=1;iy<=ny_local;++iy){
      for (ix=1;ix<=nx_local;++ix){
        *(access_grid(&temp_local, ix, iy)) = 0.25 * (
            *(access_grid(&values_local, ix+1, iy  )) +
            *(access_grid(&values_local, ix  , iy+1)) +
            *(access_grid(&values_local, ix-1, iy  )) +
            *(access_grid(&values_local, ix  , iy-1)));
      }
    }
    copy_grid(&values_local, &temp_local, 1, nx_local, 1, ny_local);
    bcs(&values_local);
    if(icycle%50==0){
      gather_to_zero(&values, &values_local);
#ifndef NODISPLAY
      if(rank == 0) {
        display_result(&values);
        printf("Please press a key to advance\n");
        getchar();
      }
#endif
    }
  }
  deallocate_grid(&values);
  deallocate_grid(&values_local);
  deallocate_grid(&temp_local);
  MPI_Finalize();
}
