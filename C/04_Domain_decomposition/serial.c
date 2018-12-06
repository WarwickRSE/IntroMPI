#include "array.h"
#include "display.h"
#include "defines.h"
#include <stdio.h>
#include <stdlib.h>

void bcs(grid_type *data)
{

  //Assign the boundary conditions. 1.0 along the left and bottom
  //10.0 along the right and top.
  assign_grid(data, 0, 0, 0, ny+1, 1.0);
  assign_grid(data, nx+1, nx+1, 0, ny+1, 10.0);
  assign_grid(data, 0, nx+1, 0, 0, 1.0);
  assign_grid(data, 0, nx+1, ny+1, ny+1, 10.0);
}

int main(int argc, char ** argv)
{

  grid_type values, temp;
  int ix, iy, icycle;
  //Allocate a 2D array with indices that run 0->nx+1 and 0->ny+1
  //This replicates Fortran's arrays with variable starts and ends
  allocate_grid(&values, 0, nx+1, 0, ny+1);

  allocate_grid(&temp, 0, nx+1, 0, ny+1);
  //Assign the value 5.5 to the whole grid
  assign_grid(&values, 0, nx+1, 0, ny+1, 5.5);

  bcs(&values);
  display_result(&values);
  printf("Please press a key to advance\n");
  getchar();

  //To a C programmer, this looks backwards, but the array is using
  //Fortran ordering deliberately
  for (icycle=0;icycle<500;++icycle){
    for (iy=1;iy<=ny;++iy){
      for (ix=1;ix<=nx;++ix){
        *(access_grid(&temp, ix, iy)) = 0.25 * (
            *(access_grid(&values, ix+1, iy  )) +
            *(access_grid(&values, ix  , iy+1)) +
            *(access_grid(&values, ix-1, iy  )) +
            *(access_grid(&values, ix  , iy-1)));
      }
    }
    copy_grid(&values, &temp, 1, nx, 1, ny);
    bcs(&values);
    if(icycle%50==0){
      display_result(&values);
      printf("Please press a key to advance\n");
      getchar();
    }
  }
  deallocate_grid(&values);
  deallocate_grid(&temp);
}
