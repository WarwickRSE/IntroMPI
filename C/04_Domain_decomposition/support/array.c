#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "array.h"

/*This is a quick wrapper to create a friendly runtime allocatable array
with Fortran like ordering. It uses an index function to give you a reference
to the data*/

float* access_grid(grid_type *grid, int ix, int iy)
{
  //This uses Fortran ordering. Bit odd, but C does give you a choice
  return grid->data +
      ((ix-grid->min_x) + (iy-grid->min_y) * (grid->max_x-grid->min_x+1));
}

void zero_grid(grid_type * grid)
{
  memset(grid->data, 0, grid->n_elements * sizeof(float));
}

void allocate_grid(grid_type *grid, int min_x, int max_x, int min_y, int max_y)
{
  grid->debug = 0;
  grid->min_x = min_x;
  grid->max_x = max_x;
  grid->min_y = min_y;
  grid->max_y = max_y;
  grid->n_elements = (size_t)(grid->max_x - grid->min_x+1)
      * (size_t)(grid->max_y-grid->min_y+1);
  grid->data = (float*) malloc(grid->n_elements * sizeof(float));
  zero_grid(grid);
}

void assign_grid(grid_type *grid, int min_x, int max_x, int min_y, int max_y,
    float value)
{
  int ix, iy;
  for (iy = min_y; iy <= max_y;++iy){
    for (ix = min_x; ix <= max_x;++ix){
      *(access_grid(grid, ix, iy)) = value;
    }
  }
}

void copy_grid(grid_type *grid_dest, grid_type *grid_src, int min_x, int max_x,
    int min_y, int max_y)
{
  int ix, iy;
  for (iy = min_y; iy <= max_y;++iy){
    for (ix = min_x; ix <= max_x;++ix){
      *(access_grid(grid_dest, ix, iy)) = *(access_grid(grid_src, ix, iy));
    }
  }
}

void copy_subgrid(grid_type *grid_dest, grid_type *grid_src, int min_x_s,
    int max_x_s, int min_y_s, int max_y_s, int min_x_d, int max_x_d,
    int min_y_d, int max_y_d)
{
  int ix, iy, ixdest, iydest;
  assert(max_x_s - min_x_s == max_x_d - min_x_d);
  assert(max_y_s - min_y_s == max_y_d - min_y_d);

  ixdest = min_x_d;
  iydest = min_y_d;
  for (iy = min_y_s; iy <= max_y_s;++iy){
    for (ix = min_x_s; ix <= max_x_s;++ix){
      *(access_grid(grid_dest, ixdest, iydest)) =
          *(access_grid(grid_src, ix, iy));
      if(++ixdest > max_x_d) {
        ++iydest;
        ixdest = min_x_d;
      }
    }
  }
}

void deallocate_grid(grid_type *grid)
{
  free(grid->data);
  grid->data = NULL;
}
