#ifndef ARRAY_H
#define ARRAY_H
#include <stdio.h>

typedef struct grid_s {
  int debug;
  int min_x;
  int min_y;
  int max_x;
  int max_y;
  size_t n_elements;
  float *data;
} grid_type;

float* access_grid(grid_type *grid, int ix, int iy);
void zero_grid(grid_type * grid);
void allocate_grid(grid_type *grid, int min_x, int max_x, int min_y, int max_y);
void assign_grid(grid_type *grid, int min_x, int max_x, int min_y, int max_y,
    float value);
void copy_grid(grid_type *grid_dest, grid_type *grid_src, int min_x, int max_x,
    int min_y, int max_y);
void copy_subgrid(grid_type *grid_dest, grid_type *grid_src, int min_x_s,
    int max_x_s, int min_y_s, int max_y_s, int min_x_d, int max_x_d,
    int min_y_d, int max_y_d);
void deallocate_grid(grid_type *grid);

#endif
