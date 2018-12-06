#include<stdio.h>
#include "array.h"
#include "display.h"

#define NVALS 3

//This routine displays the output. It isn't a part of this course. Annotation
//is only for general interest
void display_result(grid_type *arr){

  char clear[4] = "\x01B[2J";
  char *colour[5] = {"\x01B[34m", "\x01B[39m", "\x01B[31m"};
  char vals[NVALS] = "*.+";
  int ix, iy, index;
  //Special string to clear screen using VT100 terminal codes, see
  //(http://wiki.bash-hackers.org/scripting/terminalcodes)
  printf("%s",clear);
  //Outer array is flipped because screen indexes work from top left
  //Everything else works from bottom left. Also remember Fortran ordering
  for (iy = arr->max_y-1;iy>=arr->min_y+1;--iy){
    for(ix = arr->min_x+1;ix<=arr->max_x-1;++ix){
      //Get the symbol and colour for the value in this cell
      //Colours are more VT100 terminal codes
      index = (int)(*(access_grid(arr, ix, iy))/10.0 * (float)(NVALS-1) + 0.5);
      //Write out the special VT100 colour control code and symbol
      printf("%s%c ", colour[index], vals[index]);
      //version without colour code
      //printf("%c ", vals[index]);
    }
    printf("\n");
  }
  //Set terminal colours back to default
  printf("\x01B[39m");
}

//This routine displays the output. It isn't a part of this course. Annotation
//is only for general interest
void display_debug(grid_type *arr){

  char clear[4] = "\x01B[2J";
  char *colour[5] = {"\x01B[34m", "\x01B[39m", "\x01B[31m"};
  char vals[NVALS] = "*.+";
  int ix, iy, index;
  //Outer array is flipped because screen indexes work from top left
  //Everything else works from bottom left. Also remember Fortran ordering
  for (iy = arr->max_y;iy>=arr->min_y;--iy){
    for(ix = arr->min_x;ix<=arr->max_x;++ix){
      //Get the symbol and colour for the value in this cell
      index = (int)(*(access_grid(arr, ix, iy))/10.0 * (float)(NVALS-1) + 0.5);
      printf("%1d ", index);
    }
    printf("\n");
  }
}
