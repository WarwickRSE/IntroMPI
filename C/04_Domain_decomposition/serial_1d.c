#include <stdio.h>
#include <stdlib.h>

#define NVALS 3
#define NX 40

//This routine displays the output. It isn't a part of this course. Annotation
//is only for general interest
void display_result(float *arr){

  char clear[4] = "\x01B[2J";
  char *colour[5] = {"\x01B[34m", "\x01B[39m", "\x01B[31m"};
  char vals[NVALS] = "*.+";
  int ix, index;
  //Special string to clear screen using VT100 terminal codes, see
  //(http://wiki.bash-hackers.org/scripting/terminalcodes)
  printf("%s",clear);
  //Outer array is flipped because screen indexes work from top left
  //Everything else works from bottom left. Also remember Fortran ordering
  for(ix = 1;ix<=NX;++ix){
    //Get the symbol and colour for the value in this cell
    //Colours are more VT100 terminal codes
    index = (int)((arr[ix])/10.0 * (float)(NVALS-1) + 0.5);
    //Write out the special VT100 colour control code and symbol
    printf("%s%c ", colour[index], vals[index]);
    //version without colour code
    //printf("%c ", vals[index]);
  }
    printf("\n");
  //Set terminal colours back to default
  printf("\x01B[39m");
}

void bcs(float *data)
{
  //Assign the boundary conditions. 1.0 along the left and bottom
  //10.0 along the right and top.
  data[0] = 1.0;
  data[NX+1] = 10.0;
}

int main(int argc, char ** argv)
{

  float *values, *temp;
  int ix, icycle;

  values = malloc(sizeof(float) * (NX+2));
  temp = malloc(sizeof(float) * (NX+2));

  for (ix=1;ix<=NX;++ix){values[ix] = 5.5;}

  bcs(values);
  display_result(values);
  printf("Please press a key to advance\n");
  getchar();

  for (icycle=0;icycle<500;++icycle){
    for (ix=1;ix<=NX;++ix){
      temp[ix] = 0.5 * (values[ix-1] + values[ix+1]);
    }
    for (ix=1;ix<=NX;++ix){
      values[ix] = temp[ix];
    }

    bcs(values);
    if(icycle%50==0){
      display_result(values);
      printf("Please press a key to advance\n");
      getchar();
    }
  }
  free(values);
  free(temp);
}
