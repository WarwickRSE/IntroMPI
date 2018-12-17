#include <mpi.h>
#include <stdio.h>
#include <math.h>

#define NELS 1024
#define NELS_T 32

int main(int argc, char **argv)
{
  const float pi = 3.14159;
  int nproc, rank, ipp, istart, iend, iloop, iloop2;

  MPI_Init(&argc, &argv);

  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  ipp = NELS / nproc;
  istart = rank * ipp;
  iend = istart + ipp;

  float work[iend-istart][NELS_T];
  float x_axis[iend-istart];
  float y_axis[NELS_T];

  //X axis is distributed and runs from 0 to 2 Pi
  for(iloop = istart; iloop < iend ; ++iloop){
    x_axis[iloop-istart] = 2.0*pi/(float)NELS * (float)iloop;
  }

  //Y axis is not distributed but also runs from 0 to 2 Pi
  for(iloop = 0;  iloop < NELS_T; ++iloop){
    y_axis[iloop] = 2.0 * pi / (float)NELS_T * (float)iloop;
  }

  for(iloop = 0; iloop < NELS_T; ++iloop){
    for(iloop2 = istart; iloop2<iend; ++iloop2){
      work[iloop2-istart][iloop] = sin(x_axis[iloop2-istart]) 
          * sin(y_axis[iloop]);
    }
  }

  /*This isn't a brilliant way of getting data to rank 0
  but it will work so long as you do a lot of computer before
  you are finished. Might be better to write one file per processor
  and stitch them together in post processing*/

  float final[NELS][NELS_T], fsum[NELS][NELS_T];
  for(iloop = 0; iloop < NELS_T; ++iloop){
    for(iloop2 = 0; iloop2<NELS; ++iloop2){
      final[iloop2][iloop] = 0.0;
    }
  }
  for(iloop = 0; iloop < NELS_T; ++iloop){
    for(iloop2 = istart; iloop2<iend; ++iloop2){
      final[iloop2][iloop] = work[iloop2-istart][iloop];
    }
  }
  MPI_Reduce(final, fsum, NELS*NELS_T, MPI_FLOAT, MPI_SUM,
      0, MPI_COMM_WORLD);

  if (rank == 0) {
    FILE *f;
    f = fopen("out.dat", "w+");
    for(iloop = 0; iloop < NELS_T; ++iloop){
      for(iloop2 = 0; iloop2<NELS; ++iloop2){
        fprintf(f,"%f ", fsum[iloop2][iloop]);
      }
      fprintf(f,"\n");
    }
    fclose(f);
  }

  MPI_Finalize();

}
