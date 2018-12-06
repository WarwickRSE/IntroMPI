#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <mpi.h>

void seed_rng(){
  time_t t;
  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  srand((unsigned) time(&t) + rank);
}


  void dispatcher_fn(int total_work)
    {
    int work_index, result, nproc, inflight, dat;
    MPI_Status stat;
    int dests[total_work], packages[total_work], results[total_work];
    double start_times[total_work], end_times[total_work];

    MPI_Comm_size(MPI_COMM_WORLD, &nproc);
    work_index = 1; // Start from work package 0
    inflight = 0; //This counts how many inflight work packages there are
      //When it reaches zero they are all done. Since there are a fixed
      //number here it could be done in other ways but this works even if
      //you don't always know how much work there is to be done until it is
      //all finished

    for(;;) {
      //Wait to receive any data. On the first pass tag will be 0 which is
      //Just the workers saying that they are ready
      MPI_Recv(&result, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, 
          MPI_COMM_WORLD, &stat);

      //Use the tag to say where this result should be stored
      //This is OK until you have enough results that they can't be stored
      //in a 32 bit integer but for a test case it's good enough
      //Tag = 0 is first check in rather than a returned result
      //You can get various bits of information from the stat struct, but
      //The ones that we use here are 
      //stat.MPI_TAG - The tag value for the received message
      //stat.MPI_SOURCE - The rank number of the process that sent the message
      //We have to get this information from the status variable because 
      //we received the message with MPI_ANY_SOURCE and MPI_ANY_TAG
      if (stat.MPI_TAG > 0) {
        inflight --; // You have one fewer inflight transaction
        results[stat.MPI_TAG-1] = result;
        end_times[stat.MPI_TAG-1] = MPI_Wtime();
      }

      //If you've still got work to give out then give it out
      if (work_index <= total_work) {
        //Now pick a new work result and send it to the worker that just reported
        //that it had finished. Note that tag is now work_index so that it can be
        //sent back by the client to tie up the work
        dests[work_index-1] = stat.MPI_SOURCE;

        //Normally you'd get work work packages from somewhere external, but
        //here just pick a random number
        packages[work_index-1] = rand()%100;

        //Record the start time
        start_times[work_index-1] = MPI_Wtime();

        //Send the work package with the tag of the work index to the source
        //of the previously received message
        MPI_Send(&packages[work_index-1], 1, MPI_INT, stat.MPI_SOURCE,
            work_index, MPI_COMM_WORLD);
        //Move to next work index
        work_index++;
        //Count new inflight work package
        inflight++;
      } else {
        //No more work to do so shut down the worker. Here, this is just
        //Sending a message with a zero tag
        dat = -1;
        MPI_Send(&dat, 1, MPI_INT, stat.MPI_SOURCE, 0, MPI_COMM_WORLD);
      }
      if (inflight == 0) break;
    }

    for(work_index = 0; work_index < total_work; ++work_index){
      printf("Package %3i with payload %3i was processed by %3i and gave \
           result %5i in %5.4g seconds\n",
          work_index, packages[work_index], dests[work_index], 
          results[work_index], end_times[work_index] - start_times[work_index]);
    }
  }


  void worker_fn(void)
    {
    MPI_Status stat;
    int package, result, tag, dat;
    double start_time, wait_time;

    //First just call home to say "I'm here and I'm waiting"
    //So send -1 message and 0 tag to rank 0
    dat = -1;
    MPI_Send(&dat, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);

    for(;;){
      //Receive the work package from rank 0
      MPI_Recv(&package, 1, MPI_INT, 0, MPI_ANY_TAG,
          MPI_COMM_WORLD, &stat);

      //Need to have a termination condition, here tag = 0
      if (stat.MPI_TAG == 0) break;

      //Do the work
      result = package * 2;

      //Now wait for as many microseconds as are in the work package
      //value. This is to simulate different work packages having different
      //difficulties
      wait_time = (float)package/1000.0;
      start_time = MPI_Wtime();//Get start time
      while(MPI_Wtime()-start_time < wait_time){;}

      //Send the result back using the same tag
      MPI_Send(&result, 1, MPI_INT, 0 , stat.MPI_TAG, MPI_COMM_WORLD);
    }
  }


void main(int argc, char** argv)
{
  int rank, nproc;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  if (nproc == 1) {
    printf("This example requires more than 1 processor\n");
    MPI_Abort(MPI_COMM_WORLD, 0);
  }

  if (rank == 0) {
    //Do 100 units of work
    dispatcher_fn(100);
  } else {
    worker_fn();
  }

  MPI_Finalize();
}
