MODULE fns

  USE MPI
  IMPLICIT NONE

  CONTAINS

  !> Function to seed random number generator
  SUBROUTINE seed_rng
    INTEGER :: rand_size, time, rank, ierr
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed

    CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    !Fortran RNG seeding is a bit strange. This works
    CALL RANDOM_SEED(size=rand_size)
    ALLOCATE(seed(1:rand_size))
    CALL SYSTEM_CLOCK(time)
    seed = time + rank !Add rank to make random across processors
    CALL RANDOM_SEED(put=seed)
    DEALLOCATE(seed)
  END SUBROUTINE seed_rng



  SUBROUTINE dispatcher_fn(total_work)
    INTEGER, INTENT(IN) :: total_work
    INTEGER :: work_index, result, errcode, nproc, inflight
    INTEGER, DIMENSION(MPI_STATUS_SIZE) :: stat
    INTEGER, DIMENSION(total_work) :: dests, packages, results
    REAL(KIND(1.D0)), DIMENSION(total_work) :: start_times, end_times
    REAL :: rn

    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, errcode)
    work_index = 1 ! Start from work package 1
    inflight = 0 !This counts how many inflight work packages there are
      !When it reaches zero they are all done. Since there are a fixed
      !number here it could be done in other ways but this works even if
      !you don't always know how much work there is to be done until it is
      !all finished

!    CALL seed_rng()
    DO
      !Wait to receive any data. On the first pass tag will be 0 which is
      !Just the workers saying that they are ready
      !Note that "stat" is just an array, that we acccess by index later
      CALL MPI_RECV(result, 1, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, &
          MPI_COMM_WORLD, stat, errcode)

      !Use the tag to say where this result should be stored
      !This is OK until you have enough results that they can't be stored
      !in a 32 bit integer but for a test case it's good enough
      !Tag = 0 is first check in rather than a returned result
      !You can get various bits of information from the stat array, but
      !The ones that we use here are 
      !stat(MPI_TAG) - The tag value for the received message
      !stat(MPI_SOURCE) - The rank number of the process that sent the message
      !We have to get this information from the status variable because 
      !we received the message with MPI_ANY_SOURCE and MPI_ANY_TAG
      IF (stat(MPI_TAG) > 0) THEN
         inflight = inflight - 1 ! You have one fewer inflight transaction
         results(stat(MPI_TAG)) = result
         end_times(stat(MPI_TAG)) = MPI_WTIME()
      END IF

      !If you've still got work to give out then give it out
      IF (work_index <= total_work) THEN
        !Now pick a new work result and send it to the worker that just reported
        !that it had finished. Note that tag is now work_index so that it can be
        !sent back by the client to tie up the work
        dests(work_index) = stat(MPI_SOURCE)

        !Normally you'd get work work packages from somewhere external, but
        !here just pick a random number
        CALL RANDOM_NUMBER(rn)
        packages(work_index) = FLOOR(rn * 100.0)

        !Record the start time
        start_times(work_index) = MPI_WTIME()

        !Send the work package with the tag of the work index to the source
        !of the previously received message
        CALL MPI_SEND(packages(work_index), 1, MPI_INTEGER, stat(MPI_SOURCE), &
            work_index, MPI_COMM_WORLD, errcode)
        !Move to next work index
        work_index = work_index + 1
        !Count new inflight work package
        inflight = inflight + 1
      ELSE
        !No more work to do so shut down the worker. Here, this is just
        !Sending a message with a zero tag
        CALL MPI_SEND(-1, 1, MPI_INTEGER, stat(MPI_SOURCE), &
            0, MPI_COMM_WORLD, errcode)
      END IF
      IF (inflight == 0) EXIT
    END DO

    DO work_index = 1, total_work
      PRINT '("Package ",I3," with payload ", I3, " was processed by ", I3, &
          & " and gave result ", I5, " in ", F5.4, " seconds")', &
          work_index, packages(work_index), dests(work_index), &
          results(work_index), end_times(work_index) - start_times(work_index)
    END DO
  END SUBROUTINE dispatcher_fn


  SUBROUTINE worker_fn
    INTEGER, DIMENSION(MPI_STATUS_SIZE) :: stat
    INTEGER :: package, result, tag, errcode
    REAL(KIND=KIND(1.D0)) :: start_time, wait_time

    CALL seed_rng() !Set up the random number generator

    !First just call home to say "I'm here and I'm waiting"
    !So send -1 message and 0 tag to rank 0
    CALL MPI_SEND(-1, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, errcode)

    DO
      !Receive the work package from rank 0
      CALL MPI_RECV(package, 1, MPI_INTEGER, 0, MPI_ANY_TAG, &
          MPI_COMM_WORLD, stat, errcode)

      !Need to have a termination condition, here tag = 0
      IF (stat(MPI_TAG) == 0) EXIT

      !Do the work
      result = package * 2

      !Now wait for a random time. This is to simulate the fact that
      !in most real problems like this not all of the tasks are as easy
      !as each other 
      CALL RANDOM_NUMBER(wait_time)
      wait_time = wait_time * 0.1 !Maximum of 0.1 second wait
      wait_time = REAL(package, KIND(1.D0))/1000.0
      start_time = MPI_WTIME()!Get start time
      DO WHILE(MPI_WTIME()-start_time < wait_time)
      END DO

      !Send the result back using the same tag
      CALL MPI_SEND(result, 1, MPI_INTEGER, 0 , stat(MPI_TAG), &
          MPI_COMM_WORLD, errcode)
    END DO

  END SUBROUTINE worker_fn


END MODULE fns



PROGRAM main

  USE MPI
  USE fns
  IMPLICIT NONE
  INTEGER :: rank, nproc, errcode

  CALL MPI_INIT(errcode)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, errcode)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, errcode)

  IF (nproc == 1) THEN
    PRINT *,'This example requires more than 1 processor'
    CALL MPI_ABORT(MPI_COMM_WORLD, 0, errcode)
  END IF

  IF (rank == 0) THEN
    !Do 100 units of work
    CALL dispatcher_fn(100)
  ELSE
    CALL worker_fn
  END IF

  CALL MPI_FINALIZE(errcode)
END PROGRAM main
