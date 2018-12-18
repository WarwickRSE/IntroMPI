
PROGRAM main

  USE MPI
  IMPLICIT NONE
  INTEGER :: rank, nproc, rank_right, rank_left, rank_recv
  INTEGER :: errcode, rank_recv5, rank5, rank_2left, rank_2right
  INTEGER, DIMENSION(2) :: requests

  CALL MPI_INIT(errcode)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, errcode)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, errcode)

  requests = MPI_REQUEST_NULL

  rank_left = rank -1
  !Ranks run from 0 to nproc-1, so wrap the ends around to make a loop
  IF(rank_left == -1) rank_left = nproc-1
  rank_right = rank + 1
  IF(rank_right == nproc) rank_right = 0

  rank_2left = rank_left -1
  IF(rank_2left == -1) rank_2left = nproc-1
  rank_2right = rank_right + 1
  IF(rank_2right == nproc) rank_2right = 0

  rank5 = 70 + rank
  CALL MPI_Isend(rank, 1, MPI_INTEGER, rank_right, 100, MPI_COMM_WORLD, &
      requests(1), errcode)
  !Barrier to ensure we're really mixing the calls
  CALL MPI_Barrier(MPI_COMM_WORLD, errcode)
  CALL MPI_Isend(rank5, 1, MPI_INTEGER, rank_2right, 50, MPI_COMM_WORLD, &
      requests(2), errcode)
  !Barrier to ensure we're really mixing the calls
  CALL MPI_Barrier(MPI_COMM_WORLD, errcode)

  !Receive the second send first (tag 50)
  CALL MPI_Recv(rank_recv5, 1, MPI_INTEGER, rank_2left, 50, MPI_COMM_WORLD, &
       MPI_STATUS_IGNORE, errcode)
  CALL MPI_Recv(rank_recv, 1, MPI_INTEGER, rank_left, 100, MPI_COMM_WORLD, &
       MPI_STATUS_IGNORE, errcode)
  !We don't actually care about sending being finished here, but may as well
  !wait for both
  CALL MPI_Waitall(2, requests, MPI_STATUSES_IGNORE, errcode)

  PRINT ('("Rank ",I3," has received value ", I3, " from rank ", I3)'), &
      rank, rank_recv, rank_left
  PRINT ('("Rank ",I3," has also received value ", I3, " from rank ", I3)'), &
      rank, rank_recv5, rank_2left


  CALL MPI_FINALIZE(errcode)
END PROGRAM main
