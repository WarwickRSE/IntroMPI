
PROGRAM main

  USE MPI
  IMPLICIT NONE
  INTEGER :: rank, nproc, rank_right, rank_left, rank_recv, errcode
  INTEGER, DIMENSION(MPI_STATUS_SIZE) :: stat

  CALL MPI_INIT(errcode)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, errcode)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, errcode)

  rank_left = rank -1
  !Ranks run from 0 to nproc-1, so wrap the ends around to make a loop
  IF(rank_left == -1) rank_left = nproc-1
  rank_right = rank + 1
  IF(rank_right == nproc) rank_right = 0

  CALL MPI_Sendrecv(rank,     1, MPI_INTEGER, rank_right, 100, &
                    rank_recv,1, MPI_INTEGER, rank_left , 100, &
                    MPI_COMM_WORLD, stat, errcode)

  CALL MPI_Send(rank, 1, MPI_INTEGER, rank_right, 100, MPI_COMM_WORLD, errcode)
  CALL MPI_Recv(rank_recv, 1, MPI_INTEGER, rank_left, 100, MPI_COMM_WORLD, &
      stat, errcode)

  PRINT ('("Rank ",I3," has received value ", I3, " from rank ", I3)'), &
      rank, rank_recv, rank_left

  CALL MPI_FINALIZE(errcode)
END PROGRAM main
