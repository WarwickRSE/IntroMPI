PROGRAM test

  USE mpi
  IMPLICIT NONE
  REAL, PARAMETER :: pi = 4.0 * ATAN(1.0)
  INTEGER, PARAMETER :: nels = 1024, nels_t=32
  INTEGER :: nproc, rank, ipp, istart, iend, iloop, errcode
  INTEGER :: iloop2
  REAL, DIMENSION(:), ALLOCATABLE :: x_axis, y_axis
  REAL, DIMENSION(:,:), ALLOCATABLE :: work, final, fsum

  CALL MPI_INIT(errcode)

  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, errcode)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, errcode)

  ipp = nels / nproc
  istart = rank * ipp + 1
  iend = istart + ipp - 1

  ALLOCATE(work(istart:iend, nels_t))
  ALLOCATE(x_axis(istart:iend), y_axis(nels_t))

  !X axis is distributed and runs from 0 to 2 Pi
  DO iloop = istart, iend
    x_axis(iloop) = 2.0*pi/REAL(nels) * REAL(iloop)
  END DO

  !Y axis is not distributed but also runs from 0 to 2 Pi
  DO iloop = 1, nels_t
    y_axis(iloop) = 2.0 * pi / REAL(nels_t) * REAL(iloop)
  END DO

  DO iloop2 = 1, nels_t
    DO iloop = istart, iend
      work(iloop,iloop2) = SIN(x_axis(iloop)) * SIN(y_axis(iloop2))
    END DO
  END DO

  !This isn't a brilliant way of getting data to rank 0
  !but it will work so long as you do a lot of computer before
  !you are finished. Might be better to write one file per processor
  !and stitch them together in post processing
  ALLOCATE(final(1:nels,nels_t), fsum(1:nels, nels_t))
  final = 0.0
  final(istart:iend,:) = work(istart:iend,:)
  CALL MPI_Reduce(final, fsum, nels*nels_t, MPI_REAL, MPI_SUM, &
      0, MPI_COMM_WORLD, errcode)

  IF (rank == 0) THEN
    OPEN(UNIT = 10 , FILE='out.dat', FORM='FORMATTED')
    WRITE(10,*) fsum
    CLOSE(10)
  END IF
  CALL MPI_FINALIZE(errcode)

END PROGRAM test
