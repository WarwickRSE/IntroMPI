
PROGRAM simple_print

  USE mpi
  IMPLICIT NONE
  INTEGER :: ierr !Fortran MPI commands have an integer success parameter

  CALL MPI_Init(ierr)
  PRINT *,"Multiprocessor code"
  CALL MPI_Finalize(ierr)

END PROGRAM simple_print
