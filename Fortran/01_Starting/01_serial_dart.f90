MODULE support

  IMPLICIT NONE

  !Use SELECTED_REAL_KIND to get a kind value for a double precision number
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)
  INTEGER, PARAMETER :: its = 10000000

  CONTAINS

  SUBROUTINE seed_rng
    INTEGER :: rand_size, time
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed

    !Fortran RNG seeding is a bit strange. This works
    CALL RANDOM_SEED(size=rand_size)
    ALLOCATE(seed(1:rand_size))
    CALL SYSTEM_CLOCK(time)
    seed = time
    CALL RANDOM_SEED(put=seed)
    DEALLOCATE(seed)
  END SUBROUTINE seed_rng



  FUNCTION get_random_in_range() RESULT(rnum)

    REAL(dp) :: rnum

    CALL RANDOM_NUMBER(rnum)
    rnum = rnum * 2.0_dp - 1.0_dp
  END FUNCTION get_random_in_range

END MODULE support

PROGRAM main
  USE support

  INTEGER :: count = 0, index
  REAL(dp) :: d1, d2

  CALL seed_rng
  DO index = 0, its - 1
    ! random numbers between ±1
    d1=get_random_in_range()
    d2=get_random_in_range()
    if (d1*d1 + d2*d2 <= 1) count = count + 1
  END DO

  PRINT *, count, its
END PROGRAM main
