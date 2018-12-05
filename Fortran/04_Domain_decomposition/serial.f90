MODULE display

  IMPLICIT NONE

  !Information about the global array
  INTEGER, PARAMETER :: nx = 20, ny = 20
  REAL, DIMENSION(0:nx+1, 0:ny+1) :: values, temp

  CONTAINS

  !This routine displays the output. It isn't a part of this course. Annotation
  !is only for general interest. Note that normally you'd use a library like
  !ncurses (https://en.wikipedia.org/wiki/Ncurses) to do the terminal trickery
  !that I'm doing here. This is just for maximum compatability
  SUBROUTINE display_result(array)

    REAL, DIMENSION(0:,0:), INTENT(IN) :: array
    CHARACTER(LEN=3) :: clrstr = '[2J'
    CHARACTER(LEN=5), DIMENSION(3) :: colours = (/'[34m', '[39m', '[31m'/)
    CHARACTER(LEN=1), DIMENSION(3) :: vals = (/'*', '.', '+'/)
    INTEGER, DIMENSION(2) :: sizes
    INTEGER :: ix, iy, index

    !Special string to clear screen using VT100 terminal codes, see
    !(http://wiki.bash-hackers.org/scripting/terminalcodes)
    WRITE(*,'(A)') CHAR(27) // TRIM(clrstr)

    sizes = SHAPE(array)
    !Outer array is flipped because screen indexes work from top left
    !Everything else works from bottom left
    DO iy = sizes(2) - 2, 1, -1
      DO ix = 1, sizes(1) - 2
        !Get the symbol and colour for the value in this cell
        !Colours are more VT100 terminal codes
        index = NINT(array(ix,iy)/10.0 * REAL(SIZE(vals)-1))+1

        !Write out the special VT100 colour control code and symbol
        WRITE(*,'(A,A)', ADVANCE='NO') ACHAR(27) // TRIM(colours(index)) , &
            vals(index) // " "
        !Version without colour code
!        WRITE(*,'(A)', ADVANCE='NO') vals(index) // " "
      END DO
      WRITE(*,*) ""
    END DO
    !Set terminal colours back to default
    WRITE(*,'(A)', ADVANCE='NO') ACHAR(27) // TRIM('[39m')

  END SUBROUTINE display_result

  !Routine to fill ghost cells with boundary values
  !In this case those are constants, but that is not
  !in general true for all boundary conditions
  !So need to have the function
  SUBROUTINE bcs(array)

    REAL, DIMENSION(0:, 0:), INTENT(INOUT) :: array

    array(0,:) = 1.0
    array(nx+1,:) = 10.0
    array(:,0) = 1.0
    array(:, ny+1) = 10.0

  END SUBROUTINE bcs

END MODULE display

PROGRAM serial

  USE display

  IMPLICIT NONE

  INTEGER :: ix, iy, icycle


  !Set initial default value
  values = 5.5
  !Apply boundary conditions
  CALL bcs(values)

  PRINT *,'Please press a key to start iterating'
  READ(*,*)

  !Display initial conditions
  CALL display_result(values)
  PRINT *,'Please press a key to advance'
  READ(*,*)

  !Now iterate
  DO icycle = 1, 500

    !Operate on the data
    DO iy = 1, ny
      DO ix = 1, nx
        temp(ix,iy) = 0.25 * (values(ix+1,iy) + &
             values(ix,iy+1) + values(ix-1,iy) + &
             values(ix,iy-1))
      END DO
    END DO
    values(1:nx,1:ny) = temp(1:nx,1:ny)

    !Now apply the boundary conditions
    CALL bcs(values)

    !And output
    IF (MOD(icycle,50) == 0) THEN
      CALL display_result(values)
      PRINT *,'Please press a key to advance'
      READ (*,*)
    END IF
  END DO

END PROGRAM serial
