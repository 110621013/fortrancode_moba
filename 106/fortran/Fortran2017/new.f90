      PROGRAM new 
      IMPLICIT NONE

       INTEGER           :: a
       CHARACTER(len=20) :: b
       REAL              :: c
       REAL,PARAMETER    :: pi = 3.1415926 
!-------------------------------------------------------
       a  = 100
       b  = "class tutor"
       c  = 12.3
!   pi = 3.1415926
       WRITE(*,"(I10)")   a
       WRITE(*,"(A20)")   b
!   WRITE(*,"(F4.1)")  c
       WRITE(*,100)       c
       WRITE(*,"(F10.7)") pi
        
       100 FORMAT(1X,F5.2)
      
      END
