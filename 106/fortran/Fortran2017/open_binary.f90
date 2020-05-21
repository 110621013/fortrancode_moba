      program  open2_file
      implicit none
     
      integer :: num = 10
!----------------------------
      open(unit=10,file="test.txt",form="formatted")

      write(10,*)num
      close(10)
!----------------------------
      stop
      end
 
