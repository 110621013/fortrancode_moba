      program  open_file
      implicit none
     
      integer :: num = 10
!----------------------------
      open(unit=10,file="binary.dat",form="unformatted",access="direct",recl=4)

      write(10,rec=1)num
      close(10)
!----------------------------
      stop
      end
 
