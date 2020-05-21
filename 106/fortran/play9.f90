      program test
      implicit none
      
      integer :: num=11

      open(unit=11,file="data.txt",form="unformatted",access="direct",recl=4)

      write(11,rec=1)num
      close(11)

      stop
      end

