      program play7.f90
      implicit none
      open(unit=10,file="data.txt")
      write(10,"(a20)")"how are you??"
      close(10)

      stop
      end
