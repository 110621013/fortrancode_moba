      program test
      implicit none
      
      open(unit=10,file="106601015.txt")
      write(10,"(a30)")"today is a good day!"
      close(10)

      stop
      end
      
