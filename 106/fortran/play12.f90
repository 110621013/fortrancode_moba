      program dowhile
      implicit none
      
      integer :: ans=0
      
      open(unit=100,file="number.txt")
      
      do while(ans<=19)
      ans=ans+1
      write(100,*)ans
      enddo

      close(100)

      stop
      end
      
