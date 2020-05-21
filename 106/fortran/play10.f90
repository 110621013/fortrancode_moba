      program FindGrade
      implicit none
      
      integer :: counter
      integer :: ans=0
      do counter = 1,100,1
      ans=ans+counter
      enddo
      write(*,*)ans

      stop
      end
