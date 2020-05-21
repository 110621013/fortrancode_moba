      program loop
      implicit none
  
      integer :: i
      integer :: sum
!-------------------------------------
      sum = 0
      do i = 1 ,100 , 2 
        sum = sum + i
      enddo

      print*,'Sum = ',sum
!------------------------------------
      stop
      end
