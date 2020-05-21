      program bubble
      implicit none

      integer :: num
      integer :: a(5)=(/8,3,7,2,6/)
      integer :: i,j,temp

      print*,(a(i),i=1,5)

      do i=4,1,-1
        do j=1,i
          if(a(j)>a(j+1))then
          temp=a(j+1)
          a(j+1)=a(j)
          a(j)=temp
          endif
        enddo
      enddo

      print*,(a(i),i=1,5)

      stop
      end

