      program Array
      implicit none 
  
      integer  :: i,j
      character(len=10)  ::  color(5)
!---------------------------------------
      open(unit=99,file="Array_color.txt")

      do i = 1 ,5 ,1
       read(99,*) color(i)
      enddo

      print*,'Color : ' ,(color(j),j=1,5)
!--------------------------------------
      stop 
      end

