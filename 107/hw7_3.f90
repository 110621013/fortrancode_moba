      program hw7_3
      implicit none

      integer :: i,j,n=6
      real*8,dimension(n) :: x,y
      real*8,dimension(n,n) :: cha

      x(1)=1
      x(2)=2
      x(3)=3
      x(4)=5
      x(5)=7
      x(6)=8

      y(1)=3
      y(2)=6
      y(3)=19
      y(4)=99
      y(5)=291
      y(6)=444

!f=b0+b1(x-x0)+b2(x-x0)(x-x1)+........
      do j=1,n-1
        cha(1,j)=(y(j+1)-y(j))/(x(j+1)-x(j))
      enddo
      
      do i=2,n-1
        do j=1,n-i
          cha(i,j)=(cha(i-1,j+1)-cha(i-1,j))/(x(j+i)-x(j))
!print*,"owo",(cha(i-1,j+1)-cha(i-1,j)),(x(j+i)-x(j))
        enddo
      enddo

do i=1,n
do j=1,n
print*,cha(i,j)
enddo
print*,"666"
enddo

do i=1,4
write(*,"(a1,i1,a1,f10.7)"),"b",i,"=",cha(i,1)
enddo

      stop
      end
