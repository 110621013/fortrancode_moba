      program hw7_3
      implicit none

      integer :: i,j,n=6,order=3
      real*8,dimension(n) :: x,y
      real*8 :: pro,summ,lag,niho=5!niho為你要知道的x點

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


      summ = 0
      DO i = 0, order!拿前三點作圖得f(order)
        pro = y(i+1)
        DO j = 0, order
          IF(i/=j)THEN
            pro = pro*(niho - x(j+1))/(x(i+1) - x(j+1))
          ENDIF
        END DO
        summ = summ + pro
      END DO
      lag = summ

print*,"拿x=1,2,3,5作圖得f(3)"
write(*,"(a5,f10.6,a6,f12.6)")"在x=",niho,"f(x)=",lag

      stop
      end

