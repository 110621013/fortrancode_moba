      program hw7_5_1
      implicit none

      integer :: i,n=6
      real*8 :: sumx,sumy,sumxy,sumx2,st,sr,xm,ym,a0,a1,syx,r2
      real,dimension(n) :: x,y

      x(1)=0
      x(2)=8
      x(3)=16
      x(4)=24
      x(5)=32
      x(6)=40

      y(1)=14.621
      y(2)=11.843
      y(3)=9.87
      y(4)=8.418
      y(5)=7.305
      y(6)=6.413


      DO i = 1, n
        sumx = sumx + x(i)
        sumy = sumy + y(i)
        sumxy = sumxy + x(i)*y(i)
        sumx2 = sumx2 + x(i)*x(i)
      END DO
      xm = sumx/n
      ym = sumy/n
      a1 = (n*sumxy - sumx*sumy)/(n*sumx2 - sumx*sumx)
print*,n*sumxy,sumx*sumy,n*sumx2,sumx*sumx
      a0 = ym - a1*xm
      DO i = 1, n
        st = st + (y(i) - ym)**2
        sr = sr + (y(i) - a1*x(i) - a0)**2
      END DO
print*,st,sr
      syx = sqrt((sr/(n - 2)))
      r2 = (st - sr)/st

print*,"a1=",a1
print*,"a0=",a0
print*,"syx=",syx
print*,"r2=",r2


      stop
      end

