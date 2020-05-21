      program hw7_2
      implicit none

      integer :: i,j,n=6
      real*8,dimension(n) :: x,y,lny
      real*8 :: sumx,sumy,sumxy,sumx2,st,sr,xm,ym,a0,a1,syx,r2


      x(1)=0.4
      x(2)=0.8
      x(3)=1.2
      x(4)=1.6
      x(5)=2
      x(6)=2.3

      y(1)=800
      y(2)=975
      y(3)=1500
      y(4)=1950
      y(5)=2900
      y(6)=3600

      do i=1,n
        lny(i)=log(y(i))
      enddo
print*,lny

print*,"以ln(y)-x作擬合直線"
      DO i = 1, n
        sumx = sumx + x(i)
        sumy = sumy + lny(i)
        sumxy = sumxy + x(i)*lny(i)
        sumx2 = sumx2 + x(i)*x(i)
      END DO
      xm = sumx/n
      ym = sumy/n
      a1 = (n*sumxy - sumx*sumy)/(n*sumx2 - sumx*sumx)
print*,n*sumxy,sumx*sumy,n*sumx2,sumx*sumx
      a0 = ym - a1*xm
      DO i = 1, n
        st = st + (lny(i) - ym)**2
        sr = sr + (lny(i) - a1*x(i) - a0)**2
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
