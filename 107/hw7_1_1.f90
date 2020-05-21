      program hw7_1_1
      implicit none

      integer :: i,n=9
      real*8 :: sumx,sumy,sumxy,sumx2,st,sr,xm,ym,a0,a1,syx,r2
      real,dimension(n) :: xi,yi

      xi(1)=1
      xi(2)=2
      xi(3)=3
      xi(4)=4
      xi(5)=5
      xi(6)=6
      xi(7)=7
      xi(8)=8
      xi(9)=9

      yi(1)=1
      yi(2)=1.5
      yi(3)=2
      yi(4)=3
      yi(5)=4.5
      yi(6)=5
      yi(7)=8
      yi(8)=10
      yi(9)=13


      DO i = 1, n
        sumx = sumx + xi(i)
        sumy = sumy + yi(i)
        sumxy = sumxy + xi(i)*yi(i)
        sumx2 = sumx2 + xi(i)*xi(i)
      END DO
      xm = sumx/n
      ym = sumy/n
      a1 = (n*sumxy - sumx*sumy)/(n*sumx2 - sumx*sumx)
print*,n*sumxy,sumx*sumy,n*sumx2,sumx*sumx
      a0 = ym - a1*xm
      DO i = 1, n
        st = st + (yi(i) - ym)**2
        sr = sr + (yi(i) - a1*xi(i) - a0)**2
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

