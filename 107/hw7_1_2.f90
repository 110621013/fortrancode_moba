      program hw7_1_2
      implicit none

      integer :: i,j,k,l,n=9,order=3,p,z
      real*8 :: summ,big,dum,Dummy,factor,syx,r2,st,sr,sumy,ym
      real*8,dimension(n) :: x,y
      real*8,dimension(order) :: b,s,ans
      real*8,dimension(order,order) :: a

      x(1)=1
      x(2)=2
      x(3)=3
      x(4)=4
      x(5)=5
      x(6)=6
      x(7)=7
      x(8)=8
      x(9)=9

      y(1)=1
      y(2)=1.5
      y(3)=2
      y(4)=3
      y(5)=4.5
      y(6)=5
      y(7)=8
      y(8)=10
      y(9)=13
!print*,x,y

      DO i = 1, order !+ 1
        DO j = 1, i
          k = i+j-2
          summ = 0
          DO l= 1, n
            summ = summ + x(l)**k
          END DO
          a(i,j) = summ
          a(j,i) = summ
        END DO
        summ = 0
        DO l = 1, n
          summ = summ + y(l)*x(l)**(i-1)
        END DO
        b(i) = summ
      END DO

!do i=1,order
!  do j=1,order
!    print*,a(i,j)
!  enddo
!  print*,"-------"
!enddo
!print*,b
!print*,"++"
print*,a,b
print*,"++++++"
!------------------------------------------------------------------
      do k=1,order-1
        do i=k+1,order     !前消
          factor=a(i,k)/a(k,k)
          a(i,k)=0
            do j=k+1,order
              a(i,j)=a(i,j)-factor*a(k,j)
            enddo
          b(i)=b(i)-factor*b(k)
        enddo
      enddo
!print*,a,b
!print*,"-------"
do i=1,order
  do j=1,order
    print*,a(i,j)
  enddo
  print*,"-------"
enddo
print*,b

      do z=1,order
        ans(order) =b(order)/a(order,order)
        do i=order-1,1,-1
          summ =0
          do j=i+1,order
            summ=summ +(a(i,j)*ans(j))
          enddo
          ans(i) =(b(i)-summ) / a(i,i)
        enddo
      enddo


      DO i = 1, n
        sumy = sumy + y(i)
      END DO
      ym = sumy/n

      DO i = 1, n
        st = st + (y(i) - ym)**2
        sr = sr + (y(i)-ans(1)-ans(2)*x(i)-ans(3)*x(i)**2)**2
print*,st,sr,"**"
      END DO

      syx = sqrt((sr/(n - 3)))
      r2 = (st - sr)/st


print*,"after sub"
write(*,"(a3,f10.7)")"a0=",ans(1)
write(*,"(a3,f10.7)")"a1=",ans(2)
write(*,"(a3,f10.7)")"a2=",ans(3)
write(*,"(a15,f10.7,a2,f10.7,a3,f10.7,a3)")"擬合曲線:y=",ans(1),"+(",ans(2),")x+",ans(3),"x^2"

print*,"syx=",syx
print*,"r2=",r2


      stop
      end
