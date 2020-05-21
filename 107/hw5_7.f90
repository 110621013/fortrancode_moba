      program hw5_7
      implicit none

      integer :: i,j,n=3,imax=100,iter,sentinel
      real,dimension(n,n) :: a
      real,dimension(n) :: b,x
      real*8 :: dummy,es=5,ea,summ,old,lambda=1.2

      a(1,1)=0.8
      a(1,2)=-0.4
      a(1,3)=0
      a(2,1)=-0.4
      a(2,2)=0.8
      a(2,3)=-0.4
      a(3,1)=0
      a(3,2)=-0.4
      a(3,3)=0.8
      b(1)=41
      b(2)=25
      b(3)=105

print*,"a,b:"
do i=1,n
print*,a(i,1),a(i,2),a(i,3),b(i)
enddo
print*,"imax=",imax,"es=",es,"%"

      DO i=1,n
        dummy = a(i,i)
        DO j=1,n
          a(i,j) = a(i,j)/dummy
        END DO
        b(i) = b(i)/dummy
      END DO


      DO i=1,n
        summ = b(i)
        DO j=1,n
          if(i/=j)then
            summ=summ-a(i,j)*x(j)
!print*,summ,i,j
          endif
        END DO
        x(i)=summ
      END DO
      iter=1

      DO
        sentinel = 1
        DO i=1,n
          old=x(i)
          summ=b(i)
          DO j=1,n
            if(i/=j)then
              summ=summ-a(i,j)*x(j)
!print*,summ,i,j
            endif
          END DO
          x(i)= lambda*summ+(1-lambda)*old
write(*,"(a4,i2,a7,f12.7,a7,f12.7,a7,f12.7)")"iter",iter,"x1=",x(1),"x2=",x(2),"x3=",x(3)
          if(sentinel==1 .and. x(i)/=0)THEN
            ea=ABS((x(i)-old)/x(i))*100
            IF(ea>es)THEN
              sentinel=0
            endif
          endif
        END DO
        iter=iter+1
        IF(sentinel==1 .OR. iter >= imax)EXIT
      END DO


      stop
      end
