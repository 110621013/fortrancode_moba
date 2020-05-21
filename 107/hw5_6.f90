      program hw5_6
      implicit none

      integer :: n=3,i,j,k
      real,dimension(n,n) :: a
      real,dimension(n) :: x,b,d
      real*8 :: factor,summ

      a(1,1)=6
      a(1,2)=15
      a(1,3)=55
      a(2,1)=15
      a(2,2)=55
      a(2,3)=225
      a(3,1)=55
      a(3,2)=225
      a(3,3)=979

      b(1)=152.6
      b(2)=585.6
      b(3)=2488.8

print*,"a:"
do i=1,n
print*,a(i,1),a(i,2),a(i,3)
enddo

!LU分解gogo
!      a(1,1)=sqrt(a(1,1))
!      DO k=1,n-1
!        DO i=k+1,n
!          factor=a(i,k)/a(k,k)
!print*,factor
!          a(i,k)=factor
!          DO j=k+1,n
!            a(i,j)=a(i,j)-factor*a(k,j)
!          END DO
!        END DO
!      END DO

!      a(1,1)=sqrt(a(1,1))
!      do i=2,n
!        a(i,1)=a(i,1)/a(1,1)
!      enddo
!      a(2,2)=sqrt(a(2,2)-a(2,1)**2)
!      a(3,2)=(a(3,2)-a(2,1)*a(3,1))/a(2,2)
!      a(3,3)=sqrt(a(3,3)-a(3,1)**2-a(3,2)**2)

      DO k=1,n
        DO i=1,k-1
          summ = 0
          DO j=1,i-1
            summ = summ + a(i,j) * a(k,j)
          END DO
          a(k,i) = (a(k,i) - summ)/a(i,i)
        END DO

        summ = 0
        DO j=1,k-1
          summ = summ + a(k,j)**2
        END DO
        a(k,k) = sqrt(a(k,k) - summ)

        do j=k+1,n
          a(k,j)=0
        enddo
      END DO  !做Cholesky’s LU decomposition


print*,"after Cholesky’s LU decomposition, L:"
do i=1,n
print*,a(i,1),a(i,2),a(i,3)
enddo

      do i=1,n
        summ=0
        do j=1,i-1
!print*,"dodo",i
          summ=summ+(a(i,j)*d(j))
        enddo
        d(i) =(b(i)-summ)/a(i,i)
      enddo
print*,"d如下"
print*,d,"d1d2d3,LD=B"


      do j=1,n
        do i=j,n
          a(j,i)=a(i,j)
          if(i/=j)then
            a(i,j)=0
          endif
        enddo
      enddo     !LU轉移互換

print*,"after L^T=U transform , U:"
do i=1,n
print*,a(i,1),a(i,2),a(i,3)
enddo

      do i=n,1,-1
        summ=0
        do j=1,n-i
!print*,"dodo",i
          summ=summ+(a(i,n+1-j)*x(n+1-j))
        enddo
        x(i) =(d(i)-summ)/a(i,i)
      enddo
!      enddo   !球D
print*,"x如下"
print*,x,"x1x2x3,UX=D"


      stop
      end
