      program gauss
      implicit none

      subroutine gauss(a, b, n, x, tol, er)  !a=原左矩陣nxn,b=右邊答案nx1,n=n維矩陣,x=解nx1,tol=容許之pivot玉質,er除錯植
        integer :: i,j,n,er=0
        real,dimension(n) :: s
        real,dimension(n,n) :: a

        DO i=1,n       !設定s初始值與er初始值  s為矩陣，用來存每一個row最大值,每個row最大值存s
          s(i) =ABS(a(i,1))
          DO j =2,n
            if(ABS(a(i,j))>s(i))then s(i)=ABS(a(i,j))
          END DO
        END DO

        call eliminate(a, s, n, b, tol, er)

        if(er /= -1)THEN
          call Substitute(a, n, b, x)
        endif
      return
      end gauss

!-------------------
      subroutine eliminate(a, s, n, b, tol, er)
        integer :: k
        real :: factor

        do k=1,n-1
          call pivot (a, b, s, n, k)
          if(ABS(a(k,k)/s(k)) < tol THEN
            er=-1            !確定每一列的pivot coefficient不為零   若對角線元素太小則出問題,er=-1不做事
            EXIT DO
          endif
          do i=k+1,n
            factor=a(i,k)/a(k,k)
              do j=k+1,n
                a(i,j) =a(i,j)-factor*a(k,j)
              enddo
            b(i)=b(i)-factor * b(k)
          enddo
          if ABS(a(n,n)/s(n))<tol THEN er=-1
        enddo
      return
      end eliminate
!-----------------------
      subroutine substitute(a, n, b, x)
        integer :: i,j
        real*8 :: sum

        x(n) =b(n)/a(n,n)
        do i=n-1,1,-1
          sum =0
          do j=i+1,n
            sum=sum +(a(i,j) * x(j))
          enddo
          x(i) =(b(i)-sum) / a(i,i)
        enddo
      return
      end Substitute
!-----------------------
      subroutine pivot(a, b, s, n, k)
        integer :: p,ii,jj
        real*8 :: big,dummy,Dummy

        p=k
        big =ABS(a(k,k)/s(k))
        do ii=k+1,n
          dummy=ABS(a(ii,k)/s(ii))
          if dummy>big THEN
            big=dummy
            p=ii
          endif
        enddo
        if p/=k THEN
          do jj=k,n
            Dummy=a(p,jj)
            a(p,jj)=a(k,jj)
            a(k,jj)=dummy
          enddo
          Dummy=b(p)
          b(p)=b(k)
          b(k)=dummy
          Dummy=s(p)
          s(p)=s(k)
          s(k)=dummy
        endif
      return
      end pivot
!--------------------------





!------------------------------------------------
print*,"done!"
      stop
      end
