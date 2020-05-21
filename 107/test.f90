      program test
      implicit none

            real,dimension(3,3) :: a
      real,dimension(3) :: b,x,s
      integer :: n=3,er=0
      real :: tol=0.1,i,j

      a(1,1)=1
      a(1,2)=2
      a(1,3)=-1
      a(2,1)=5
      a(2,2)=2
      a(2,3)=2
      a(3,1)=-3
      a(3,2)=5
      a(3,3)=-1
      b(1)=2
      b(2)=9
      b(3)=1


      subroutine gauss(a, b, n, x, tol, er)  !a=原左矩陣nxn,b=右邊答案nx1,n=n維矩陣,x=解nx1,tol=容許之pivot玉質,er除錯植
        integer :: i,j,n,er=0
        real,dimension(n) :: s
        real,dimension(n,n) :: a

        DO i=1,n       !設定s初始值與er初始值  s為矩陣，用來存每一個row最大值,每個row最大值存s
          s(i) =abs(a(i,1))
          DO j =2,n
            if(abs(a(i,j))>s(i))then
              s(i)=abs(a(i,j))
            endif
          END DO
        END DO
print*,s
!        call eliminate(a, s, n, b, tol, er)

!        if(er /= -1)THEN
!          call Substitute(a, n, b, x)
!        endif
      return
      end gauss




      stop
      end
