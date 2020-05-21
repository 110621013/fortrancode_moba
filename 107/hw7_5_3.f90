      program hw7_5_3
      implicit none

      integer :: n=11      ! base points for interpolation
      integer :: nint=21   ! compute interpolation in nint points
      real*8 :: xmin, xmax     ! given interval of x()
      real*8,dimension (n) :: xi(n), yi(n), b(n), c(n), d(n)
      real*8 :: x, y, step, ys, error, errav
      integer :: i
      real*8 :: f, ispline


      xmin = 0
      xmax = 40

! step 1: generate xi and yi from f(x), xmin, xmax, n
      step = (xmax-xmin)/(n-1)
      do i=1,n
        xi(i) = xmin + step*float(i-1)
        yi(i) = f(xi(i))
     !  write (*,200) xi(i), yi(i)
      end do


















      stop
      end
