      program test
      implicit none

      real*8 :: p
      character*8 :: a

a="    12.4"
read(a,"(f8.2)")p
print*,p

      stop
      end
