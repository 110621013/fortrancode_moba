      program test_play1

      implicit none

      integer(kind=4) ::a
      real            ::b
      character       ::c
      logical         ::d

      a=1
      b=1.0
      c='c'
      d=.true.

      write(*,*)a,b,c,d

      stop
      end

