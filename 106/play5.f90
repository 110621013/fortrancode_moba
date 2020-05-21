      program test
      implicit none
      integer ::a
      real    ::b
      complex ::c
      character(len=20) ::d
      
      a=10
      b=123.45
      c=(1.0,2.0)
      d='hello,world!'
      write(*,100)a
      write(*,200)b,b
      write(*,300)c
      write(*,400)d

      100 format(1x,i5)
      200 format(1x,f9.3,2x,E15.7)
      300 format(1x,f5.2,f5.2)
      400 format(1x,a20)

      stop
      end
