      program test_test2
      
      integer ::a=1
      real    ::b=2.0
      complex ::c=(1.0,2.0)
      character(len=20) ::str="fortran 90"

      write(*,*)a,b,c,str

      stop
      end

