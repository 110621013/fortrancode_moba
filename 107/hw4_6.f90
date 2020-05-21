      program hw4_6
      implicit none

      integer :: imax,iter
      real*8 :: xr, xrold, ea, inx, pi

      pi=3.14159265359
      imax=3!最大操作次數
      iter = 0!counter
      ea = 100!驗證錯誤直
      xrold=0
      inx=2

write(*,"(A3,F9.6,4x,A5,I3)") &
                          "X0=",inx,"imax=",imax
      xr = inx

      DO
        xrold = xr
        xr = xrold - ((pi/3)*xrold*xrold*xrold - (3*pi)*xrold*xrold + 30)/(pi*xrold*xrold - 6*pi*xrold)
        iter = iter + 1
        IF(xr /= 0)THEN
          ea =abs((xr - xrold)/xr)*100
        ENDIF
Write(*,"(A9, I3, 4x, A3, F16.13, 2x, A3, F16.12, A1)") &
                              "Iteration", iter, "xr=", xr, "ea=", ea, "%"

        IF(iter >= imax)exit
      ENDDO

print*,"root of f(x) is:",xr


      stop
      end

