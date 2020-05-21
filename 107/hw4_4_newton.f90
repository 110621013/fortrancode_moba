      program hw4_4_newton
      implicit none

      integer :: imax,iter
      real*8 :: xr, xrold, ea, es, inx

      imax=50!最大操作次數
      iter = 0!counter
      ea = 100!驗證錯誤直
      es=0.05 !可容忍錯誤直((無容忍誤差,做完
      xrold=0
      xr=0
      inx=5

write(*,"(A3,F9.6,4x,A5,I3,4x,A3,F16.12,A2)") "X0=",inx,"imax=",imax,"es=",es,"%"
      xr = inx
      iter = 0
      DO
        xrold = xr
        xr = xrold - (xrold*xrold*(-1) + 1.8*xrold + 2.5)/(-2*xrold + 1.8)
        iter = iter + 1
        IF(xr /= 0)THEN
          ea =abs((xr - xrold)/xr)*100
        ENDIF
Write(*,"(A9, I3, 4x, A3, F16.13, 2x, A3, F16.12)") "Iteration", iter, "xr=", xr, "ea=", ea

        IF(ea < es .OR. iter >= imax)exit
      ENDDO

print*,"root of f(x) is:",xr

      stop
      end

