      program hw4_4_fixed
      implicit none

      integer :: imax,iter
      real*8 :: xr, xrold, ea, es, inx

      imax=40!最大操作次數
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
        xr = sqrt(1.8*xrold+2.5) !變換一下  x^2 x^1項皆可成為x
        iter = iter + 1
        IF(xr /= 0)THEN
          ea =abs((xr - xrold)/xr)*100
        ENDIF
!print*,iter,xr,ea
Write(*,"(A9, I3, 4x, A3, F16.13, 2x, A3, F16.12)") "Iteration", iter, "xr=", xr, "ea=", ea

        IF(ea < es .OR. iter >= imax)exit
      ENDDO


print*,"root of f(x) is:",xr

      stop
      end

