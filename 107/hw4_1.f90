      program hw4_1
      implicit none

      integer :: imax,iter
      real*8 :: xl, xu, xr, xrold, ea, es, test

      imax=200!最大操作次數
      iter = 0!counter
      ea = 100!驗證錯誤直
      es=2 !可容忍錯誤直((無容忍誤差,做完
      xl=0.5
      xu=1
      xrold=0
      xr=0

print*,"imax=",imax,"es=",es,"%"

      DO
        xrold = xr
        xr = (xl + xu) / 2
        iter = iter + 1
        if(xr /= 0) then
          ea = ABS((xr - xrold) / xr) * 100
        end if
        test = (sin(xl)-xl**3) * (sin(xr)-xr**3)!!!

        if(test < 0) then
          xu = xr
          ELSE IF(test > 0) then
          xl = xr
          ELSE
          ea = 0
        END IF
Write(*,"(A9, I2, 4x, A3, F16.13, 2x, A3, F16.13)") "Iteration", iter, &
                               "xl=",xl,"xu=",xu
        IF (ea < es .OR. iter >= imax) exit
      END DO
print*,"first nontrivial root is",(xl+xu)/2

      stop
      end
