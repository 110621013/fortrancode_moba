      program hw4_3
      implicit none

      integer :: imax,iter,iu,il
      real*8 :: xl, xu, xr, xrold, ea, et, es, test, fl, fu, fr, ttt


      imax=200!最大操作次數
      iter = 0!counter
      ea = 100!驗證錯誤直((approximate percent relative error
      et = 100!驗證錯誤直((true percent relative error
      es=0.01 !可容忍錯誤直
      xl=0
      xu=1.3
      xr=0
      ttt=1!真值

print*,"imax=",imax,"es=",es,"%"
Write(*,"(A5, I3, 4x, A3, F16.13, 2x, A3, F16.13)") "Iter=", iter, &
                               "xl=",xl,"xu=",xu
      fl = xl**10-1
      fu = xu**10-1

      DO
        xrold=xr
        xr = xu - fu*(xl-xu)/(fl-fu)
        fr = xr**10-1
        iter = iter + 1
        IF(xr<0 .or. xr>0) THEN
          ea = abs((xr-xrold)/xr)*100
          et = abs((xr-ttt)/ttt)*100
        END IF
        test = fl * fr
        IF(test < 0)THEN
          xu = xr
          fu = xu**10-1
          iu = 0
          il = il + 1
          if(il >= 2)THEN 
            fl = fl/2
          endif
        ELSE IF(test > 0)THEN
          xl = xr
          fl = xl**10-1
          il = 0
          iu = iu + 1
          IF(iu >= 2)THEN
            fu = fu/2
          endif
        ELSE
          et = 0
        END IF

        Write(*,"(A5, I3, 4x, A3, F16.13, 2x, A3, F16.13, 2x, A3, F17.12, 2x, A3, F17.12)") "Iter=", iter, &
                               "xl=",xl,"xu=",xu, "et=", et, "ea=", ea
        if(et < es .OR. iter >= imax)then 
          exit
        endif
      ENDDO

      write(*,"(A28,2x,F16.13)") "the root of the function is:",(xu+xl)/2

      stop
      end
