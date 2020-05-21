      program hw4_2
      implicit none

      integer :: imax,iter,osf
      real*8 :: xu,xl,xr,absolute_error,test,a,b,c,d,e,f,g,h

      imax=50!最大操作次數
      iter = 0!counter
      xl=273.15
      xu=313.15
      xr=0
      osf=8
      absolute_error=abs(xu-xl)/2

      print*,"please insert your osf:"
      read(*,*)osf

      print*,"imax=",imax,"osf=",osf

Write(*,"(A9, 1x, I3, A18, F16.13)") "Iteration", iter, &
                               "absolute error=",absolute_error
      do while (absolute_error > 0.05 )
        iter = iter + 1 
        !!!!!!!!!![Compute xu, xl, xr]
        xr = (xl + xu) / 2

        a=157570.1/xr
        b=-66423080/xr**2
        c=12438000000/xr**3
        d=-862194900000/xr**4
        e=157570.1/xl
        f=-66423080/xl**2
        g=12438000000/xl**3
        h=-862194900000/xl**4

        test=(exp(-139.34411+a+b+c+d)-osf)*(exp(-139.34411+e+f+g+h)-osf)
        if(test < 0) then
          xu = xr
          else if(test > 0) then
          xl = xr
        end if

        absolute_error=abs(xu-xl)/2

        Write(*,"(A9, 1x, I3, A18, F16.13)") "Iteration", iter, &
                               "absolute error=",absolute_error
        
        if (iter>=imax) exit
      enddo

print*,((xu+xl)/2)-273.15,"is the temperature."
print*,iter,"times would be required."

      stop
      end
