      program hw2_2
      implicit none

      integer :: ea,iter,es,maxit,solold
      real*8 :: a,sol

      maxit=2000!最大操作次數
      iter = 1!counter
      sol = 100000000!解初始直
      ea = 100!驗證錯誤直
      es=0!可容忍錯誤直((無容忍誤差,做完
      a=598481.0!給定任意a

      print*,"a=",a

      DO
        solold = sol
!print*,real(a)/sol
        sol = (sol+real(a)/sol)/2
        iter = iter + 1
        if (sol /= 0) then
          ea=abs(real(sol - solold)/sol)*100 !real防止變成0讀不到
        endif
print*,sol,ea,iter
        IF (ea <= es .or. iter >= maxit) EXIT
      END DO
      
      print*,"x=sqrt(a)=",sol!((a的平方根,即x

      stop
      end

