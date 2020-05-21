      program hw3_1
      implicit none

      integer :: ea,iter,maxit,input,sta,solold
      real*8 :: sol,x,per,es,num,jay

      !maxit=100!最大操作次數
      iter = 1!counter
      sol = 0!解初始直
      ea = 100!驗證錯誤直
      es=1!可容忍錯誤直((%
      per=1!每次開展的項值
      x=2.9 !角度
      jay=1!皆呈
      num=0.00000000000000000001  !當展開式後面項<num=10**-20則停止迴圈
      sta=0 !狀態代表數,0為正常,1為相對誤差<es,2為超過限定迴圈數量,3為馬克勞林末項過小


      print*,"請輸入角度(0~pi)"
      read(*,*) x
      print*,"請輸入可容忍相對誤差值es(%)(默認1%)"
      read(*,*) es
      print*,"請輸入最大迴圈值maxit(默認100次)"
      read(*,*) maxit
!      if(maxit>=12)then
!        maxit=12
!      endif
print*,"************************************************"
      print*,"角度x=",x
      print*,"es=",es
      print*,"maxit=",maxit
print*,"************************************************"
      DO
        solold = sol
          jay=jay*iter           !做皆呈
!print*,"6666666666",jay
        if(mod(iter,4)==2)then   !做出每項瑪克勞您
          per=-(x**iter)/jay
          sta=3
          if(abs(per)<num) exit
        elseif(mod(iter,4)==0)then
          per=(x**iter)/jay
          sta=3
          if(abs(per)<num) exit
        elseif(iter==1)then
          per=1                  !給初始直
        else
          per=0.0
        endif
!print*,per

        sol = sol + per
        iter = iter + 1
        if (sol /= 0) then
          ea=abs(real(sol - solold)/sol)*100 !real防止變成0讀不到
        endif
print*,"展開式中X次數:",iter-1,"解:",sol
        if (ea <= es)then
          sta=1
          exit
        endif
        if (iter>=maxit)then
          sta=2
          exit
        endif
      END DO

print*,"************************************************"

      if(sta==1)then
        print*,"相對誤差ea已小於您的設定es=",es
      else if(sta==2)then
        print*,"迴圈次數已大於您的設定=",maxit
      else if(sta==3)then
        print*,"馬克勞林末項過小(<10^-20),已跳出迴圈"
      endif

      print*,"cos(x)真值=",cos(x)
      print*,"馬克勞林展開cos(x)=",sol
print*,"************************************************"
      stop
      end

