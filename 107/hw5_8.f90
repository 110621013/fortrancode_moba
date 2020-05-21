      program hw5_8
      implicit none

      integer :: i,j,k,p,n=3,imax=100,iter,sentinel
      real,dimension(n,n) :: a
      real,dimension(n) :: b,x,s
      real*8 :: dummy,es=5,ea,summ,old,lambda=0,big,dum

      a(1,1)=2
      a(1,2)=-6
      a(1,3)=-1
      a(2,1)=-3
      a(2,2)=-1
      a(2,3)=7
      a(3,1)=-8
      a(3,2)=1
      a(3,3)=-2
      b(1)=-38
      b(2)=-34
      b(3)=-20

print*,"a,b:"
do i=1,n
print*,a(i,1),a(i,2),a(i,3),b(i)
enddo
print*,"imax=",imax,"es=",es,"%"


      do i=1,n       !設定s初始值與er初始值  s為矩陣，用來存每一個row最大值,每個row最大值存s
        s(i) =abs(a(i,1))
        do j =2,n
          if(abs(a(i,j))>s(i))then
            s(i)=abs(a(i,j))
          endif
        enddo
      enddo
print*,"s:",s

      do k=1,n-1!gogogo pivot
        p=k       !紀錄k
!print*,p,"p"
        big =abs(a(k,k)/s(k)) !big是k列對角線值/k列最大值
!print*,big,"big"
        do i=k+1,n     !ii作k下面列的大小確認
          dum=abs(a(i,k)/s(i))  !dum=big功能
!print*,dum,"dum"
          if(dum>big)THEN  !若dum>big則需要對調
            big=dum        !
            p=i           !紀錄第p出示
          endif
        enddo
        if(p/=k)THEN       !需要對調
          do j=k,n
            Dummy=a(p,j)
            a(p,j)=a(k,j)
            a(k,j)=Dummy
!print*,Dummy,a(p,jj),a(k,jj)
          enddo
          Dummy=b(p)
          b(p)=b(k)
          b(k)=Dummy
          Dummy=s(p)
          s(p)=s(k)
          s(k)=Dummy
        endif
        enddo!gg pivot

print*,"after pivot",k
do i=1,n
print*,a(i,1),a(i,2),a(i,3),b(i)
enddo


      DO i=1,n
        dummy = a(i,i)
        DO j=1,n
          a(i,j) = a(i,j)/dummy
        END DO
        b(i) = b(i)/dummy
      END DO


      DO i=1,n
        summ = b(i)
        DO j=1,n
          if(i/=j)then
            summ=summ-a(i,j)*x(j)
!print*,summ,i,j,x(j),"==="
          endif
        END DO
        x(i)=summ
      END DO
      iter=1
!print*,"uuuuuuuuuu",x

print*,"In A situation:"
      DO
        sentinel = 1
        DO i=1,n
          old=x(i)
          summ=b(i)
          DO j=1,n
            if(i/=j)then
              summ=summ-a(i,j)*x(j)
!print*,summ,i,j
            endif
          END DO
          x(i)= lambda*summ+(1-lambda)*old
write(*,"(a4,i2,a7,f12.7,a7,f12.7,a7,f12.7)")&
"iter",iter,"x1=",x(1),"x2=",x(2),"x3=",x(3)
!print*,"--------------------",x(i),old
          if(sentinel==1 .and. x(i)/=0)THEN
            ea=ABS((x(i)-old)/x(i))*100
            IF(ea>es)THEN
              sentinel=0
            endif
          endif
        END DO
        iter=iter+1
        IF(sentinel==1 .OR. iter >= imax)EXIT
      END DO

      iter=1
      lambda=1.2
print*,"In B situation:"
      DO
        sentinel = 1
        DO i=1,n
          old=x(i)
          summ=b(i)
          DO j=1,n
            if(i/=j)then
              summ=summ-a(i,j)*x(j)
!print*,summ,i,j
            endif
          END DO
          x(i)= lambda*summ+(1-lambda)*old
write(*,"(a4,i2,a7,f12.7,a7,f12.7,a7,f12.7)")&
"iter",iter,"x1=",x(1),"x2=",x(2),"x3=",x(3)
!print*,"--------------------",x(i),old
          if(sentinel==1 .and. x(i)/=0)THEN
            ea=ABS((x(i)-old)/x(i))*100
            IF(ea>es)THEN
              sentinel=0
            endif
          endif
        END DO
        iter=iter+1
        IF(sentinel==1 .OR. iter >= imax)EXIT
      END DO


      stop
      end

