      program hw5_1
      implicit none

      real,dimension(3,3) :: a
      real,dimension(3) :: b,x,s
      integer :: n=3,er=0,i,j,k,p,ii,jj,z
      real*8 :: tol=0,factor,big,dum,Dummy,summ
      !a=原左矩陣nxn,b=右邊答案nx1,n=n維矩陣,x=解nx1,tol=容許之pivot玉質,er除錯植
      !factor再eliminate用
      !p,ii,jj,big,dummy,Dummy再pivot用
      
      a(1,1)=1
      a(1,2)=2
      a(1,3)=-1
      a(2,1)=5
      a(2,2)=2
      a(2,3)=2
      a(3,1)=-3
      a(3,2)=5
      a(3,3)=-1
      b(1)=2
      b(2)=9
      b(3)=1

print*,"原本A長這樣:"
do i=1,n
print*,a(i,1),a(i,2),a(i,3)
enddo

      do i=1,n       !設定s初始值與er初始值  s為矩陣，用來存每一個row最大值,每個row最大值存s
        s(i) =abs(a(i,1))
        do j =2,n
          if(abs(a(i,j))>s(i))then
            s(i)=abs(a(i,j))
          endif
        enddo
      enddo
print*,"s:",s

!gogogo eliminateg
      do k=1,n-1
  !gogogo pivot
!print*,"inpppppppppp"
        p=k       !紀錄k
!print*,p,"p"
        big =abs(a(k,k)/s(k)) !big是k列對角線值/k列最大值
!print*,big,"big"
        do ii=k+1,n     !ii作k下面列的大小確認
          dum=abs(a(ii,k)/s(ii))  !dum=big功能
!print*,dum,"dum"
          if(dum>big)THEN  !若dum>big則需要對調
            big=dum        !
            p=ii           !紀錄第p出示
          endif
        enddo
        if(p/=k)THEN       !需要對調
          do jj=k,n
            Dummy=a(p,jj)
            a(p,jj)=a(k,jj)
            a(k,jj)=Dummy
!print*,Dummy,a(p,jj),a(k,jj)
          enddo
          Dummy=b(p)
          b(p)=b(k)
          b(k)=Dummy
          Dummy=s(p)
          s(p)=s(k)
          s(k)=Dummy
        endif
print*,"after pivot",k
do i=1,n
print*,a(i,1),a(i,2),a(i,3),b(i)
enddo
!print*,"outppppppppppppp"
  !gg pivot
        if(ABS(a(k,k)/s(k))<tol)then
          er=-1            !確定每一列的pivot coefficient不為零   若對角線元素太小則出問題,er=-1不做事
print*,"!!!!!!!!!!!!!!!!!!!!!!!!!"
          exit
        endif
        do i=k+1,n     !前消
          factor=a(i,k)/a(k,k)
!print*,factor
          a(i,k)=0
            do j=k+1,n
              a(i,j)=a(i,j)-factor*a(k,j)
            enddo
          b(i)=b(i)-factor*b(k)
        enddo

        if(abs(a(n,n)/s(n))<tol)then
          er=-1
print*,"?????????????"
        endif

!gg eliminatei
print*,"after eli"
do i=1,n
print*,a(i,1),a(i,2),a(i,3),b(i)
enddo
      enddo

      do z=1,n
        if(er /= -1)THEN
!gogogo Substitute
          x(n) =b(n)/a(n,n)
!print*,x,"xxxxxxxxxxxxxx"
          do i=n-1,1,-1
            summ =0
            do j=i+1,n
              summ=summ +(a(i,j) * x(j))
!print*,summ,"summmmmmmmmmmmmmmmmmmm"
            enddo
            x(i) =(b(i)-summ) / a(i,i)
          enddo
!gg Substitute
        endif
      enddo
      
print*,"開始後代"
print*,"after sub"
print*,"x:",x

      stop
      end
