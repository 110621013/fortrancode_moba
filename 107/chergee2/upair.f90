      program upair
      implicit none

      integer :: i=0,j=0,k=0,l=0,m=0,height=0,con=0
      character*2,dimension(12) :: mouth !用以製作infile
      real,dimension(7835,2,31,12) :: zo !位溫,上下層位溫差
      real :: delzo,dzodz,maxx
      real,dimension(12,7835) :: p,tx,td,ws!資料
      integer,dimension(12,7835) :: dd,hh,h,wd,rh!資料 
      integer :: stno,yyyymm,si!無用資料
      integer,dimension(12) :: num !12個月分總資料量
      character*100 :: gg,path !檔案路徑
      character*100,dimension(12) :: infile !12個檔案名稱
!-----------------建立infile路徑--------------------------------------------------------------------------------
      do i=1,12
        if(i<10)then
          write(mouth(i),'(a1,i1)')'0',i
!          print*,mouth(i)
        else
          write(mouth(i),'(i2)')i
!          print*,mouth(i)
          endif
      enddo

      do i=1,12
        path='/home/106601015/107/chergee2/'
        infile(i)=TRIM(path)//'2018'//TRIM(mouth(i))//'_upair.txt'
!        print*,infile(i)
      end do
!------------------建立各檔案應讀次數---------------------------------------------------
      num(1)=5916-13
      num(2)=4984-13
      num(3)=6650-13
      num(4)=6320-13
      num(5)=7848-13
      num(6)=7139-13
      num(7)=7549-13 
      num(8)=6922-13
      num(9)=7312-13
      num(10)=6704-13
      num(11)=6330-13
      num(12)=7280-13
!------------------讀資料+計算+寫進data.txt-------------------------------------------------------------------------------
      open(12,file='/home/106601015/107/chergee2/data.txt')
      open(13,file='/home/106601015/107/chergee2/case.txt')
      do i=1,12
        open(11,file=infile(i))!,form='formatted') !開檔案

        do j=1,13
          read(11,*)gg !讀掉標頭垃圾
        enddo

        do j=1,num(i)
          read(11,'(i6,i7,i2,i2,i3,f7.2,i6,f6.2,f6.2,i4,f6.2,i4)')stno,yyyymm,dd(j,i),hh(j,i),si,p(j,i),h(j,i),tx(j,i),td(j,i),wd(j,i),ws(j,i),rh(j,i)

          m=k
          if(hh(j,i)==0)then
            k=1
          else if(hh(j,i)==12)then
            k=2
          endif

          l=dd(j,i)

          zo(j,k,l,i)=tx(j,i)*(1000/p(j,i))**0.286
          if(j>=2)then
            delzo=zo(j,k,l,i)-zo(j-1,k,l,i)
            dzodz=delzo/(h(j,i)-h(j-1,i))
            if(dzodz>0.02 .and. maxx<dzodz .and. h(j,i)<1000 .and. h(j,i)>200)then !判斷逆溫層case
              maxx=dzodz
              height= h(j,i)
              con=con+1
            endif
          endif

          write(12,'(i2,1x,i2,1x,i2,1x,i4,1x,i6,1x,f9.4,1x,f9.5,1x,f9.7,1x)')i,l,k,j,h(j,i),zo(j,k,l,i),delzo,dzodz
print*,l,k,j,zo(j,k,l,i),dzodz,h(j,i)

          if(k-m/=0 .and. maxx>0.01)then
            if(maxx>1 .or. zo(j,k,l,i)==999.9)then  !除資料錯
              continue
            endif
            write(13,'(i2,1x,i2,1x,i2,1x,i4,1x,f9.7,1x,i4)')i,l,k,j,maxx,height
print*,'>>>>>>>>>>>>>>',i,l,k,j,maxx,height
            maxx=0
          endif
        enddo
!        print*,stno,yyyymm,dd(1,i),hh(1,i),si,p(1,i),h(1,i),tx(1,i),td(1,i),wd(1,i),ws(1,i),rh(1,i)
        close(11)
      enddo

      close(12)
      close(13)
print*,"case剩下:",con,"個"

      stop
      end

