      program personhw2
      implicit none

      integer :: i,j,k,l,day
      integer,dimension(60*24*31,12,14) :: yy,mm,dd,hh,mi
      real*8 :: tempmax=0,rainfalltotal
      real,dimension(60*24*31,12,14) :: temp,rh,p,ws,wd,rad,rainfall
      character*12 :: tempmaxtime
      character*10 :: gar
      character*2 :: path,ich,jch
      character*100,dimension(14,12) :: infile
!------------------------------------------------------------------------------------------------
      do i=3,16
        if(i<10)then
          write(ich,"(A1,I1)") "0",i
        else
          write(ich,"(I2)") i
        endif
        do j=1,12
          if(j<10)then
            write(jch,"(A1,I1)") "0",j
          else
            write(jch,"(I2)") j
          endif

          path='20'
          infile(i,j)=TRIM(path)//ich//jch//'.dat'
        enddo
      end do
!-------------------------------------------------------------------------------------------------

      do i=3,16
        do j=1,12
          open(10,file=infile(i,j),form='formatted')!30!!!!!!!!!!!!!!!!!
          read(10,*)   
          read(10,*)
          read(10,*)            !30

          if(j==4 .or. j==6 .or. j==9 .or. j==11)then
            day=30
          elseif(j==2)then
            day=28
          else
            day=31
          endif
          if(i==4 .or. i==8 .or. i==12 .or. i==16)then
            if(j==2)then
              day=29
            endif
          endif

          do k=1,60*24*day
            read(10,'(i4,i2,i2,i2,a6,f5.3,a5,f6.3,a4,f7.3,a6,f5.3,a5,&
                  f6.3,a5,f6.3,a5,f6.3,)')yy(k,j,i),mm(k,j,i),&
                   dd(k,j,i),hh(k,j,i),mi(k,j,i),gar,temp(k,j,i),&
                   gar,rh(k,j,i),gar,p(k,j,i),gar,ws(k,j,i),gar,&
                   wd(k,j,i),gar,rad(k,j,i),gar,rainfall(k,j,i)
!print*,temp(k,j,i),rh(k,j,i),p(k,j,i),ws(k,j,i),wd(k,j,i),rad(k,j,i),rainfall(k,j,i)
          enddo
          close(10)
        enddo
      enddo

!-------------------------------

      do i=3,16
        do j=1,12
          if(j==4 .or. j==6 .or. j==9 .or. j==11)then
            day=30
          elseif(j==2)then
            day=28
          else
            day=31
          endif
          if(i==4 .or. i==8 .or. i==12 .or. i==16)then
            if(j==2)then
              day=29
            endif
          endif
          do k=1,60*24*day
            if(temp(k,j,i)>45 .or. temp(k,j,i)<-10)then
              temp(k,j,i)=temp(k-1,j,i)
            endif
            if(rh(k,j,i)>100 .or. rh(k,j,i)<10)then
              rh(k,j,i)=rh(k-1,j,i)
            endif
            if(p(k,j,i)>1100 .or. p(k,j,i)<850)then
              p(k,j,i)=p(k-1,j,i)
            endif
            if(ws(k,j,i)<0)then
              ws(k,j,i)=ws(k-1,j,i)
            endif
            if(wd(k,j,i)>360 .or. wd(k,j,i)<0)then
              wd(k,j,i)=wd(k-1,j,i)
            endif
            if(rad(k,j,i)<0)then
              rad(k,j,i)=rad(k-1,j,i)
            endif
            if(rainfall(k,j,i)<0)then
              rainfall(k,j,i)=rainfall(k-1,j,i)
            endif                             !資料除錯


            if(tempmax<temp(k,j,i))then
              tempmax=temp(k,j,i)
              write(tempmaxtime,"(i4,i2,i2,i2,i2)")yy(k,j,i),mm(k,j,i),dd(k,j,i),hh(k,j,i),mi(k,j,i)
            endif

          enddo
        enddo
      enddo


!      i=64
!      l=0.
!      open(13,file=infile(i),form='formatted')
!      do j=1,8760
!      if(mm(j)==01.and.dd(j)>=01)then
!        if(mm(j)==01.and.dd(j)<=31)then

!          if(pm25(j,i)/=-999.0)then
!          l=l+1
!          k=k+pm25(j,i)

!          endif
!        endif
!      endif
!      enddo

!      avg=k/l
!      write(*,*)avg
!      close(13)

      stop
      end

