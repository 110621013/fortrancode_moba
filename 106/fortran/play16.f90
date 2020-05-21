      program play16
      implicit none
      integer :: i,j=0
      real :: k=0.0,l=0,avg=0.0,pk=0,king
      integer :: nlev,nflag,st,et,days,record
      integer,dimension(24*365) :: yy,mm,dd,hh
      real :: pi
      integer,dimension(76,8760) :: time
      real,dimension(76) :: lat,lon
      real,dimension(76,8760) :: temp,ch4,co,nmhc,no1,no2,nox,o3,pm10,pm25,rain,rh,so2,thc,wd,ws,u,v
      character,dimension(76) :: stn*10,nn*6
      character*51 :: path
      character*100,dimension(76) :: infile
!------------------讀中壢測站all資料,讀1月pm25資料算平均--------------------------------------------------------------------------------
      open(10,file='/homework/106/data/EPA_location.txt')
      do i=1,76
        read(10,*) stn(i),lat(i),lon(i)
      end do
!-------------------------------------------------------------------------------------------------
      do i=1,76
        path='/homework/106/data/'
        infile(i)=TRIM(path)//trim(stn(i))//'_2015.txt'
      end do
!-------------------------------------------------------------------------------------------------
!      open(13,file='/homework/106/data/Zhongli_2015.txt')
!---------------------------------------

      do i=1,76
        open(11,file=infile(i),form='formatted')!30!!!!!!!!!!!!!!!!!
        read(11,*)               !30
           do j=1,8760
            read(11,'(i4,i2,i2,i2,18(f8.2))')yy(j),mm(j),dd(j),hh(j),temp(j,i),ch4(j,i),co(j,i),nmhc(j,i),no1(j,i),&
                no2(j,i),nox(j,i),o3(j,i),pm10(j,i),pm25(j,i),rain(j,i),rh(j,i),so2(j,i),thc(j,i),wd(j,i),ws(j,i),u(j,i),v(j,i)
     
           enddo
      close(11)
      enddo

!-------------------------------
      i=64
      l=0.
      open(13,file=infile(i),form='formatted')
      do j=1,8760
      if(mm(j)==01.and.dd(j)>=01)then
        if(mm(j)==01.and.dd(j)<=31)then 
        
          if(pm25(j,i)/=-999.0)then
          l=l+1
          k=k+pm25(j,i)
          
          endif
        endif
      endif
      enddo

      avg=k/l
      write(*,*)avg
      close(13)

      stop
      end


