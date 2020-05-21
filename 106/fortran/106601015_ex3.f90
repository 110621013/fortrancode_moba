      program midexam3
      implicit none

      integer :: i,j,l,z,m,nlev,nflag,st,et,days,record
      real :: pi
      integer,dimension(76,8760) :: time
      real,dimension(76) :: lat,lon
      real,dimension(76,8760) :: temp,ch4,co,nmhc,no1,no2,nox,o3,pm10,pm25,rain,rh,so2,thc,wd,ws,u,v
      character,dimension(76) :: stn*10,nn*6
      character*51 :: path
      character*100,dimension(76) :: infile
      integer,dimension(24*365) :: yy,mm,dd,hh
      real :: k=0.0,n=0.0,avg=0.0,aven=0.0
!-------------------------------------------------------------------------------------------------
      open(10,file='/homework/106/data/EPA_location.txt')
      do i=1,76
        read(10,*) stn(i),lat(i),lon(i)
      end do
!-------------------------------------------------------------------------------------------------
      do i=1,76
        path='/homework/106/data/'
        infile(i)=TRIM(path)//trim(stn(i))//'_2015.txt'
      end do
!---------------------------------------
      i=22
      open(13,file=infile(i),form='formatted')
      read(13,*)
!30
      do j=1,24*365
      read(13,'(i4,i2,i2,i2,18(f8.2))')yy(j),mm(j),dd(j),hh(j),temp(j,i),ch4(j,i),co(j,i),nmhc(j,i),no1(j,i),&
                no2(j,i),nox(j,i),o3(j,i),pm10(j,i),pm25(j,i),rain(j,i),rh(j,i),so2(j,i),thc(j,i),wd(j,i),ws(j,i),u(j,i),v(j,i)
      enddo
      

      if(mm(j)>=05.and.dd(j)>=15)then
        if(mm(j)<=06.and.dd(j)<=15)then
          do j=1,24*365
           if(temp(j,i)/=-999.0)then
           z=z+1
           l=l+temp(j,i)
           endif
          enddo
          avg=l/z
          write(*,*)avg

          do j=1,24*365
           if(ws(j,i)/=-999.0)then
           m=m+1
           n=n+ws(j,i)
           endif
          enddo
          aven=m/n
          write(*,*)aven

          endif
      endif
      close(13)

      stop
      end

