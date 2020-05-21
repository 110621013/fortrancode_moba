      program hw7!用來寫測站形式的dat

      implicit none
      integer :: i,j,k,nlev,nflag
      integer :: yy,mm,dd,hh,sd,ed,sm,em,days,record
      real :: pi,time
      real,dimension(76) :: lat,lon,pm25sum,pm25ave,num
      real,dimension(24*365,76) ::temp,ch4,co,nmhc,no,no2,nox,o3,pm10,&
                                 pm25,rain,rh,so2,thc,wd,ws,u,v
      character,dimension(76) :: stn*10,stn11*8
      character*51 :: path
      character*100 :: infile(76),outfile
!------------------------------------------------------------------------------
      path='/homework/106/data'
!------------------------------------------------------------------------------
      open(11,file='/homework/106/data/EPA_location.txt')!路徑季的改
      do i=1,76
        read(11,*) stn(i),lat(i),lon(i)
        infile(i)=TRIM(path)//'/'//TRIM(stn(i))//'_2015.txt'
!      print*,stn(i),lat(i),lon(i)
      enddo
      close(11)
!------------------------------------------------------------------------------
        num=0.
      do j = 1,76
         open (12,file=trim(infile(j)),form='formatted')
!         print*,infile(j)

              read (12,*)

         PM25sum(j)=0.

         do i = 1,24*365
        read (12,'(i4,i2,i2,i2,18(f8.2))')yy,mm,dd,hh,temp(i,j),ch4(i,j),co(i,j),nmhc(i,j),no(i,j),&
               no2(i,j),nox(i,j),o3(i,j),pm10(i,j),pm25(i,j),rain(i,j),&
               rh(i,j),so2(i,j),thc(i,j),wd(i,j),ws(i,j),u(i,j),v(i,j)
         enddo

         close(12)
      enddo
!-------------------------------------------------------------------------------------
      open(31,file='106601015_hw7.dat',form='unformatted')!測站都在一張資料裡,一檔案
      do i=24*90+1,24*90+25                !!時間,天數
        time=0.0
        nlev=1
        nflag=1
        do j=1,76
          write(stn11(j),'(i8)') j!在強制將測站名稱改成數字
!          print*,'00000000=',stn11(j)
          write(31) stn11(j),lat(j),lon(j),time,nlev,nflag!一定要降寫
          write(31) pm25(i,j),u(i,j),v(i,j),o3(i,j)
!          write(31) temp(i,j),ch4(i,j),co(i,j),nmhc(i,j),no(i,j),&
!               no2(i,j),nox(i,j),o3(i,j),pm10(i,j),pm25(i,j),rain(i,j),&
!               rh(i,j),so2(i,j),thc(i,j),wd(i,j),ws(i,j),u(i,j),v(i,j)
        enddo
        nlev=0!宣告這時間結束了
        write(31) stn11(76),lat(76),lon(76),time,nlev,nflag!return nlev=0,這時間結束
      enddo
      write(31) stn11(76),lat(76),lon(76),time,nlev,nflag!回會有連續兩個nlev=0,資料結束
!      print*,'00000000=',stn11(76)
      close(31)
!-------------------------------------------------------------


      stop
      end

