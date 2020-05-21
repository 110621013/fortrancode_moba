      program stationformgif!用來寫測站形式的dat

      implicit none
      integer :: i,j,jp,k,nlev,nflag,record
      !integer :: yy,mm,dd,hh,sd,ed,sm,em,days,record
      real :: time
      real,dimension(20) :: lat,lon,bug
      real,dimension(24,20) :: num1,stp1,sfp1,maxstp1,minstp1,temp1,maxtemp1,mintemp1,td1,ws1,wd1,rh1
      real,dimension(30) :: num,stp,sfp,maxstp,minstp,temp,maxtemp,mintemp,td,ws,wd,rh
      character,dimension(20) :: stn*10,stn11*8
      !character*51 :: path
      character*100 :: input,js,trash,infile,outfile
      character*10 :: jpp
!------------------------------------------------------------------------------
!      open(11,file='./EPA_location.txt')!路徑季的改
!      do i=1,76
!        read(11,*) stn(i),bug(i),lat(i),lon(i)
!      enddo
!      write(*,*)stn(i),bug(i),lat(i),lon(i)
!      close(11)
!------------------------------------------------------------------------------
         open(10,file='banqiao2014_11.txt')
         open(15,file='banqiao201411.dat',form='unformatted',access='direct',recl=4*1*1)
         record=1
         do i = 1,30
           read (10,'(f8.0,f8.0,f8.0,f8.0,f8.0,f8.0,f8.0,f8.0)')num(i),stp(i),sfp(i),temp(i),td(i),rh(i),ws(i),wd(i)
           write(15,rec=record)stp(i)
           record=record+1
           write(15,rec=record)sfp(i)
           record=record+1
           write(15,rec=record)temp(i)
           record=record+1
           write(15,rec=record)td(i)
           record=record+1
           write(15,rec=record)rh(i)
           record=record+1
           write(15,rec=record)ws(i)
           record=record+1
           write(15,rec=record)wd(i)
           record=record+1
         enddo
         close(10)
         close(15)      
 
!      open(15,file='banqiao201411.dat',form='unformatted',access='direct',recl=4*1*1)
!      record=0
!        do j = 1,30
!          record=record+1
!          write(15,rec=record)stp(j)
!        enddo
!        record=30
!        do j = 1,30
!          record=record+1
!          write(15,rec=record)sfp(j)
!        enddo
!        record=60
!        do j = 1,30
!          record=record+1
!          write(15,rec=record)temp(j)
!        enddo
!        record=90
!        do j = 1,30
!          record=record+1
!          write(15,rec=record)td(j)
!        enddo
!        record=120
!        do j = 1,30
!          record=record+1
!          write(15,rec=record)rh(j)
!        enddo
!        record=150
!        do j = 1,30
!          record=record+1
!          write(15,rec=record)ws(j)
!        enddo
!        record=180
!        do j = 1,30
!          record=record+1
!          write(15,rec=record)wd(j)
!        enddo
       print*,'oh ya'
!!-------------------------------------------------------------------------------------
      do j = 1,4
        jp=j+2
        lon(j)=121.514853
        lat(j)=25.037658
        write(jpp,'(i1)')jp
        infile='taipei12'//trim(jpp)//'.txt'
        outfile='taipei12'//trim(jpp)//'.dat'
        open(30,file=trim(infile))
        do i = 1,24
          read (30,'(f8.0,f8.1,f8.1,f8.1,f8.1,f8.0,f8.1,f8.0)')num1(i,j),stp1(i,j),sfp1(i,j),temp1(i,j),td1(i,j),rh1(i,j),ws1(i,j),wd1(i,j)
          print*,num1(i,j),stp1(i,j),sfp1(i,j),temp1(i,j),td1(i,j),rh1(i,j),ws1(i,j),wd1(i,j)
        enddo
      enddo
      do j = 5,8
        jp=j-2 
        lon(j)=120.684075
        lat(j)=24.145736
        write(jpp,'(i1)')jp
        infile='taichung12'//trim(jpp)//'.txt'
        outfile='taichung12'//trim(jpp)//'.dat'
        open(31,file=trim(infile))
        do i = 1,24
          read (31,'(f8.0,f8.1,f8.1,f8.1,f8.1,f8.0,f8.1,f8.0)')num1(i,j),stp1(i,j),sfp1(i,j),temp1(i,j),td1(i,j),rh1(i,j),ws1(i,j),wd1(i,j)
          print*,num1(i,j),stp1(i,j),sfp1(i,j),temp1(i,j),td1(i,j),rh1(i,j),ws1(i,j),wd1(i,j)
        enddo
      enddo
      do j = 9,12
        jp=j-6
        lon(j)=120.315733
        lat(j)=22.565992
        write(jpp,'(i1)')jp
        infile='kaohsiung12'//trim(jpp)//'.txt'
        outfile='kaohsiung12'//trim(jpp)//'.dat'
        open(32,file=trim(infile))
        do i = 1,24
          read (32,'(f8.0,f8.1,f8.1,f8.1,f8.1,f8.0,f8.1,f8.0)')num1(i,j),stp1(i,j),sfp1(i,j),temp1(i,j),td1(i,j),rh1(i,j),ws1(i,j),wd1(i,j)
          print*,num1(i,j),stp1(i,j),sfp1(i,j),temp1(i,j),td1(i,j),rh1(i,j),ws1(i,j),wd1(i,j)
        enddo
      enddo
      do j = 13,16
        jp=j-10
        lon(j)=121.613275
        lat(j)=23.975128
        write(jpp,'(i1)')jp
        infile='hualien12'//trim(jpp)//'.txt'
        outfile='hualien12'//trim(jpp)//'.dat'
        open(33,file=trim(infile))
        do i = 1,24
          read (33,'(f8.0,f8.1,f8.1,f8.1,f8.1,f8.0,f8.1,f8.0)')num1(i,j),stp1(i,j),sfp1(i,j),temp1(i,j),td1(i,j),rh1(i,j),ws1(i,j),wd1(i,j)
          print*,num1(i,j),stp1(i,j),sfp1(i,j),temp1(i,j),td1(i,j),rh1(i,j),ws1(i,j),wd1(i,j)
        enddo
      enddo
      do j = 17,20
        jp=j-14
        lon(j)=119.563094
        lat(j)=23.565503
        write(jpp,'(i1)')jp
        infile='penghu12'//trim(jpp)//'.txt'
        outfile='penghu12'//trim(jpp)//'.dat'
        open(34,file=trim(infile))
        do i = 1,24
          read (34,'(f8.0,f8.1,f8.1,f8.1,f8.1,f8.0,f8.1,f8.0)')num1(i,j),stp1(i,j),sfp1(i,j),temp1(i,j),td1(i,j),rh1(i,j),ws1(i,j),wd1(i,j)
          print*,num1(i,j),stp1(i,j),sfp1(i,j),temp1(i,j),td1(i,j),rh1(i,j),ws1(i,j),wd1(i,j)
        enddo
      enddo

      open(40,file='totalstationdata.dat',form='unformatted')!測站都在一張資料裡,一檔案
      do i=1,24!!時間,天數
        time=0.0
        nlev=1
        nflag=1
        do j=1,20
          write(stn11(j),'(i8)') j!在強制將測站名稱改成數字
!          print*,'00000000=',stn11(j)
          write(40) stn11(j),lat(j),lon(j),time,nlev,nflag!一定要降寫
          write(40) stp1(i,j),sfp1(i,j),temp1(i,j),td1(i,j),rh1(i,j),ws1(i,j),wd1(i,j)
        enddo
        nlev=0!宣告這時間結束了
        write(40) stn11(20),lat(20),lon(20),time,nlev,nflag!return nlev=0,這時間結束
      enddo
      write(40) stn11(20),lat(20),lon(20),time,nlev,nflag!回會有連續兩個nlev=0,資料結束
      print*,'00000000=',stn11(19)
      close(40)
      print*,'oh ya ya'
!-------------------------------------------------------------
      stop
      end

