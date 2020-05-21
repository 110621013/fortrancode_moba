      program DataTest1
      implicit none

      integer :: num
      integer,parameter :: hours=8760
      integer :: time(hours)
      real,dimension(hours) :: time,AMB_TEMP,CH4,CO,NMHC,NO1,NO2,NOx,O3
      real,dimension(hours) :: PM10,PM25,RAINFALL,RH,SO2,THC,WD_HR,WS_HR
      real,dimension(hours) ;; u,v

      open(unit=500,file="/homework/106/data/Banqiao_2015.txt")
      open(unit=600,file="banquao.txt")

      do num = 1,24
      read(500,"10i,18(F8.2)")time(num),AMB_TEMP(num),CH4(num),CO(num),NMHC(num),NO1(num),NO2(num),NOx(num),O3(num),PM10(num),PM25(num),RAINFALL(num),RH(num),SO2(num),THC(num),WD_HR(num),WS_HR(num),u(num),v(num)
      write(600,*)
      enddo
      print(*,*)

      close(500)
      close(600)
      stop
      end
