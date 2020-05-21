
        program cwb_skewt

        dimension pp(6000),zz(6000),tt(6000),td(6000),wd(6000),ws(6000)
        character tempfile*100,station*5,time*8
        data spv/-999.0/

c-----------------------------------------------------------------------
c* To open souding data.

        print*,'Enter snd file'
        read(*,'(a)',err=100)tempfile
c       open(10,file=tempfile,form='formatted',status='unknown',err=100)

        print*,'min spacing of wind flag from 1000 mb'
        read*,wsps

c-----------------------------------------------------------------------
c*  read souding data.

        call readsnd(tempfile,station,iy,im,id,ih,pp,tt,td,wd,ws,zz,nn)

c       read(10,10,err=200)station,iy,im,id,ih
c10     format(a5,4i3)
        write(time,'(4i2.2)')mod(iy,100),im,id,ih
c       nn=0
c20     continue
c       read(10,30,end=40,err=300)p,t,d,spd,dir,z
c       nn=nn+1
c       pp(nn)=p
c       zz(nn)=z
c       tt(nn)=t
c       td(nn)=d
c       wd(nn)=dir
c       ws(nn)=spd
c       go to 20
 30     format(3x,6f7.1)
c40     continue

c-----------------------------------------------------------------------
c*  plot skewT diagram.

        call sort(nn,pp,zz,tt,td,wd,ws,spv)
        p0=pp(1)
        z0=zz(1)
        t0=tt(1)
        td0=td(1)
        call skewt(p0,z0,t0,td0,station,time,nn,pp,zz,tt,td,wd,ws,wsps,
     +       ier)
        if( ier.eq.1 )then
            print*,'Cannot plot skewT diagram.'
            call exit(1)
        endif
        stop

 100    continue
        print*,'Cannot open sounding data file.'
        print*,'sounding data file: ',tempfile(1:100)
        call exit(1)
 200    continue
        print*,'Cannot read sounding data.'
        print*,'sounding data file: ',tempfile(1:100)
        call exit(1)        
 300    continue
        print*,'To read sounding data have error.'
        print*,'sounding data file: ',tempfile(1:100)
        print*,'Station Number: ',station(1:5)
        print*,'Time: ',time(1:8)
        do k=1,nn
           write(*,30)pp(k),zz(k),tt(k),td(k),wd(k),ws(k),k
        enddo
        call exit(1)
        end

        subroutine skewt(p0,z0,t0,td0,station,time,no,pp,zz,tt,td,
     +             wd,ws,wsps,ier)

       dimension pp(no),zz(no),tt(no),td(no),wd(no),ws(no)
       character station*5,time*8
       dimension xx(6000),yy(6000),wp(6000),ip(10)
       dimension p_a(6000),t_a(6000),t_e(6000)
       data ip/850,700,600,500,400,300,250,200,150,100/

       ier=0

c* To open G.K.S.

       call opngks

c-----------------------------------------------------------------------
c* To define color index.

       call defcolor

c-----------------------------------------------------------------------
c* To force filling area.

       call gsfais(1)

c-----------------------------------------------------------------------
c* To set the line pattern of black color.

       call gsplci(1)
       call gstxci(1)

c-----------------------------------------------------------------------
c* To plot base map.

       call base_map

c-----------------------------------------------------------------------
c* To write title: station number and date.

       call wt_title(station,time)

c-----------------------------------------------------------------------
c* To plot wind flag.

       nn=0
       do 10 k=1,no
          if( pp(k).lt.100. )go to 10
          if( (wd(k).lt.0.).or.(wd(k).gt.360.) )go to 10
          if( (ws(k).lt.0.).or.(ws(k).gt.200.) )go to 10
          nn=nn+1
          wp(nn)=pp(k)
          xx(nn)=wd(k)
          yy(nn)=ws(k)
 10    continue
       if( nn.gt.0 )call pl_wind(nn,wp,xx,yy,wsps)

c-----------------------------------------------------------------------
c* To write height characters.

       nn=0
       do 15 k=1,no
          if( (pp(k).lt.10.).or.(zz(k).lt.0.) )go to 15
          nn=nn+1
          wp(nn)=pp(k)
          yy(nn)=zz(k)
 15    continue
       if( nn.gt.0 )call wt_ht(nn,wp,yy)

c-----------------------------------------------------------------------
c* To compute LCL, CCL, LFC, EL, CAPE, CIN, the temperature, pressure,
c*    of air parcel and enviroment.

       p_lcl=-999.0
       h_lcl=-999.0
       t_lcl=-999.0
       p_ccl=-999.0
       h_ccl=-999.0
       t_ccl=-999.0
       p_lfc=-999.0
       h_lfc=-999.0
       t_lfc=-999.0
       p_el=-999.0
       h_el=-999.0
       t_el=-999.0
       cape=-999.0
       cin=-999.0
       id_cape=0.
       na=0
       if( (abs(t0).lt.99.0).and.(abs(td0).lt.99.0).and.
     +     (p0.gt.0.)  )then
           nn=1
           wp(nn)=p0
           xx(nn)=z0
           yy(nn)=t0
           do 20 k=1,no
              if( (pp(k).lt.100.).or.(pp(k).ge.p0) )go to 20
              if( (zz(k).lt.0.).or.(abs(tt(k)).ge.99.0) )go to 20
              nn=nn+1
              wp(nn)=pp(k)
              xx(nn)=zz(k)
              yy(nn)=tt(k)
 20        continue
           if( nn.gt.2 )then
               if( xx(1).lt.0. )then
                   ttt=yy(2)+273.15
                   xx(1)=hyd(xx(2),wp(2),ttt,wp(1))
                   zz(1)=xx(1)
                   z0=xx(1)
               endif
               id_cape=1
               call ht_cape(p0,z0,t0,td0,nn,wp,xx,yy,p_lcl,p_ccl,p_lfc,
     +                      p_el,h_lcl,h_ccl,h_lfc,h_el,t_lcl,t_ccl,
     +                      t_lfc,t_el,cape,cin,na,p_a,t_a,t_e,ierr)

               print*,p_lcl,p_ccl,p_lfc,p_el
               print*,h_lcl,h_ccl,h_lfc,h_el
               print*,t_lcl,t_ccl,t_lfc,t_el
               print*,cape,cin

               ier=ierr
               if( ierr.eq.1 )then
                   id_cape=0
                   ier=2
               endif
           endif
       endif    

c-----------------------------------------------------------------------
c* To fill CIN and CAPE area.

       if( id_cape.eq.1 )then
           call fil_energy(p_lfc,t_lfc,p_el,t_el,cape,cin,
     +                     na,p_a,t_a,t_e)
           if( ierr.eq.1 )ier=2
       endif

c-----------------------------------------------------------------------
c* To plot base map.

       call base_map1

c-----------------------------------------------------------------------
c* To plot air-parcel vertical motion line.

       if( id_cape.eq.1 )then
           call parcel_line(p_lcl,p_lfc,p_el,na,p_a,t_a)
       endif

c-----------------------------------------------------------------------
c* To set the line pattern of red color.

       call setusv('LW',2000)
c      call gsplci(2)
c      call gstxci(2)
c
c* To set the line pattern of blue color.
c
        call gsplci(5)
        call gstxci(5)

c-----------------------------------------------------------------------
c* To plot dew-point temperature line.

       nn=0
       do 30 k=1,no
          if( (pp(k).lt.100.).or.(abs(td(k)).ge.99.0) )go to 30
          nn=nn+1
          wp(nn)=pp(k)
          yy(nn)=td(k)
 30    continue
       call pl_t_td(nn,wp,yy)

c-----------------------------------------------------------------------
c* To set the line pattern of blue color.

       call setusv('LW',2000)
c      call gsplci(5)
c      call gstxci(5)

       call gsplci(2)
       call gstxci(2)

c-----------------------------------------------------------------------
c* To plot temperature line below the height of 100 hPa.

       nn=0
       do 40 k=1,no
          if( (pp(k).lt.100.).or.(abs(tt(k)).ge.99.0) )go to 40
          nn=nn+1
          wp(nn)=pp(k)
          yy(nn)=tt(k)
 40    continue
       call pl_t_td(nn,wp,yy)

c-----------------------------------------------------------------------
c* To plot temperature line above the height of 70 hPa.

       nn=0
       do 45 k=1,no
          if( (pp(k).gt.70).or.(pp(k).lt.10.).or.
     +        (abs(tt(k)).ge.99.0) )go to 45
          nn=nn+1
          wp(nn)=pp(k)
          yy(nn)=tt(k)
 45    continue
c       call pl_tt(nn,wp,yy)

c-----------------------------------------------------------------------
c* To compute K Index(Lifted Index), and Total Index.

       nn=0
       do 50 k=1,no
          if( (pp(k).lt.100.).or.(abs(tt(k)).ge.99.0).or.
     +        (abs(td(k)).ge.99.0)  )go to 50
          nn=nn+1
          wp(nn)=pp(k)
          xx(nn)=tt(k)
          yy(nn)=td(k)
 50    continue
       rki=-999.
       rti=-999.
       t8=-999.
       td8=-999.
       t5=-999.
       td5=-999.
       if( nn.gt.3 )then
           call kt_index(nn,wp,xx,yy,rki,rti,t8,td8,t5,td5)
       endif

c-----------------------------------------------------------------------
c* To compute SWEAT Index.

       nn=0
       do 60 k=1,no
          if( pp(k).lt.100. )go to 60
          if( (wd(k).lt.0.).or.(wd(k).gt.360.) )go to 60
          if( (ws(k).lt.0.).or.(ws(k).gt.200.) )go to 60
          nn=nn+1
          wp(nn)=pp(k)
          xx(nn)=wd(k)
          yy(nn)=ws(k)
 60    continue
       si=-999.
       if( nn.gt.3 )then
           call s_index(nn,wp,xx,yy,rti,td8,si)
       endif

c-----------------------------------------------------------------------
c* To compute lifted index and Showalter stability index.

       if( (t8.gt.-900.).and.(td8.gt.-900.).and.
     +     (t5.gt.-900.).and.(td5.gt.-900.) )then
           tc8=1./(1./(td8+273.15-56.)+log((t8+273.15)/
     +                                  (td8+273.15))/800.)+56.
           p8_lcl=850.*((tc8)/(t8+273.15))**(1004./287.)
           the8=(t8+273.15)*(1000./850.)**0.2857
           p_wa8=6.112*exp(17.67*td8/(td8+243.5))
           wa8=0.622*p_wa8/(850.-p_wa8)
           tha8=the8*exp(2675.*wa8/tc8)
           tc5=1./(1./(td5+273.15-56.)+log((t5+273.15)/
     +                                  (td5+273.15))/800.)+56.
           the5=(t5+273.15)*(1000./500.)**0.2857
           p_wa5=6.112*exp(17.67*td5/(td5+243.5))
           wa5=0.622*p_wa5/(500.-p_wa5)
           tha5=the5*exp(2675.*wa5/tc5)
           rlf_ind=tha5-tha8

           if( p8_lcl.le.500. )then
               ta8=tc8*(500./p8_lcl)**0.2857-273.15
           else
               call ta_com(tha8,500.,t8,0.001,50000,ta8,ier)
           endif
           if( ier.eq.2 )then
               show_ind=-999.
           else
               show_ind=t5-ta8
           endif
       else
           rlf_ind=-999.
           show_ind=-999.
       endif

c-----------------------------------------------------------------------
c* To precipitable rainfall: QPF.

       qpf=-999.0
       nn=0
       do 70 k=1,no
          if( (pp(k).lt.300.).or.(abs(td(k)).ge.99.0) )go to 70
          nn=nn+1
          wp(nn)=pp(k)
          xx(nn)=td(k)
 70    continue
       if( nn.ge.3 )call com_qpf(nn,wp,xx,qpf)

c-----------------------------------------------------------------------
c* To write index and other parameters.

       call wt_ind(p0,t0,td0,p_lcl,h_lcl,t_lcl,p_ccl,h_ccl,t_ccl,
     +             p_lfc,h_lfc,t_lfc,p_el,h_el,t_el,qpf,
     +             rki,rti,rlf_ind,show_ind,si,cape,cin)

c-----------------------------------------------------------------------
c* To close G.K.S.

       call frame
       call clsgks
       return
       end

       subroutine wt_ind(p0,t0,td0,p_lcl,h_lcl,t_lcl,p_ccl,h_ccl,t_ccl,
     +                   p_lfc,h_lfc,t_lfc,p_el,h_el,t_el,qpf,
     +                   rki,rti,rlf_ind,show_ind,si,cape,cin)
c***********************************************************************
c subroutine name  : wt_ind                                            *
c                                                                      *
c description      : To write index(K.I., L.I., Showqlter I., T.I.,    *
c                    SWEAT I.) and other parameters( P0, T0, Td0, LCL, *
c                    CCL, LFC, EL, CAPE, CIN, T(LCL), T(CCL), T(LFC),  *
c                    T(EL) and QPF).                                   *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          p0         real        the initial pressure of air parcel   *
c                                 in unit of hPa.                      *
c          t0         real        the initial temperature of air parcel*
c                                 in unit of degree C.                 *
c          td0        real        the initial dew-point temperature of *
c                                 air parcel in unit of degree C.      *
c          p_lcl      real        the pressure of LCL in unit of hPa.  *
c          h_lcl      real        the height of LCL in unit of meters. *
c          t_lcl      real        the temperature of LCL in unit of    *
c                                 degree C.                            *
c          p_ccl      real        the pressure of CCL in unit of hPa.  *
c          h_ccl      real        the height of CCL in unit of meters. *
c          t_ccl      real        the temperature of CCL in unit of    *
c                                 degree C.                            *
c          p_lfc      real        the pressure of LFC in unit of hPa.  *
c          h_lfc      real        the height of LFC in unit of meters. *
c          t_lfc      real        the temperature of LFC in unit of    *
c                                 degree C.                            *
c          p_el       real        the pressure of EL in unit of hPa.   * 
c          h_el       real        the height of EL in unit of meters.  *
c          t_el       real        the temperature of EL in unit of     *
c                                 degree C.                            *
c          rki        real        K-index.                             *
c          rti        real        Total-index.                         *
c          rlf_ind    real        Lifted Index.                        *
c          show)ind   real        Showalter stability index.           *
c          si         real        SWEAT-index.                         *
c          cape       real        the positive parcel energy of CAPE   *
c                                 in unit of (m**2)(s**2).             *
c          cin        real        the negative parcel energy of CIN    *
c                                 in unit of (m**2)(s**2).             *
c                                                                      *
c called fun./sub. : set,setusv,gsplci,                                *
c                    gstxci,plchhq -- NCAR GRAPHICS                    *
c                                                                      *
c***********************************************************************

       character ca4*4,ca5*5,ca6*6,ca15*15,ca28*28,ca24*24,ca26*26
       character ca27*27,ca57*57,ca46*46,ca12*12,ca20*20
       call set(0.6,0.8,0.66875,0.9,0.,1.,0.,1.,1)

c-----------------------------------------------------------------------
c* To set black color for write characters.

       call setusv('LW',1000)
       call gsplci(1)
       call gstxci(1)
       kk=0

c-----------------------------------------------------------------------
c* To P0,T0,Td0.

       if( (p0.gt.0.).and.(abs(t0).lt.99.0).and.(abs(td0).lt.99.0) )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           ca46(1:9)='P:B:o:N:='
           write(ca6,'(f6.1)')p0
           ca46(10:15)=ca6(1:6)
           ca46(16:25)=' T:B:o:N:='
           write(ca5,'(f5.1)')t0
           ca46(26:30)=ca5(1:5)
           ca46(31:41)=' Td:B:o:N:='
           write(ca5,'(f5.1)')td0
           ca46(42:46)=ca5(1:5)
           call plchhq(0.01,y,ca46,0.007,0.,-1.)
       endif
           
c-----------------------------------------------------------------------
c* To write L.C.L.

       ip_lcl=p_lcl+0.5
       write(ca4,'(i4)')ip_lcl
       if( h_lcl.gt.0. )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           z=0.001*h_lcl
           if( z.lt.10.0 )then
               write(ca5,'(f5.3)')z
               if( ca5(1:1).eq.' ' )ca5(1:1)='0'
           else
               write(ca5,'(f5.2)')z
           endif
           ca26(1:7)='L.C.L.='
           ca26(8:11)=ca4(1:4)
           ca26(12:15)=' hPa'
           ca26(16:17)=' ('
           ca26(18:22)=ca5(1:5)
           ca26(23:26)=' km)'
           call plchhq(0.01,y,ca26,0.007,0.,-1.)
       else
           ca15(1:7)='L.C.L.='
           ca15(8:11)=ca4(1:4)
           ca15(12:15)=' hPa'
           call plchhq(0.01,y,ca15,0.007,0.,-1.)
       endif

c-----------------------------------------------------------------------
c* To write C.C.L.

       if( p_ccl.gt.0. )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           ip_ccl=p_ccl+0.5
           write(ca4,'(i4)')ip_ccl
           if( h_ccl.gt.0. )then
               z=0.001*h_ccl
               if( z.lt.10.0 )then
                   write(ca5,'(f5.3)')z
                   if( ca5(1:1).eq.' ' )ca5(1:1)='0'
               else
                   write(ca5,'(f5.2)')z
               endif
               ca26(1:7)='C.C.L.='
               ca26(8:11)=ca4(1:4)
               ca26(12:15)=' hPa'
               ca26(16:17)=' ('
               ca26(18:22)=ca5(1:5)
               ca26(23:26)=' km)'
               call plchhq(0.01,y,ca26,0.007,0.,-1.)
           else
               ca15(1:7)='C.C.L.='
               ca15(8:11)=ca4(1:4)
               ca15(12:15)=' hPa'
               call plchhq(0.01,y,ca15,0.007,0.,-1.)
           endif
       endif

c-----------------------------------------------------------------------
c* To write L.F.C.

       if( p_lfc.gt.0. )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           ip_lfc=p_lfc+0.5
           write(ca4,'(i4)')ip_lfc
           z=0.001*h_lfc
           if( z.lt.10.0 )then
               write(ca5,'(f5.3)')z
               if( ca5(1:1).eq.' ' )ca5(1:1)='0'
           else
               write(ca5,'(f5.2)')z
           endif
           ca26(1:7)='L.F.C.='
           ca26(8:11)=ca4(1:4)
           ca26(12:15)=' hPa'
           ca26(16:17)=' ('
           ca26(18:22)=ca5(1:5)
           ca26(23:26)=' km)'
           call plchhq(0.01,y,ca26,0.007,0.,-1.)
       endif

c-----------------------------------------------------------------------
c* To write E.L.

       if( p_el.gt.0. )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           ip_el=p_el+0.5
           write(ca4,'(i4)')ip_el
           z=0.001*h_el
           if( z.lt.10.0 )then
               write(ca5,'(f5.3)')z
               if( ca5(1:1).eq.' ' )ca5(1:1)='0'
           else
               write(ca5,'(f5.2)')z
           endif
           ca24(1:5)='E.L.='
           ca24(6:9)=ca4(1:4)
           ca24(10:13)=' hPa'
           ca24(14:15)=' ('
           ca24(16:20)=ca5(1:5)
           ca24(21:24)=' km)'
           call plchhq(0.01,y,ca24,0.007,0.,-1.)
       endif

c-----------------------------------------------------------------------
c* To T(LCL) and T(CCL)

       if( (abs(t_lcl).lt.900.).and.(abs(t_ccl).lt.900.) )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           ca57(1:15)='T:B3:LCL:N:   ='
           write(ca5,'(f5.1)')t_lcl
           ca57(16:20)=ca5(1:5)
           ca57(21:28)=':S:o:N:C'
           ca57(29:44)=' T:B3:CCL:N:   ='
           write(ca5,'(f5.1)')t_ccl
           ca57(45:49)=ca5(1:5)
           ca57(50:57)=':S:o:N:C'
           call plchhq(0.01,y,ca57,0.007,0.,-1.)
       endif
       if( (abs(t_lcl).lt.900.).and.(abs(t_ccl).gt.900.) )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           ca28(1:15)='T:B3:LCL:N:   ='
           write(ca5,'(f5.1)')t_lcl
           ca28(16:20)=ca5(1:5)
           ca28(21:28)=':S:o:N:C'
           call plchhq(0.01,y,ca28,0.007,0.,-1.)
       endif
       if( (abs(t_ccl).lt.900.).and.(abs(t_lcl).gt.900.) )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           ca28(1:15)='T:B3:CCL:N:   ='
           write(ca5,'(f5.1)')t_ccl
           ca28(16:20)=ca5(1:5)
           ca28(21:28)=':S:o:N:C'
           call plchhq(0.01,y,ca28,0.007,0.,-1.)
       endif

c-----------------------------------------------------------------------
c* To T(LFC) and T(EL)

       if( (abs(t_lfc).lt.900.).and.(abs(t_el).lt.900.) )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           ca57(1:15)='T:B3:LFC:N:   ='
           write(ca5,'(f5.1)')t_lfc
           ca57(16:20)=ca5(1:5)
           ca57(21:28)=':S:o:N:C'
           ca57(29:44)='  T:B2:EL:N:   ='
           write(ca5,'(f5.1)')t_el
           ca57(45:49)=ca5(1:5)
           ca57(50:57)=':S:o:N:C'
           call plchhq(0.01,y,ca57,0.007,0.,-1.)
       endif
       if( (abs(t_lfc).lt.900.).and.(abs(t_el).gt.900.) )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           ca28(1:15)='T:B3:LFC:N:   ='
           write(ca5,'(f5.1)')t_lfc
           ca28(16:20)=ca5(1:5)
           ca28(21:28)=':S:o:N:C'
           call plchhq(0.01,y,ca28,0.007,0.,-1.)
       endif
       if( (abs(t_el).lt.900.).and.(abs(t_lfc).gt.900.) )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           ca28(1:15)='T:B2:EL:N:   = '
           write(ca5,'(f5.1)')t_el
           ca28(16:20)=ca5(1:5)
           ca28(21:28)=':S:o:N:C'
           call plchhq(0.01,y,ca28,0.007,0.,-1.)
       endif

c-----------------------------------------------------------------------
c* To write K. INDX. and TOTAL.

       if( (rki.gt.-900.).and.(rti.gt.-900.) )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           write(ca5,'(f5.1)')rki
           ca27(1:8)='K. INDX='
           ca27(9:13)=ca5(1:5)
           ca27(14:22)='  TOTAL.='
           write(ca5,'(f5.1)')rti
           ca27(23:27)=ca5(1:5)
           call plchhq(0.01,y,ca27,0.007,0.,-1.)
       endif

c-----------------------------------------------------------------------
c* To write Lifted Index.

       if( rlf_ind.gt.-900. )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           write(ca5,'(f5.1)')rlf_ind
           ca20(1:15)='Lifted INDX. = '
           ca20(16:20)=ca5(1:5)
           call plchhq(0.01,y,ca20,0.007,0.,-1.)
       endif

c-----------------------------------------------------------------------
c* To write Showalter stability Index.

       if( show_ind.gt.-900. )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           write(ca5,'(f5.1)')show_ind
           ca24(1:19)='Showalter INDX. = '
           ca24(20:24)=ca5(1:5)
           call plchhq(0.01,y,ca24,0.007,0.,-1.)
       endif

c-----------------------------------------------------------------------
c* To write SWEAT Index.

       if( si.gt.0. )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           write(ca5,'(f5.1)')si
           ca20(1:15)='SWEAT INDX. =  '
           ca20(16:20)=ca5(1:5)
           call plchhq(0.01,y,ca20,0.007,0.,-1.)
       endif

c-----------------------------------------------------------------------
c* To write CAPE.

       if( cape.ge.0. )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           write(ca6,'(f6.1)')cape
           if( ca6(4:4).eq.' ' )ca6(4:4)='0'
           ca27(1:7)='CAPE = '
           ca27(8:13)=ca6(1:6)
           ca27(14:27)=' m:S:2:N:s:S:2'
           call plchhq(0.01,y,ca27,0.007,0.,-1.)
       endif

c-----------------------------------------------------------------------
c* To write CIN.

       if( cin.ge.0. )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           write(ca6,'(f6.1)')cin
           if( ca6(4:4).eq.' ' )ca6(4:4)='0'
           ca27(1:7)='CIN  = '
           ca27(8:13)=ca6(1:6)
           ca27(14:27)=' m:S:2:N:s:S:2'
           call plchhq(0.01,y,ca27,0.007,0.,-1.)
       endif

c-----------------------------------------------------------------------
c* To write QPF

       if( qpf.gt.0. )then
           kk=kk+1
           y=0.96-0.07*(kk-1)
           write(ca5,'(f5.1)')qpf
           ca12(1:4)='QPF='
           ca12(5:9)=ca5(1:5)
           ca12(10:12)=' mm'
           call plchhq(0.01,y,ca12,0.007,0.,-1.)
       endif

       return
       end

       subroutine com_qpf(nn,pp,td,qpf)
c***********************************************************************
c subroutine name  : com_qpf                                           *
c                                                                      *
c description      : To compute precipitable rainfall.                 *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          nn         integer     the number of sounding data.         *
c          pp(nn)     real array  the pressure in unit of hPa.         *
c          td(nn)     real array  the dew-point temperature in unit of *
c                                 degree C.                            *
c  output: name       type        description                          *
c          qpf        real        precipitable rainfall in unit of mm. *
c                                                                      *
c called fun./sub. : com_energy                                        *
c                                                                      *
c***********************************************************************
       dimension pp(nn),td(nn),wxx(6000),wyy(6000)

       if( nn.le.3 )return

c-----------------------------------------------------------------------
c* To compute specific humidity.

       do k=1,nn
          wxx(k)=pp(k)
          ttd=td(k)
          p_w=6.112*exp(17.67*ttd/(ttd+243.5))
          wyy(k)=6.22*p_w/(pp(k)-0.378*p_w)/9.81
       enddo

       call com_energy(nn,wxx,wyy,qpf)
       if( abs(qpf).lt.900. )then
           qpf=-10.*qpf
       endif

       return
       end

       subroutine s_index(nn,pp,wd,ws,rti,td8,si)
c***********************************************************************
c subroutine name  : s_index                                           *
c                                                                      *
c description      : To compute SWEAT index.                           *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          nn         integer     the number of sounding data.         *
c          pp(nn)     real array  the pressure in unit of hPa.         *
c          wd(nn)     real array  wind direction.                      *
c          ws(nn)     real array  wind speed in unit of meters.        *
c          rti        real        Total-index.                         *
c          td8        real        dew-point temperature at 850 hPa.    *
c  output: name       type        description                          *
c          si         real        SWEAT-index.                         *
c                                                                      *
c called fun./sub. : none                                              *
c                                                                      *
c***********************************************************************
       dimension pp(nn),wd(nn),ws(nn)

       if( (td8.lt.-900.).or.(rti.lt.-900.) )return
       f8=-999.
       f5=-999.
       d8=-999.
       d5=-999.

c-----------------------------------------------------------------------
c* To select f8,f5,d8 and d5.

       do k=1,nn
          ip=pp(k)
          if( ip.eq.850 )then
              f8=2.0*ws(k)
              d8=wd(k)
          endif
          if( ip.eq.500 )then
              f5=2.0*ws(k)
              d5=wd(k)
          endif
       enddo

c-----------------------------------------------------------------------
c* To compute SWEAT Index.

       if( (f8.gt.-900.).and.(d8.gt.-900.).and.
     +     (f5.gt.-900.).and.(d5.gt.-900.) )then
           if( td8.gt.0. )si=12.*td8
           if( rti.gt.44. )si=si+20.*(rti-44.)
           si=si+2.*f8+f5
           if( (d8.gt.250.).or.(d8.lt.130.) )return
           if( (d5.gt.310.).or.(d5.lt.210.) )return
           if( (d5-d8).lt.0.0 )return
           if( (f8.lt.15.).or.(f5.lt.15.) )return
           si=si+125.*(sin(3.1415962*(d5-d8)/180.)+0.2)
           return
       endif
       if( (pp(nn).gt.500.).or.(pp(1).lt.850.) )return
           
c-----------------------------------------------------------------------
c* To interapolate f8,f5,d8 and d5

       do k=1,nn-1
          if( (pp(k).ge.850.).and.(pp(k+1).lt.850.) )then
               rlog8=log10(850.)
               rlogt=log10(pp(k+1))
               rlogd=log10(pp(k))
               f8=ws(k)+(rlog8-rlogd)*(ws(k+1)-ws(k))/(rlogt-rlogd)
               d8=wd(k)+(rlog8-rlogd)*(wd(k+1)-wd(k))/(rlogt-rlogd)
               f8=2.*f8
          endif
          if( (pp(k).ge.500.).and.(pp(k+1).lt.500.) )then
               rlog5=log10(500.)
               rlogt=log10(pp(k+1))
               rlogd=log10(pp(k))
               f5=ws(k)+(rlog5-rlogd)*(ws(k+1)-ws(k))/(rlogt-rlogd)
               d5=wd(k)+(rlog5-rlogd)*(wd(k+1)-wd(k))/(rlogt-rlogd)
               f5=2.*f5
          endif
       enddo

c-----------------------------------------------------------------------
c* To compute SWEAT Index.

       if( (f8.gt.-900.).and.(d8.gt.-900.).and.
     +     (f5.gt.-900.).and.(d5.gt.-900.) )then
           if( td8.gt.0. )si=td8
           if( rti.gt.44. )si=si+20.*(rti-44.)
           si=si+2.*f8+f5
           if( (d8.gt.250.).or.(d8.lt.130.) )return
           if( (d5.gt.310.).or.(d5.lt.210.) )return
           if( (d5-d8).lt.0.0 )return
           if( (f8.lt.15.).or.(f5.lt.15.) )return
           si=si+(sin(3.1415962*(d5-d8)/180.)+0.2)
           return
       endif

       return
       end

       subroutine kt_index(nn,pp,tt,td,rki,rti,t8,td8,t5,td5)
c***********************************************************************
c subroutine name  : kt_index                                          *
c                                                                      *
c description      : To compute K-index, Total-index and dew-point     *
c                    temperature at 850 hPa.                           *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          nn         integer     the number of sounding data.         *
c          pp(nn)     real array  the pressure in unit of hPa.         *
c          tt(nn)     real array  the temperature in unit of degree C. *
c          td(nn)     real array  dew-point temperature in unit of C.  *
c  output: name       type        description                          *
c          rki        real        K-index.                             *
c          rti        real        Total-index.                         *
c          t8         real        temperature at 850 hPa.              *
c          td8        real        dew-point temperature at 850 hPa.    *
c          t5         real        temperature at 500 hPa.              *
c          td4        real        dew-point temperature at 500 hPa.    *
c                                                                      *
c called fun./sub. : none                                              *
c                                                                      *
c***********************************************************************
       dimension pp(nn),tt(nn),td(nn)

       t8=-999.
       td8=-999.
       t7=-999.
       td7=-999.
       t5=-999.
       td5=-999.

c-----------------------------------------------------------------------
c* To select t8,td8,t7,td7, and t5.

       do k=1,nn
          ip=pp(k)
          if( ip.eq.850 )then
              t8=tt(k)
              td8=td(k)
          endif
          if( ip.eq.700 )then
              t7=tt(k)
              td7=td(k)
          endif
          if( ip.eq.500 )then
              t5=tt(k)
              td5=td(k)
          endif
       enddo

c-----------------------------------------------------------------------
c* To compute K-Index and Total-Index.

       if( (t8.gt.-900.).and.(td8.gt.-900.).and.(t7.gt.-900.).and.
     +     (td7.gt.-900.).and.(t5.gt.-900.) )then
           rki=t8-t5+td8-(t7-td7)
           rti=t8+td8-2.*t5
           return
       endif
       if( (pp(nn).gt.500.).or.(pp(1).lt.850.) )return

c-----------------------------------------------------------------------
c* To interapolate t8,td8,t7,td7, and t5.

       do k=1,nn-1
          if( (pp(k).ge.850.).and.(pp(k+1).lt.850.) )then
              rlog8=log10(850.)
              rlogt=log10(pp(k+1))
              rlogd=log10(pp(k))
              t8=tt(k)+(rlog8-rlogd)*(tt(k+1)-tt(k))/(rlogt-rlogd)
              td8=td(k)+(rlog8-rlogd)*(td(k+1)-td(k))/(rlogt-rlogd)
          endif
          if( (pp(k).ge.700.).and.(pp(k+1).lt.700.) )then
              rlog7=log10(700.)
              rlogt=log10(pp(k+1))
              rlogd=log10(pp(k))
              t7=tt(k)+(rlog7-rlogd)*(tt(k+1)-tt(k))/(rlogt-rlogd)
              td7=td(k)+(rlog7-rlogd)*(td(k+1)-td(k))/(rlogt-rlogd)
          endif
          if( (pp(k).ge.500.).and.(pp(k+1).lt.500.) )then
              rlog5=log10(500.)
              rlogt=log10(pp(k+1))
              rlogd=log10(pp(k))
              t5=tt(k)+(rlog5-rlogd)*(tt(k+1)-tt(k))/(rlogt-rlogd)
              td5=td(k)+(rlog5-rlogd)*(td(k+1)-td(k))/(rlogt-rlogd)
          endif
       enddo

c-----------------------------------------------------------------------
c* To compute K-Index and Total-Index.

       if( (t8.gt.-900.).and.(td8.gt.-900.).and.(t7.gt.-900.).and.
     +     (td7.gt.-900.).and.(t5.gt.-900.) )then
           rki=t8-t5+td8-(t7-td7)
           rti=t8+td8-2.*t5
           return
       endif

       return
       end

       subroutine fil_energy(p_lfc,t_lfc,p_el,t_el,cape,cin,
     +                       na,p_a,t_a,t_e)
c***********************************************************************
c subroutine name  : fil_energy                                        *
c                                                                      *
c description      : To file CIN and CAPE area.                        *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          p_lfc      real        the pressure of LFC in unit of hPa.  *
c          t_lfc      real        the temperature of LFC in unit of    *
c                                 degree C.                            *
c          p_el       real        the pressure of EL in unit of hPa.   * 
c          t_el       real        the temperature of EL in unit of     *
c                                 degree C.                            *
c          cape       real        the positive parcel energy of CAPE   *
c                                 in unit of (m**2)(s**2).             *
c          cin        real        the negative parcel energy of CIN    *
c                                 in unit of (m**2)(s**2).             *
c          na         integer     the number of air parcel profile.    *
c          p_a(na)    real array  pressure of air parcel profile.      *
c          t_a(na)    real array  temperature of air parcel profile.   *
c          t_e(na)    real array  temperature of environment to the    *
c                                                                      *
c called fun./sub. : setusv,gsfaci,gsplci,gstxci,                      *
c                    sfseti,sfsgfa     ---- NCAR GRAPHICS              *
c                                                                      *
c***********************************************************************

       dimension p_a(na),t_a(na),t_e(na)
       dimension xx(6000),yy(6000),dst(8000),ind(8000)

       if( na.lt.2 )return

c-----------------------------------------------------------------------
c* To set blue color for filling negative area(CIN).
 
c       call setusv('LW',1000)
c       call gsfaci(5)
c       call gsplci(5)
c       call gstxci(5)

c-----------------------------------------------------------------------
c* To set black color for filling negative area(CIN).
 
       call setusv('LW',1000)
       call gsfaci(1)
       call gsplci(1)
       call gstxci(1)

c-----------------------------------------------------------------------
c* To fill negative area(CIN).

       do 10 k=2,na
          if( p_lfc.gt.p_a(k) )then
              k1=k-1
              go to 15
          endif
 10    continue
       return
 15    continue
       detp=1.0413927
       rlog_1100=3.0413927
       detx=60.
       call set(0.2,0.8,0.1,0.9,0.,1.,0.,1.,1)
       do k=1,k1
          rlog_p=log10(p_a(k))
          t0=(rlog_p-2.)*80./detp-100.
          xx(k)=(t_a(k)-t0)/detx 
          yy(k)=(rlog_1100-rlog_p)/detp
       enddo
       nn=k1+1
       rlog_p=log10(p_lfc)
       t0=(rlog_p-2.)*80./detp-100.
       xx(nn)=(t_lfc-t0)/detx 
       yy(nn)=(rlog_1100-rlog_p)/detp
       do k=k1,2,-1
          rlog_p=log10(p_a(k))
          t0=(rlog_p-2.)*80./detp-100.
          nn=nn+1
          xx(nn)=(t_e(k)-t0)/detx 
          yy(nn)=(rlog_1100-rlog_p)/detp
       enddo
       if( cin.gt.0. )then
           call sfseti('TY',-3)
           call sfsgfa(xx,yy,nn,dst,8000,ind,8000,9)
       endif


       if( cape.le.0. )return

c-----------------------------------------------------------------------
c* To set red color for filling positive area(CAPE).
 
c       call setusv('LW',2000)
c       call gsfaci(2)
c       call gsplci(2)
c       call gstxci(2)

c-----------------------------------------------------------------------
c* To set blue color for filling positive area(CAPE).
 
       call gsfaci(5)
       call gsplci(5)
       call gstxci(5)

c-----------------------------------------------------------------------
c* To fill positive area(CAPE).

       do 20 k=k1+1,na,1
          if( p_el.gt.p_a(k) )then
              k2=k-1
              go to 25
          endif
 20    continue
       return
 25    continue
       nn=1
       rlog_p=log10(p_lfc)
       t0=(rlog_p-2.)*80./detp-100.
       xx(nn)=(t_lfc-t0)/detx 
       yy(nn)=(rlog_1100-rlog_p)/detp
       do k=k1+1,k2,1
          nn=nn+1
          rlog_p=log10(p_a(k))
          t0=(rlog_p-2.)*80./detp-100.
          xx(nn)=(t_e(k)-t0)/detx 
          yy(nn)=(rlog_1100-rlog_p)/detp
       enddo
       nn=nn+1
       rlog_p=log10(p_el)
       t0=(rlog_p-2.)*80./detp-100.
       xx(nn)=(t_el-t0)/detx 
       yy(nn)=(rlog_1100-rlog_p)/detp
       do k=k2,k1+1,-1
          nn=nn+1
          rlog_p=log10(p_a(k))
          t0=(rlog_p-2.)*80./detp-100.
          xx(nn)=(t_a(k)-t0)/detx 
          yy(nn)=(rlog_1100-rlog_p)/detp
       enddo
       call sfseti('TY',-2)
       call sfsgfa(xx,yy,nn,dst,8000,ind,8000,6)
c       call sfseti('TY',-3)
c       call sfsgfa(xx,yy,nn,dst,602,ind,604,9)
       call setusv('LW',1000)

       return
       end

       subroutine ht_cape(p0,zz0,t0,td0,nk,p,z,t,p_lcl,p_ccl,p_lfc,
     +                    p_el,h_lcl,h_ccl,h_lfc,h_el,t_lcl,t_ccl,
     +                    t_lfc,t_el,cape,cin,ii,p_a,t_a,t_e,ier)
c***********************************************************************
c subroutine name  : ht_cape                                           *
c                                                                      *
c description      : To compute lifting condensation level(LCL),       *
c                    convective condensation level(CCL),level of free  *
c                    convection(LFC), equilibrium level(EL),           *
c                    convective available potential energy(CAPE) and   * 
c                    negative energy(CIN).                             *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          p0         real        the initial pressure of air parcel   *
c                                 in unit of hPa.                      *
c          zz0        real        the initial height of air parcel     *
c                                 in unit of meters.                   *
c          t0         real        the initial temperature of air parcel*
c                                 in unit of degree C.                 *
c          td0        real        the initial dew-point temperature of *
c                                 air parcel in unit of degree C.      *
c          nk         integer     the number of sounding profile.      *
c          p(nk)      real array  the pressure of sounding profile in  *
c                                 unit of hPa.                         *
c                                 p(1) . p(2) > p(3) > ... > p(nk)     *
c          z(nk)      real array  the height of sounding profile in    *
c                                 unit of meters.                      *
c                                 z(1) < z(2) < z(3) < ... < z(nk)     *
c          t(nk)      real array  the temperature of sounding profile  *
c                                 unit of degree C.                    *
c                                 t(1) >= t(2) >= t(3) >= ... >= t(nk) *
c  output: name       type        description                          *
c          p_lcl      real        the pressure of LCL in unit of hPa.  *
c          p_ccl      real        the pressure of CCL in unit of hPa.  *
c          p_lfc      real        the pressure of LFC in unit of hPa.  *
c          p_el       real        the pressure of EL in unit of hPa.   * 
c          h_lcl      real        the height of LCL in unit of meters. *
c          h_ccl      real        the height of CCL in unit of meters. *
c          h_lfc      real        the height of LFC in unit of meters. *
c          h_el       real        the height of EL in unit of meters.  *
c          t_lcl      real        the temperature of LCL in unit of    *
c                                 degree C.                            *
c          t_ccl      real        the temperature of CCL in unit of    *
c                                 degree C.                            *
c          t_lfc      real        the temperature of LFC in unit of    *
c                                 degree C.                            *
c          t_el       real        the temperature of EL in unit of     *
c                                 degree C.                            *
c          cape       real        the positive parcel energy of CAPE   *
c                                 in unit of (m**2)(s**2).             *
c          cin        real        the negative parcel energy of CIN    *
c                                 in unit of (m**2)(s**2).             *
c          ii         integer     the number of air parcel profile.    *
c          p_a(200)   real array  pressure of air parcel profile.      *
c          t_a(200)   real array  temperature of air parcel profile.   *
c          t_e(200)   real array  temperature of environment to the    *
c          ier        integer     error message,                       *
c                                 = 0, no error,                       *
c                                 = 1, input data is error.            *
c                                                                      *
c other parameters :                                                   *
c          name       type        description                          *
c          g          real        gravity at sea level.                *
c          cp         real        specific heat of dry air at constant *
c                                 pressure.                            *
c          rgas       real        constant, = 0.622                    *
c          z_a(200)   real array  height of air parcel profile.        *
c                                 parcel.                              *
c                                                                      *
c called fun./sub. : hyd      fit      ta_com       com_energy         *
c                                                                      *
c***********************************************************************

        parameter( g=9.81,cp=1004.,rgas=287.,aslon=0.622 )
        dimension p(nk),z(nk),t(nk)
        dimension p_a(6000),t_a(6000),z_a(6000),t_e(6000),qws(6000)
        dimension wxx(6000),wyy(6000)
        
        ier=0

        tt=t0+273.15
        ttd=td0+273.15

c-----------------------------------------------------------------------
c* To compute potential temperature of air parcel.

        theta0=tt*(1000./p0)**(rgas/cp)

c-----------------------------------------------------------------------
c* To compute isentropic condensation temperature(K) of air parcel.

        tc0=1./(1./(ttd-56.)+log(tt/ttd)/800.)+56.

c-----------------------------------------------------------------------
c* To compute mixing ratio of air parcel.

        p_w0=6.112*exp(17.67*td0/(td0+243.5))
        w0=aslon*p_w0/(p0-p_w0)

c-----------------------------------------------------------------------
c* To compute equivalent potential temperature of air parcel.

        if( p0.gt.100. )then
          thetae0=theta0*exp(2675.*w0/tc0)
        else
          thetae0=theta0
        endif

c-----------------------------------------------------------------------
c* To compute the height of LCL.

        p_lcl=p0*(tc0/tt)**(cp/rgas)
        t_lcl=tc0-273.15
        do k=1,nk-1
           if( (p_lcl.le.p(k)).and.(p_lcl.gt.p(k+1)) )then
               tbar1=0.5*(t(k)+t_lcl+546.3)
               tbar2=0.5*(t(k+1)+t_lcl+546.3)
               za1=hyd(z(k),p(k),tbar1,p_lcl)
               za2=hyd(z(k+1),p(k+1),tbar2,p_lcl)
               h_lcl=0.5*(za1+za2) 
           endif
        enddo

c-----------------------------------------------------------------------
c* To compute the height of CCL.

        q0=1000.*w0
        do k=1,nk
           p_w=6.112*exp(17.67*t(k)/(t(k)+243.5))
           qws(k)=622.*p_w/(p(k)-p_w)
        enddo
        do k=1,nk-1
          if( (q0.le.qws(k)).and.(q0.gt.qws(k+1)) )then
            deta=(p_lcl-p0)*(t(k+1)-t(k))-(p(k+1)-p(k))*(t_lcl-td0)
            if( abs(deta).gt.0.0000001 )then
               dn=( (p(k)-p0)*(t_lcl-td0)-(p_lcl-p0)*(t(k)-td0) )/deta
               t_ccl=t(k)+(t(k+1)-t(k))*dn
               p_ccl=p(k)+(p(k+1)-p(k))*dn
               tbar1=0.5*(t(k)+t_ccl+546.3)
               tbar2=0.5*(t(k+1)+t_ccl+546.3)
               za1=hyd(z(k),p(k),tbar1,p_ccl)
               za2=hyd(z(k+1),p(k+1),tbar2,p_ccl)
               h_ccl=0.5*(za1+za2)
            endif
          endif
        enddo
        if( td0.ge.t0 )then
            p_lcl=p0
            h_lcl=zz0
            t_lcl=t0
            p_ccl=p0
            h_ccl=zz0
            t_ccl=t0
        endif

c-----------------------------------------------------------------------
c* To compute pressure, temperature, height of air parcel and 
c*            environment temperature.

c-----------------------------------------------------------------------
c* To compute p_a(1), z_a(1), t_a(1) and t_e(1)

        ii=1
        p_a(ii)=p0
        z_a(ii)=zz0
        t_a(ii)=t0
        t_e(ii)=t0

c-----------------------------------------------------------------------
c* To compute p_a, z_a, t_a and t_e for p0 >= p(k) >= p_lcl.
        
        if( p(2).le.p_lcl )then
            kk=2
            ii=ii+1
            k_lcl=ii
            p_a(ii)=p_lcl
            z_a(ii)=h_lcl
            t_a(ii)=t_lcl
            t_e(ii)=t(1)+(h_lcl-z(1))*(t(2)-t(1))/(z(2)-z(1))
            if( td0.ge.t0 )then
                ii=1
                k_lcl=1
            endif
            go to 10
        endif
        do k=2,nk-1
           if( (p(k).lt.p0).and.(p(k).gt.p_lcl) )then
               ii=ii+1
               kk=k
               p_a(ii)=p(k)
               z_a(ii)=z(k)
               t_e(ii)=t(k)
               t_a(ii)=theta0*(p(k)/1000.)**(rgas/cp)-273.15
           endif
        enddo
        kk=kk+1
        if( kk.ge.nk )then
            ier=2
            return
        endif

c-----------------------------------------------------------------------
c* To compute p_a, z_a, t_a and t_e for p_lcl >= p(k).

        ii=ii+1
        p_a(ii)=p_lcl
        t_a(ii)=tc0-273.15
        k_lcl=ii

c-----------------------------------------------------------------------
c* For condition: kk > 1 , p(kk-1) > p_lcl >= p(kk)

        tbar=0.5*(t(kk-1)+t(kk)+546.3)
        z1=hyd(z(kk-1),p(kk-1),tbar,p_a(ii))
        z2=hyd(z(kk),p(kk),tbar,p_a(ii))
        z_a(ii)=0.5*(z1+z2)
        t_e(ii)=fit(z(kk-1),t(kk-1),z(kk),t(kk),z_a(ii))

c-----------------------------------------------------------------------
c* To compute p_a(i), t_a(i), z_a(i), t_e(i), and t_a(i) i > 1

 10     continue
        do 40 i=kk,nk,1
           ii=ii+1
           t_e(ii)=t(i)
           p_a(ii)=p(i)
           z_a(ii)=z(i)
          call ta_com(thetae0,p_a(ii),t_a(ii-1),0.001,50000,t_a(ii),ier)
           if( ier.eq.2 )return
 40     continue
         
        if( ii.lt.2 )then
          ier=2
          return
        endif

c-----------------------------------------------------------------------
c* To compute the height of LFC.

        if( t_a(k_lcl).ge.t_e(k_lcl) )then
            p_lfc=p_lcl
            h_lfc=h_lcl
            t_lfc=t_lcl
            i1=k_lcl
            cin=0.
            go to 60
        endif
        do 50 k=k_lcl+1,ii-1,1
           if( t_a(k).ge.t_e(k) )then
               detx=t_a(k)-t_a(k-1)-t_e(k)+t_e(k-1)
               if( abs(detx).lt.0.0000001 )then
                   ier=2
                   return
               endif
               dm=(t_e(k-1)-t_a(k-1))/detx
               rpa1=log(p_a(k-1))+(log(p_a(k))-log(p_a(k-1)))*dm
               pa1=exp(rpa1)
               te1=t_a(k-1)+(t_a(k)-t_a(k-1))*dm
               call ta_com(thetae0,pa1,t_a(k-1),0.001,50000,ta1,ier)
               if( ier.eq.2 )return
               detx=ta1-t_a(k-1)-te1+t_e(k-1)
               if( abs(detx).lt.0.0000001 )then
                   ier=2
                   return
               endif
               dm=(t_e(k-1)-t_a(k-1))/detx
               rpa2=log(p_a(k-1))+(rpa1-log(p_a(k-1)))*dm
               pa2=exp(rpa2)
               te2=t_a(k-1)+(ta1-t_a(k-1))*dm
               call ta_com(thetae0,pa2,t_a(k-1),0.001,50000,ta2,ier)
               p_lfc=pa2
               t_lfc=ta2
               tbar1=0.5*(t_e(k-1)+t_lfc+546.3)
               tbar2=0.5*(t_e(k)+t_lfc+546.3)
               za1=hyd(z_a(k-1),p_a(k-1),tbar1,p_lfc)
               za2=hyd(z_a(k),p_a(k),tbar2,p_lfc)
               h_lfc=0.5*(za1+za2)
               i1=k-1
               go to 60
           endif
 50     continue
        ier=2
        return
 60     continue
        if( td0.ge.t0 )then
            p_lcl=p0
            h_lcl=zz0
            t_lcl=t0
            p_ccl=p0
            t_ccl=t0
            p_lfc=p0
            h_lfc=zz0
            t_lfc=t0
            go to 65
        endif

c-----------------------------------------------------------------------
c* To compute CIN.

        if( i1.ge.2 )then
            nn=0
            do k=1,i1
               value=g*(t_e(k)-t_a(k))/(t_e(k)+273.15)
               if( value.ge.0.0 )then
                   nn=nn+1
                   wxx(nn)=z_a(k)
                   wyy(nn)=value
               endif
            enddo
            nn=nn+1
            wxx(nn)=h_lfc
            wyy(nn)=0.
            call com_energy(nn,wxx,wyy,cin)
        endif
        i1=i1+1
 65     continue

c-----------------------------------------------------------------------
c* To compute the height of EL.

        do 70 k=i1,ii,1
           if( t_e(k).gt.t_a(k) )then
               detx=t_a(k)-t_a(k-1)-t_e(k)+t_e(k-1)
               if( abs(detx).lt.0.0000001 )then
                   ier=2
                   return
               endif
               dm=(t_e(k-1)-t_a(k-1))/detx
               rpa1=log(p_a(k-1))+(log(p_a(k))-log(p_a(k-1)))*dm
               pa1=exp(rpa1)
               te1=t_a(k-1)+(t_a(k)-t_a(k-1))*dm
               call ta_com(thetae0,pa1,t_a(k-1),0.001,50000,ta1,ier)
               if( ier.eq.2 )return
               detx=ta1-t_a(k-1)-te1+t_e(k-1)
               if( abs(detx).lt.0.0000001 )then
                   ier=2
                   return
               endif
               dm=(t_e(k-1)-t_a(k-1))/detx
               rpa2=log(p_a(k-1))+(rpa1-log(p_a(k-1)))*dm
               pa2=exp(rpa2)
               te2=t_a(k-1)+(ta1-t_a(k-1))*dm
               call ta_com(thetae0,pa2,t_a(k-1),0.001,50000,ta2,ier)
               if( ier.eq.2 )return
               p_el=pa2
               t_el=ta2
               tbar1=0.5*(t_e(k-1)+t_el+546.3)
               tbar2=0.5*(t_e(k)+t_el+546.3)
               za1=hyd(z_a(k-1),p_a(k-1),tbar1,p_el)
               za2=hyd(z_a(k),p_a(k),tbar2,p_el)
               h_el=0.5*(za1+za2)
               i2=k-1
               go to 75
           endif
 70     continue

c-----------------------------------------------------------------------
c* At top level, t_a > t_e  ----> top level assigned to EL

        p_el=p_a(ii)
        h_el=z_a(ii)
        t_el=t_a(ii)
        i2=ii-1
 75     continue

        if( i1.gt.i2 )then
          cape=0.
          ier=2
          return
        endif

c-----------------------------------------------------------------------
c* To compute convective available potential energy(CAPE)
c*    from h_lfc to h_el.

        nn=1
        wxx(nn)=h_lfc
        wyy(nn)=0.
        do k=i1,i2,1
           value=g*(t_a(k)-t_e(k))/(t_e(k)+273.15)
           if( value.ge.0.0 )then
               nn=nn+1
               wxx(nn)=z_a(k)
               wyy(nn)=value
           endif
        enddo
        nn=nn+1
        wxx(nn)=h_el
        wyy(nn)=0.
        call com_energy(nn,wxx,wyy,cape)

        return
        end

        subroutine ta_com(thetae,pa,ti,det,nx,ta,ier)
c***********************************************************************
c subroutine name  : ta_com                                            *
c                                                                      *
c description      : To find temperature(ta) at constant thetae and    *
c                    pressure(pa).                                     * 
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          thetae     real        theta E in unit of degree K.         *
c          pa         real        pressure in unit of hPa.             *
c          ti         real        the first guess temperature in unit  *
c                                 of degree C, the first temperature   *
c                                 must be greater than ta.             *
c          det        real        the interval temperature.            *
c          nx         integer     the number of iteration.             *
c  output: name       type        description                          *
c          ta         real        the tempertaure in unit of degree C. *
c          ier        integer     the error message,                   *
c                                 = 0, no error.                       *
c                                 = 2, cannot to get ta.               *
c                                                                      *
c called fun./sub. : none                                              *
c                                                                      *
c***********************************************************************

        ier=0
        if( pa.lt.100. )then
          ta=thetae*(pa/1000.)**(287./1004.) - 273.15
          return
        endif
        ta=ti
        do i=1,nx
          ta=ta-det
          theta=(ta+273.15)*(1000./pa)**(287./1004.)
          p_wa=6.112*exp(17.67*ta/(ta+243.5))
          wa=0.622*p_wa/(pa-p_wa)
          thetae_a=theta*exp(2675.*wa/(ta+273.15))
          if( thetae_a.le.thetae )return
        enddo
        ier=2
        return
        end

        subroutine com_energy(nn,x,y,energy)
c***********************************************************************
c subroutine name  : com_energy                                        *
c                                                                      *
c description      : To compute energy or integral.                    *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          nn         integer     the number of input data.            *
c          x(nn)      real array  the x-coordinate.                    *
c                                 = g*(T(parcel)-T(envir.))/T(envir.)  *
c          y(nn)      real array  the y-coordinate.                    *
c                                 = height in unit of meters           *
c  output: name       type        description                          *
c          energy     real        the energy or integral.              *
c                                                                      *
c called fun./sub. : rsum                                              *
c                                                                      *
c***********************************************************************
        dimension x(nn),y(nn)
        if( nn.le.2 )then
            energy=-999.
            return
        endif
        energy=0.
        do 10 k=1,nn-1
          if( k.eq.1 )then
            a1=rsum(x(k),x(k+1),x(k+2),y(k),y(k+1),y(k+2),x(k),x(k+1))
            energy=energy+a1
            go to 10
          endif
          if( k.eq.(nn-1) )then
            a2=rsum(x(k-1),x(k),x(k+1),y(k-1),y(k),y(k+1),x(k),x(k+1))
            energy=energy+a2
            go to 10
          endif
          a1=rsum(x(k),x(k+1),x(k+2),y(k),y(k+1),y(k+2),x(k),x(k+1))
          a2=rsum(x(k-1),x(k),x(k+1),y(k-1),y(k),y(k+1),x(k),x(k+1))
          energy=energy+0.5*(a1+a2)
 10     continue
        return
        end

        function rsum(x1,x2,x3,y1,y2,y3,p,q)
c***********************************************************************
c function name    : rsum                                              *
c                                                                      *
c description      : To compute integral from point P to Q by          *
c                    parabolic curved fitting.                         *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          x1         real        the x-coordinate at point 1.         *
c          x2         real        the x-coordinate at point 2.         *
c          x3         real        the x-coordinate at point 3.         *
c          y1         real        the y-coordinate at point 1.         *
c          y2         real        the y-coordinate at point 2.         *
c          y3         real        the y-coordinate at point 3.         *
c          p          real        the x-coordinate at point P.         *
c          q          real        the x-coordinate at point Q.         *
c  output: name       type        description                          *
c          rsum       real        the integral from P to Q.            *
c                                                                      *
c called fun./sub. : none                                              *
c                                                                      *
c***********************************************************************
        x12=x1-x2
        x13=x1-x3
        x23=x2-x3
        if( abs(x12).le.1.0E-10 )x12=1.0E-10
        if( abs(x13).le.1.0E-10 )x13=1.0E-10
        if( abs(x23).le.1.0E-10 )x23=1.0E-10
        a=y1/(x12*x13)
        b=-y2/(x12*x23)
        c=y3/(x13*x23)
        rsum=(a+b+c)*(q*q*q-p*p*p)/3.-
     +       0.5*( (x2+x3)*a+(x1+x3)*b+(x1+x2)*c )*(q*q-p*p)+
     +       ( x2*x3*a+x1*x3*b+x1*x2*c )*(q-p)
        return
        end

        function hyd(z,p,t,p0)
c***********************************************************************
c function name    : hyd                                               *
c                                                                      *
c description      : To interpolate height by hydrostatic              *
c                    approximation.                                    *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          z          real        the known height in unit of meters.  *
c          p          real        the known pressure in unit of hPa.   *
c          t          real        the known temperature in unit of     *
c                                 degree C.                            *
c          p0         real        the interpolated pressure in init of *
c                                 hPa.                                 *
c  output: name       type        description                          *
c          hyd        real        the height in unit of meters.        *
c                                                                      *
c called fun./sub. : none                                              *
c                                                                      *
c***********************************************************************
        hyd=z+287.*t*log(p/p0)/9.81
        return
        end 

        function fit(x1,y1,x2,y2,x)
c***********************************************************************
c function name    : fit                                               *
c                                                                      *
c description      : To interpolate y-value at point x by linear method*
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          x1         real        the x-coordinate at Point 1.         *
c          y1         real        the y-coordinate at Point 1.         *
c          x2         real        the x-coordinate at Point 2.         *
c          y2         real        the y-coordinate at Point 2.         *
c          x          real        the x-coordinate at Point x.         *
c  output: name       type        description                          *
c          fit        real        the y-value at point x.              *
c                                                                      *
c called fun./sub. : none                                              *
c                                                                      *
c***********************************************************************
        fit=y1+(x-x1)*(y2-y1)/(x2-x1)
        return
        end

        subroutine parcel_line(p_lcl,p_lfc,p_el,na,p_a,t_a)
c***********************************************************************
c subroutine name  : parcel_line                                       *
c                                                                      *
c description      : To plot air-parcel vertical motion line.          * 
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          p_lcl      real        the pressure of LCL in unit of hPa.  *
c          p_lfc      real        the pressure of LFC in unit of hPa.  *
c          p_el       real        the pressure of EL in unit of hPa.   * 
c          na         integer     the number of air parcel profile.    *
c          p_a(na)    real array  pressure of air parcel profile.      *
c          t_a(na)    real array  temperature of air parcel profile.   *
c                                                                      *
c called fun./sub. : setusv,gsplci,gstxci,set,curve --- NCAR GRAPHICS  *
c                                                                      *
c***********************************************************************

        dimension p_a(na),t_a(na),xx(6000),yy(6000)

        if( na.lt.2 )return

c-----------------------------------------------------------------------
c* To set black color for plotting parcel line of vertical motion.

        call setusv('LW',2000)
        call gsplci(1)
        call gstxci(1)

        nn=0
        ptop=p_lcl
        if( p_lfc.gt.0. )ptop=p_lfc
        if( ptop.gt.500. )then
            if( p_a(na).lt.500. )then
                ptop=500.
            else
                ptop=p_a(na)
            endif
        endif
        if( p_el.gt.0. )ptop=p_el
        do k=1,na
           if( ptop.gt.p_a(k) )then
               nn=k+1
               go to 10
           endif
        enddo
        nn=na
 10     continue
        if( nn.gt.na )nn=na
        if( nn.lt.2 )return

        detp=1.0413927
        rlog_1100=3.0413927
        detx=60.
        call set(0.2,0.8,0.1,0.9,0.,1.,0.,1.,1)
        do k=1,nn
           rlog_p=log10(p_a(k))
           t0=(rlog_p-2.)*80./detp-100.
           xx(k)=(t_a(k)-t0)/detx 
           yy(k)=(rlog_1100-rlog_p)/detp
        enddo
        call curve(xx,yy,nn)
        return
        end

       subroutine wt_title(station,time)
c***********************************************************************
c subroutine name  : wt_title                                          *
c                                                                      *
c description      : To write title: station number and date.          *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          station    C*5         the station number.                  *
c          time       C*8         the date = yymmddhh.                 *
c                                                                      *
c called fun./sub. : set,plchhq --- NCAR GRAPHICS                      *
c                                                                      *
c***********************************************************************

       character station*5,time*8,ca*15

c-----------------------------------------------------------------------
c* To write station number.

       ind=0
       do k=1,5
          if( station(k:k).ne.' ' )ind=1
       enddo
       if( ind.eq.0 )go to 10
       if( station(1:1).eq.char(0) )go to 10
       call set(0.2,0.8,0.9,1.0,0.,100.,0.,10.,1)
       ca(1:10)='STATION = '
       ca(11:15)=station(1:5)
       call plchhq(20.,2.5,ca,0.012,0.,0.)

c-----------------------------------------------------------------------
c* To write date.

 10    continue
       ind=0
       do k=1,8
          if( time(k:k).ne.' ' )ind=1
       enddo
       if( ind.eq.0 )return
       if( time(1:1).eq.char(0) )return
       ca(1:7)='TIME = '
       ca(8:15)=time(1:8)

       call plchhq(80.,2.5,ca,0.012,0.,0.)

       return
       end

       subroutine wt_ht(nn,pp,zz)
c***********************************************************************
c subroutine name  : wt_ht                                             *
c                                                                      *
c description      : To write height, station number, and date         *
c                    characters.                                       * 
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          nn         integer     the number of input data.            *
c          pp(nn)     real array  the pressure in unit of hPa.         *
c          zz(nn)     real array  the height in unit of meters.        *
c                                                                      *
c called fun./sub. : set,plchhq,pwritx --- NCAR GRAPHICS               *
c                                                                      *
c***********************************************************************
       dimension pp(nn),zz(nn),ip(16),xx(2),yy(2)
       character ca2*2,ca4*4,ca5*5,ca7*7
       data ip/1000,850,700,600,500,400,300,250,200,150,100,
     +         70,50,30,20,10/

       if( nn.le.0 )return

c-----------------------------------------------------------------------
c* To write height character.

       call set(0.8,1.,0.02,0.98,0.,100.,-0.1,1.1,1)
       call plchhq(40.,0.5,'HEIGHT (km)',0.015,90.,0.)

       call set(0.8,1.0,0.02,0.98,0.,100.,-0.1,1.1,1)
       x=15.
       detp=1.0413927
       if( nn.lt.1 )return

c-----------------------------------------------------------------------
c* To write height characters below the height of 100 hPa.

       do 20 k=1,11
          do 10 i=1,nn
             ipp=pp(i)
             if( ipp.eq.ip(k) )then
                 p=ip(k)
                 z=0.001*zz(i)
                 go to 15
             endif
 10       continue
          go to 20
 15       continue
          y=(3.0413927-log10(p))/detp
          if( z.lt.10.0 )then
              write(ca5,'(f5.3)')z
              if( ca5(1:1).eq.' ' )ca5(1:1)='0'
          else
              write(ca5,'(f5.2)')z
          endif
          call pwritx(x,y,ca5,5,12,0,0)
 20    continue

c-----------------------------------------------------------------------
c* To write height characters above the height of 100 hPa.

       id_atop=0
       x=15.
       ca7(1:1)='('
       ca7(7:7)=')'
       do 40 k=12,16
          do 30 i=1,nn
             ipp=pp(i)
             if( ipp.eq.ip(k) )then
                 p=ip(k)
                 z=0.001*zz(i)
                 go to 35
             endif
 30       continue
          go to 40
 35       continue
          id_atop=1
          if( z.lt.10.0 )then
              write(ca5,'(f5.3)')z
              if( ca5(1:1).eq.' ' )ca5(1:1)='0'
          else
              write(ca5,'(f5.2)')z
          endif
          ca7(2:6)=ca5(1:5)
          if( k.eq.12 )then
c              call pwritx(x,0.5218,ca7,7,10,0,0)
          endif
          if( k.eq.13 )then
c              call pwritx(x,0.6909,ca7,7,10,0,0)
          endif
          if( k.eq.14 )then
c              call pwritx(x,0.8109,ca7,7,10,0,0)
          endif
          if( k.eq.15 )then
c              call pwritx(x,0.9069,ca7,7,10,0,0)
          endif
          if( k.eq.16 )then
c              call pwritx(x,0.98,ca7,7,10,0,0)
          endif
 40    continue
       if( id_atop.eq.0 )return
       call set(0.,0.2,0.02,0.98,0.,100.,-0.1,1.1,1)
       x=90.
       ca4(1:1)='('
       ca4(4:4)=')'
       write(ca2,'(i2)')70
       ca4(2:3)=ca2(1:2)
c       call pwritx(x,0.5218,ca4,4,10,0,0)
       write(ca2,'(i2)')50
       ca4(2:3)=ca2(1:2)
c       call pwritx(x,0.6909,ca4,4,10,0,0)
       write(ca2,'(i2)')30
       ca4(2:3)=ca2(1:2)
c       call pwritx(x,0.8109,ca4,4,10,0,0)
       write(ca2,'(i2)')20
       ca4(2:3)=ca2(1:2)
c       call pwritx(x,0.9069,ca4,4,10,0,0)
       write(ca2,'(i2)')10
       ca4(2:3)=ca2(1:2)
c       call pwritx(x,0.98,ca4,4,10,0,0)

       call set(0.2,0.8,0.1,0.9,0.,1.,0.,1.,1)
       call dashdb(ipat)
       xx(1)=0.0
       xx(2)=0.6667
       yy(1)=0.9069
       yy(2)=0.9069
c       call curved(xx,yy,2)

       return
       end

       subroutine pl_tt(nn,pp,tt)
c***********************************************************************
c subroutine name  : pl_tt                                             *
c                                                                      *
c description      : To plot temperature or dew-point temperature line * 
c                    above the height of 70 hPa.                       *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          nn         integer     the number of input data.            *
c          pp(nn)     real array  the pressure in unit of hPa.         *
c          tt(nn)     real array  the temperature in unit of degree C. *
c                                                                      *
c called fun./sub. : set,curve --- NCAR GRAPHICS                       *
c                                                                      *
c***********************************************************************
       dimension pp(nn),tt(nn),pcor(5),ycor(5)
       dimension xx1(6000),yy1(6000),xx(6000),yy(6000)
       data ycor/0.541843,0.710935,0.830908,0.906942,1.000000/
       data pcor/1.845098,1.698970,1.477121,1.301030,1.000000/

       if( nn.le.0 )return
       call set(0.2,0.8,0.1,0.9,0.,1.,0.,1.,1)

       do k=1,nn
          if( pp(k).ge.50. )then
              y=log10( pp(k) )
              yy1(k)=ycor(1)+(y-pcor(1))*(ycor(2)-ycor(1))/
     +                       (pcor(2)-pcor(1))
              t0=-20.0-yy1(k)*80.0
              xx1(k)=(tt(k)-t0)/60.
          endif
          if( (pp(k).lt.50.).and.(pp(k).ge.30.) )then
              y=log10( pp(k) )
              yy1(k)=ycor(2)+(y-pcor(2))*(ycor(3)-ycor(2))/
     +                       (pcor(3)-pcor(2))
              t0=-20.0-yy1(k)*80.0
              xx1(k)=(tt(k)-t0)/60.
          endif
          if( (pp(k).lt.30.).and.(pp(k).ge.20.) )then
              y=log10( pp(k) )
              yy1(k)=ycor(3)+(y-pcor(3))*(ycor(4)-ycor(3))/
     +                       (pcor(4)-pcor(3))
              t0=-20.0-yy1(k)*80.0
              xx1(k)=(tt(k)-t0)/60.
          endif
          if( pp(k).lt.20. )then
              y=log10( pp(k) )
              yy1(k)=ycor(4)+(y-pcor(4))*(ycor(5)-ycor(4))/
     +                       (pcor(5)-pcor(4))
              t0=-20.0-yy1(k)*80.0
              xx1(k)=(tt(k)-t0)/60.
          endif
       enddo

       k1=0
       do k=1,nn
          if( (xx1(k).ge.0.).and.(xx1(k).le.0.6667) )then
             k1=k
             go to 10
          endif
       enddo
       return
 10    continue
       k2=0
       do k=nn,1,-1
          if( (xx1(k).ge.0.).and.(xx1(k).le.0.6667) )then
             k2=k
             go to 20
          endif
       enddo
       return
 20    continue
       ii=0
       if( k1.gt.1 )then
           ii=1
           xx(ii)=0.0
           yy(ii)=yy1(k1-1)-xx1(k1-1)*(yy1(k1)-yy1(k1-1))/
     +                                (xx1(k1)-xx1(k1-1))
       endif
       do k=k1,k2,1
          ii=ii+1
          xx(ii)=xx1(k)
          yy(ii)=yy1(k)
       enddo
       if( k2.lt.nn )then
          ii=ii+1
          xx(ii)=0.6667
          yy(ii)=yy1(k2)+(xx1(k2+1)-0.6667)*(yy1(k2+1)-yy1(k2))/
     +                                      (xx1(k2+1)-xx1(k2))
       endif
       if( ii.gt.1 )then
           call curve(xx,yy,ii)
       endif

       return
       end

       subroutine pl_t_td(nn,pp,tt)
c***********************************************************************
c subroutine name  : pl_t_td                                           *
c                                                                      *
c description      : To plot temperature or dew-point temperature line * 
c                    below the height of 100 hPa.                      *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          nn         integer     the number of input data.            *
c          pp(nn)     real array  the pressure in unit of hPa.         *
c          tt(nn)     real array  the temperature or dew-point         *
c                                 temperature in unit of degree C.     *
c                                                                      *
c called fun./sub. : set,curve --- NCAR GRAPHICS                       *
c                                                                      *
c***********************************************************************
       dimension pp(nn),tt(nn)
       dimension xx(6000),yy(6000)

       if( nn.le.0 )return
       detp=1.0413927
       rlog_1100=3.0413927
       detx=60.
       call set(0.2,0.8,0.1,0.9,0.,1.,0.,1.,1)
       do 10 k=1,nn
          rlog_p=log10(pp(k))
          t0=(rlog_p-2.)*80./detp-100.
          yy(k)=(rlog_1100-rlog_p)/detp
          xx(k)=(tt(k)-t0)/detx 
 10    continue

c-----------------------------------------------------------------------
c* To plot temperature or dew-point temperature line.

       call curve(xx,yy,nn)

       return
       end

       subroutine pl_wind(nn,pp,wd,ws,wsps)
c***********************************************************************
c subroutine name  : pl_wind                                           *
c                                                                      *
c description      : To plot wind flag.                                *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          nn         integer     the number of input data.            *
c          pp(nn)     real array  the pressure in unit of hPa.         *
c          wd(nn)     real array  the wind direction.                  *
c          ws(nn)     real array  the wind speed in unit of m/s.       *
c                                                                      *
c called fun./sub. : flag                                              *
c                    set,plchhq --- NCAR GRAPHICS                      *
c                                                                      *
c***********************************************************************
       dimension pp(nn),wd(nn),ws(nn),xx(2),yy(2)

       if( nn.le.0. )return
       detp=1.0413927
       rlog_1100=3.0413927
       call set(0.8,1.,0.02,0.98,0.,100.,-0.1,1.1,1)
       x=65.
       xx(1)=x
       xx(2)=x
       yy(1)=0.0413927/detp
       yy(2)=1.0
       call curve(xx,yy,2)
       plsps=abs(log10(1000.)-log10(1000.-wsps))
       pplt=99999999.
       do 10 k=1,nn
          rlog_p=log10(pp(k))
          if(abs(rlog_p-pplt).ge.plsps)then
             y=(rlog_1100-rlog_p)/detp
             call plchhq(x,y,':KRL:E',0.005,0.,0.)
c            call flag(x,y,ws(k),wd(k),1,-25)
             call flag(x,y,ws(k),wd(k),1,-25.,10.)
             pplt=rlog_p
          endif
 10    continue
       call set(0.8,1.,0.9,1.,0.,100.,0.,10.,1)
       call plchhq(65.,4.,'WIND',0.01,0.,0.)
       return
       end

       subroutine base_map
c***********************************************************************
c subroutine name  : base_map                                          *
c                                                                      *
c description      : To plot base map for logP, temperature, and       *
c                    potential temperature line.                       *
c                                                                      *
c called fun./sub. : set,gsfaci,gfa,gsplci,gstxci,curve,setusv,        *
c                    dashdb,curved,plchhq,pwritx --- NCAR GRAPHICS     *
c                                                                      *
c***********************************************************************
       character ca3*3,ca4*4,ca2*2,ca1*1
       dimension xx(5),yy(5)
       dimension rlog_p(12),ip(12)
       dimension xb(5),yb(5),yp(10)

       dimension ax1(4),ay1(4),ax2(4),ay2(4),ax3(4),ay3(4)
       dimension ax4(4),ay4(4),ax5(4),ay5(4),ax6(4),ay6(4)
       dimension ax7(4),ay7(4),ax8(4),ay8(4),ax9(4),ay9(4)
       dimension ax10(4),ay10(4),ax11(4),ay11(4),ax12(4),ay12(4)
       dimension ax13(4),ay13(4),ax14(4),ay14(4)

       dimension x1(4),y1(4),x2(7),y2(7),x3(11),y3(11)
       dimension x4(14),y4(14),x5(18),y5(18),x6(21),y6(21)
       dimension x7(21),y7(21),x8(21),y8(21),x9(21),y9(21)
       dimension x10(20),y10(20),x11(18),y11(18)
       dimension x12(16),y12(16),x13(15),y13(15)
       dimension x14(7),y14(7),x15(6),y15(6)
       dimension x16(5),y16(5),x17(4),y17(4)
       dimension x18(4),y18(4),x19(2),y19(2),nxy(19)

       data xb/0.,1.,1.,0.,0./     !boundary line: x-coordinate.
       data yb/0.,0.,1.,1.,0./     !boundary line: y-coordinate.

       data ax1/0.9167,1.0000,1.0000,0.9167/  !area 1
       data ay1/0.0000,0.0000,0.0625,0.0000/  
       data ax2/0.7500,0.8333,1.0000,1.0000/  !area 2 
       data ay2/0.0000,0.0000,0.1250,0.1875/
       data ax3/0.5833,0.6667,1.0000,1.0000/  !area 3
       data ay3/0.0000,0.0000,0.2500,0.3125/
       data ax4/0.4167,0.5000,1.0000,1.0000/  !area 4
       data ay4/0.0000,0.0000,0.3750,0.4375/
       data ax5/0.2500,0.3333,1.0000,1.0000/  !area 5
       data ay5/0.0000,0.0000,0.5000,0.5625/
       data ax6/0.0833,0.1667,1.0000,1.0000/  !area 6
       data ay6/0.0000,0.0000,0.6250,0.6875/
       data ax7/0.0000,0.0000,0.9479,0.8646/  !area 7
       data ay7/0.0625,0.0000,0.7109,0.7109/
       data ax8/0.0000,0.0000,0.7812,0.6979/  !area 8
       data ay8/0.1875,0.1250,0.7109,0.7109/
       data ax9/0.0000,0.0000,0.6667,0.6667/  !area 9
       data ay9/0.3125,0.2500,0.7500,0.8125/
       data ax10/0.0000,0.0000,0.6667,0.6667/ !area 10
       data ay10/0.4375,0.3750,0.8750,0.9375/
       data ax11/0.0000,0.0000,0.6667,0.5833/ !area 11
       data ay11/0.5625,0.5000,1.0000,1.0000/
       data ax12/0.0000,0.0000,0.5000,0.4167/ !area 12
       data ay12/0.6875,0.6250,1.0000,1.0000/
       data ax13/0.0000,0.0000,0.3333,0.2500/ !area 13
       data ay13/0.8125,0.7500,1.0000,1.0000/
       data ax14/0.0000,0.0000,0.1667,0.0833/ !area 14
       data ay14/0.9375,0.8750,1.0000,1.0000/

       data yp/0.03975,0.10752,0.18849,0.25278,0.32881, !for horzental line
     *         0.42187,0.54184,0.61788,0.71094,0.83091/

       data nxy/4,7,11,14,18,21,21,21,21,20,18,16,15,7,6,5,4,4,2/
       data x1/0.0000,0.0500,0.1000,0.1163/   !for -20 C
       data y1/0.0746,0.0417,0.0101,0.0000/   
       data x2/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.2876/   !for -10 C
       data y2/0.1838,0.1486,0.1149,0.0826,0.0516,0.0217,0.0000/
       data x3/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.4589/          !for 0 C 
       data y3/0.2927,0.2548,0.2188,0.1845,0.1516,0.1200,0.0897,0.0604,
     *         0.0321,0.0048,0.0000/
       data x4/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.5000,0.5500,0.6000,0.6301/          !for 10 C
       data y4/0.4018,0.3610,0.3223,0.2856,0.2506,0.2172,0.1851,0.1542,
     *         0.1245,0.0959,0.0682,0.0413,0.0153,0.0000/
       data x5/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.5000,0.5500,0.6000,0.6500,0.7000,0.7500,
     *         0.8000,0.8014/                                      !for 20 C
       data y5/0.5120,0.4676,0.4258,0.3864,0.3490,0.3135,0.2795,0.2469,
     *         0.2157,0.1856,0.1566,0.1285,0.1014,0.0751,0.0496,0.0248,
     *         0.0007,0.0000/
       data x6/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.5000,0.5500,0.6000,0.6500,0.7000,0.7500,
     *         0.8000,0.8500,0.9000,0.9500,0.9727/                 !for 30 C
       data y6/0.6241,0.5753,0.5299,0.4874,0.4473,0.4093,0.3732,0.3388,
     *         0.3058,0.2742,0.2437,0.2144,0.1861,0.1587,0.1321,0.1064,
     *         0.0813,0.0570,0.0333,0.0103,0.0000/
       data x7/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.5000,0.5500,0.6000,0.6500,0.7000,0.7500,
     *         0.8000,0.8500,0.9000,0.9500,1.0000/                 !for 40 C
       data y7/0.7395,0.6852,0.6353,0.5890,0.5458,0.5051,0.4666,0.4300,
     *         0.3952,0.3618,0.3298,0.2991,0.2695,0.2409,0.2132,0.1865,
     *         0.1605,0.1353,0.1108,0.0870,0.0637/
       data x8/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.5000,0.5500,0.6000,0.6500,0.7000,0.7500,
     *         0.8000,0.8500,0.9000,0.9500,1.0000/                 !for 50 C
       data y8/0.8596,0.7983,0.7429,0.6922,0.6451,0.6012,0.5599,0.5210,
     *         0.4840,0.4488,0.4151,0.3829,0.3518,0.3220,0.2932,0.2653,
     *         0.2384,0.2122,0.1869,0.1622,0.1382/
       data x9/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.5000,0.5500,0.6000,0.6500,0.7000,0.7500,
     *         0.8000,0.8500,0.9000,0.9500,1.0000/                 !for 60 C
       data y9/0.9881,0.9167,0.8538,0.7974,0.7459,0.6982,0.6538,0.6121,
     *         0.5727,0.5354,0.4998,0.4659,0.4333,0.4021,0.3720,0.3430,
     *         0.3150,0.2879,0.2616,0.2361,0.2113/
       data x10/0.0785,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,0.4000,
     *          0.4500,0.5000,0.5500,0.6000,0.6500,0.7000,0.7500,0.8000,
     *          0.8500,0.9000,0.9500,1.0000/                       !for 70 C
       data y10/1.0000,0.9704,0.9062,0.8489,0.7967,0.7485,0.7036,0.6615,
     *          0.6218,0.5841,0.5483,0.5142,0.4814,0.4500,0.4198,0.3906,
     *          0.3624,0.3352,0.3088,0.2831/
       data x11/0.1649,0.2000,0.2500,0.3000,0.3500,0.4000,0.4500,0.5000,
     *          0.5500,0.6000,0.6500,0.7000,0.7500,0.8000,0.8500,0.9000,
     *          0.9500,1.0000/                                     !for 80 C
       data y11/1.0000,0.9557,0.8974,0.8446,0.7960,0.7508,0.7084,0.6684,
     *          0.6305,0.5945,0.5601,0.5272,0.4956,0.4653,0.4360,0.4077,
     *          0.3804,0.3538/
       data x12/0.2513,0.3000,0.3500,0.4000,0.4500,0.5000,0.5500,0.6000,
     *          0.6500,0.7000,0.7500,0.8000,0.8500,0.9000,0.9500,1.0000/
       data y12/1.0000,0.9434,0.8899,0.8409,0.7954,0.7528,0.7126,0.6746,
     *          0.6384,0.6039,0.5708,0.5392,0.5087,0.4793,0.4510,0.4235/
       data x13/0.3377,0.3500,0.4000,0.4500,0.5000,0.5500,0.6000,0.6500,
     *          0.7000,0.7500,0.8000,0.8500,0.9000,0.9500,1.0000/
       data y13/1.0000,0.9863,0.9328,0.8834,0.8377,0.7949,0.7546,0.7164,
     *          0.6802,0.6455,0.6125,0.5807,0.5501,0.5207,0.4923/
       data x14/0.4241,0.4500,0.5000,0.5500,0.6000,0.6500,0.6667/ !for 110C
       data y14/1.0000,0.9731,0.9236,0.8777,0.8348,0.7944,0.7816/
       data x15/0.7630,0.8000,0.8500,0.9000,0.9500,1.0000/   !for 110 C
       data y15/0.7109,0.6853,0.6521,0.6203,0.5897,0.5602/
       data x16/0.5105,0.5500,0.6000,0.6500,0.6667/          !for 120 C
       data y16/1.0000,0.9616,0.9156,0.8726,0.8591/
       data x17/0.8683,0.9000,0.9500,1.0000/                 !for 120 C
       data y17/0.7109,0.6899,0.6581,0.6275/
       data x18/0.5969,0.6000,0.6500,0.6667/                 !for 130 C
       data y18/1.0000,0.9971,0.9515,0.9371/
       data x19/0.9736,1.0000/                               !for 130 C
       data y19/0.7109,0.6941/

       data rlog_p/2.0000000,2.1760913,2.3010300,2.3979400,
     *             2.4771213,2.6020600,2.6989700,2.7781513,
     *             2.8450980,2.9294189,3.0000000,3.0413927/
       data ip/100,150,200,250,300,400,500,600,700,850,1000,1100/

c-----------------------------------------------------------------------
c* To set domain area.

       call set(0.2,0.8,0.1,0.9,0.,1.,0.,1.,1)

c-----------------------------------------------------------------------
c* To set yellow color for filling area.

       call gsfaci(4)

c-----------------------------------------------------------------------
c* To fill yellow area.

       call gfa(4,ax1,ay1)
       call gfa(4,ax2,ay2)
       call gfa(4,ax3,ay3)
       call gfa(4,ax4,ay4)
       call gfa(4,ax5,ay5)
       call gfa(4,ax6,ay6)
       call gfa(4,ax7,ay7)
       call gfa(4,ax8,ay8)
       call gfa(4,ax9,ay9)
       call gfa(4,ax10,ay10)
       call gfa(4,ax11,ay11)
       call gfa(4,ax12,ay12)
       call gfa(4,ax13,ay13)
       call gfa(4,ax14,ay14)
          
c-----------------------------------------------------------------------
c* To set blue color for plotting line.

       call gsplci(5)
       call gstxci(5)

c-----------------------------------------------------------------------
c* To plot tilted line(potential temperature).

       call curve(x1,y1,nxy(1))
       call curve(x2,y2,nxy(2))
       call curve(x3,y3,nxy(3))
       call curve(x4,y4,nxy(4))
       call curve(x5,y5,nxy(5))
       call curve(x6,y6,nxy(6))
       call curve(x7,y7,nxy(7))
       call curve(x8,y8,nxy(8))
       call curve(x9,y9,nxy(9))
       call curve(x10,y10,nxy(10))
       call curve(x11,y11,nxy(11))
       call curve(x12,y12,nxy(12))
       call curve(x13,y13,nxy(13))
       call curve(x14,y14,nxy(14))
       call curve(x15,y15,nxy(15))
       call curve(x16,y16,nxy(16))
       call curve(x17,y17,nxy(17))
       call curve(x18,y18,nxy(18))
       call curve(x19,y19,nxy(19))

c-----------------------------------------------------------------------
c* To set black color for plotting line.

       call gsplci(1)
       call gstxci(1)

c-----------------------------------------------------------------------
c* To plot boundary line.

       call curve(xb,yb,5)

c-----------------------------------------------------------------------
c* To plot horizental line(log P coordinate).

       xx(1)=0.
       xx(2)=1.
       do k=1,9
          yy(1)=yp(k)
          yy(2)=yp(k)
          call curve(xx,yy,2)
       enddo
       xx(2)=0.6667
       yy(1)=yp(10)
       yy(2)=yp(10)
       call curve(xx,yy,2)
       ipat=ishift(32382,1)

c-----------------------------------------------------------------------
c* To plot vertical line.

       xx(1)=xx(2)
       yy(1)=yp(9)
       yy(2)=1.
       call curve(xx,yy,2)

c-----------------------------------------------------------------------
c* To write mixing ratio characters.

       call plchhq(0.9236,0.03,'30',0.007,0.,0.)
       call plchhq(0.8705,0.03,'25',0.007,0.,0.)
       call plchhq(0.8072,0.03,'20',0.007,0.,0.)
       call plchhq(0.7281,0.03,'15',0.007,0.,0.)
       call plchhq(0.6213,0.03,'10',0.007,0.,0.)
       call plchhq(0.5316,0.03,'7',0.007,0.,0.)
       call plchhq(0.4503,0.03,'5',0.007,0.,0.)
       call plchhq(0.3980,0.03,'4',0.007,0.,0.)
       call plchhq(0.3327,0.03,'3',0.007,0.,0.)
       call plchhq(0.2440,0.03,'2',0.007,0.,0.)
       call plchhq(0.1834,0.03,'1.5',0.007,0.,0.)
       call plchhq(0.1012,0.03,'1',0.007,0.,0.)
       call plchhq(0.0317,0.03,'0.7',0.007,0.,0.)
       call plchhq(0.97,0.03,'g/kg',0.006,-1.,0.)

c-----------------------------------------------------------------------
c* To write pressure character.

       detp=rlog_p(12)-rlog_p(1)
       call set(0.,0.2,0.02,0.98,0.,100.,-0.1,1.1,1)
       call plchhq(60.,0.5,'PRESSURE (mb)',0.015,90.,0.)
       x=85.
       do 30 k=1,10
       y=(rlog_p(12)-rlog_p(k))/detp
       write(ca3,'(i3)')ip(k)
       call pwritx(x,y,ca3,3,15,0,0)
 30    continue
       write(ca4,'(i4)')ip(11)
       y=(rlog_p(12)-rlog_p(11))/detp
       call pwritx(x,y,ca4,4,15,0,0)

c-----------------------------------------------------------------------
c* To write temperature character.

       call set(0.15,0.85,0.,0.1,-5.,65.,0.,10.,1)
       call plchhq(30.,4.5,'SKEW T, log p DIAGRAM',0.012,0.,0.)
       write(ca3,'(i3)')-20
       call pwritx(0.,9.,ca3,3,10,0,0)
       write(ca3,'(i3)')-15
       call pwritx(5.,9.,ca3,3,10,0,0)
       write(ca3,'(i3)')-10
       call pwritx(10.,9.,ca3,3,10,0,0)
       write(ca2,'(i2)')-5
       call pwritx(15.,9.,ca2,2,10,0,0)
       write(ca1,'(i1)')0
       call pwritx(20.,9.,ca1,1,10,0,0)
       write(ca1,'(i1)')5
       call pwritx(25.,9.,ca1,1,10,0,0)
       write(ca2,'(i2)')10
       call pwritx(30.,9.,ca2,2,10,0,0)
       write(ca2,'(i2)')15
       call pwritx(35.,9.,ca2,2,10,0,0)
       write(ca2,'(i2)')20
       call pwritx(40.,9.,ca2,2,10,0,0)
       write(ca2,'(i2)')25
       call pwritx(45.,9.,ca2,2,10,0,0)
       write(ca2,'(i2)')30
       call pwritx(50.,9.,ca2,2,10,0,0)
       write(ca2,'(i2)')35
       call pwritx(55.,9.,ca2,2,10,0,0)
       write(ca2,'(i2)')40
       call pwritx(60.,9.,ca2,2,10,0,0)
       call plchhq(62.5,9.,'(:S:o:N:C)',0.008,-1.,0.)
       return
       end

       subroutine base_map1
c***********************************************************************
c subroutine name  : base_map1                                         *
c                                                                      *
c description      : To plot mixing ratio, and pseudo-adiabatic line.  *
c                                                                      *
c called fun./sub. : set,gsplci,gstxci,curve,setusv,                   *
c                    dashdb,curved --- NCAR GRAPHICS                   *
c                                                                      *
c***********************************************************************

       character ca1*1,ca2*2,ca3*3
       dimension xx(2),yy(2)

       dimension x1(4),y1(4),x2(7),y2(7),x3(11),y3(11)
       dimension x4(14),y4(14),x5(18),y5(18),x6(21),y6(21)
       dimension x7(21),y7(21),x8(21),y8(21),x9(21),y9(21)
       dimension x10(20),y10(20),x11(18),y11(18)
       dimension x12(16),y12(16),x13(15),y13(15)
       dimension x14(7),y14(7),x15(6),y15(6)
       dimension x16(5),y16(5),x17(4),y17(4)
       dimension x18(4),y18(4),x19(2),y19(2),nxy(19)

       dimension qx1(5),qy1(5),qx2(7),qy2(7),qx3(9),qy3(9)
       dimension qx4(11),qy4(11),qx5(13),qy5(13),qx6(15),qy6(15)
       dimension qx7(15),qy7(15),qx8(15),qy8(15),qx9(15),qy9(15)
       dimension qx10(15),qy10(15),qx11(15),qy11(15)
       dimension qx12(15),qy12(15),qx13(15),qy13(15),nq(13)

       dimension rx1(15),ry1(15),rx2(15),ry2(15),rx3(15),ry3(15)
       dimension rx4(15),ry4(15),rx5(15),ry5(15),rx6(15),ry6(15)
       dimension rx7(13),ry7(13),rx8(11),ry8(11),rx9(9),ry9(9)
       dimension rx10(7),ry10(7),rx11(5),ry11(5),rx12(2),ry12(2)
       dimension nr(12)

       data nxy/4,7,11,14,18,21,21,21,21,20,18,16,15,7,6,5,4,4,2/
       data x1/0.0000,0.0500,0.1000,0.1163/   !for -20 C
       data y1/0.0746,0.0417,0.0101,0.0000/   
       data x2/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.2876/   !for -10 C
       data y2/0.1838,0.1486,0.1149,0.0826,0.0516,0.0217,0.0000/
       data x3/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.4589/          !for 0 C 
       data y3/0.2927,0.2548,0.2188,0.1845,0.1516,0.1200,0.0897,0.0604,
     *         0.0321,0.0048,0.0000/
       data x4/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.5000,0.5500,0.6000,0.6301/          !for 10 C
       data y4/0.4018,0.3610,0.3223,0.2856,0.2506,0.2172,0.1851,0.1542,
     *         0.1245,0.0959,0.0682,0.0413,0.0153,0.0000/
       data x5/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.5000,0.5500,0.6000,0.6500,0.7000,0.7500,
     *         0.8000,0.8014/                                      !for 20 C
       data y5/0.5120,0.4676,0.4258,0.3864,0.3490,0.3135,0.2795,0.2469,
     *         0.2157,0.1856,0.1566,0.1285,0.1014,0.0751,0.0496,0.0248,
     *         0.0007,0.0000/
       data x6/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.5000,0.5500,0.6000,0.6500,0.7000,0.7500,
     *         0.8000,0.8500,0.9000,0.9500,0.9727/                 !for 30 C
       data y6/0.6241,0.5753,0.5299,0.4874,0.4473,0.4093,0.3732,0.3388,
     *         0.3058,0.2742,0.2437,0.2144,0.1861,0.1587,0.1321,0.1064,
     *         0.0813,0.0570,0.0333,0.0103,0.0000/
       data x7/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.5000,0.5500,0.6000,0.6500,0.7000,0.7500,
     *         0.8000,0.8500,0.9000,0.9500,1.0000/                 !for 40 C
       data y7/0.7395,0.6852,0.6353,0.5890,0.5458,0.5051,0.4666,0.4300,
     *         0.3952,0.3618,0.3298,0.2991,0.2695,0.2409,0.2132,0.1865,
     *         0.1605,0.1353,0.1108,0.0870,0.0637/
       data x8/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.5000,0.5500,0.6000,0.6500,0.7000,0.7500,
     *         0.8000,0.8500,0.9000,0.9500,1.0000/                 !for 50 C
       data y8/0.8596,0.7983,0.7429,0.6922,0.6451,0.6012,0.5599,0.5210,
     *         0.4840,0.4488,0.4151,0.3829,0.3518,0.3220,0.2932,0.2653,
     *         0.2384,0.2122,0.1869,0.1622,0.1382/
       data x9/0.0000,0.0500,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,
     *         0.4000,0.4500,0.5000,0.5500,0.6000,0.6500,0.7000,0.7500,
     *         0.8000,0.8500,0.9000,0.9500,1.0000/                 !for 60 C
       data y9/0.9881,0.9167,0.8538,0.7974,0.7459,0.6982,0.6538,0.6121,
     *         0.5727,0.5354,0.4998,0.4659,0.4333,0.4021,0.3720,0.3430,
     *         0.3150,0.2879,0.2616,0.2361,0.2113/
       data x10/0.0785,0.1000,0.1500,0.2000,0.2500,0.3000,0.3500,0.4000,
     *          0.4500,0.5000,0.5500,0.6000,0.6500,0.7000,0.7500,0.8000,
     *          0.8500,0.9000,0.9500,1.0000/                       !for 70 C
       data y10/1.0000,0.9704,0.9062,0.8489,0.7967,0.7485,0.7036,0.6615,
     *          0.6218,0.5841,0.5483,0.5142,0.4814,0.4500,0.4198,0.3906,
     *          0.3624,0.3352,0.3088,0.2831/
       data x11/0.1649,0.2000,0.2500,0.3000,0.3500,0.4000,0.4500,0.5000,
     *          0.5500,0.6000,0.6500,0.7000,0.7500,0.8000,0.8500,0.9000,
     *          0.9500,1.0000/                                     !for 80 C
       data y11/1.0000,0.9557,0.8974,0.8446,0.7960,0.7508,0.7084,0.6684,
     *          0.6305,0.5945,0.5601,0.5272,0.4956,0.4653,0.4360,0.4077,
     *          0.3804,0.3538/
       data x12/0.2513,0.3000,0.3500,0.4000,0.4500,0.5000,0.5500,0.6000,
     *          0.6500,0.7000,0.7500,0.8000,0.8500,0.9000,0.9500,1.0000/
       data y12/1.0000,0.9434,0.8899,0.8409,0.7954,0.7528,0.7126,0.6746,
     *          0.6384,0.6039,0.5708,0.5392,0.5087,0.4793,0.4510,0.4235/
       data x13/0.3377,0.3500,0.4000,0.4500,0.5000,0.5500,0.6000,0.6500,
     *          0.7000,0.7500,0.8000,0.8500,0.9000,0.9500,1.0000/
       data y13/1.0000,0.9863,0.9328,0.8834,0.8377,0.7949,0.7546,0.7164,
     *          0.6802,0.6455,0.6125,0.5807,0.5501,0.5207,0.4923/
       data x14/0.4241,0.4500,0.5000,0.5500,0.6000,0.6500,0.6667/ !for 110C
       data y14/1.0000,0.9731,0.9236,0.8777,0.8348,0.7944,0.7816/
       data x15/0.7630,0.8000,0.8500,0.9000,0.9500,1.0000/   !for 110 C
       data y15/0.7109,0.6853,0.6521,0.6203,0.5897,0.5602/
       data x16/0.5105,0.5500,0.6000,0.6500,0.6667/          !for 120 C
       data y16/1.0000,0.9616,0.9156,0.8726,0.8591/
       data x17/0.8683,0.9000,0.9500,1.0000/                 !for 120 C
       data y17/0.7109,0.6899,0.6581,0.6275/
       data x18/0.5969,0.6000,0.6500,0.6667/                 !for 130 C
       data y18/1.0000,0.9971,0.9515,0.9371/
       data x19/0.9736,1.0000/                               !for 130 C
       data y19/0.7109,0.6941/

       data nq/5,7,9,11,13,15,15,15,15,15,15,15,15/
       data qx1/0.9236,0.9514,0.9666,0.9829,1.0000/          !for 30 g/kg
       data qy1/0.0397,0.0837,0.1075,0.1328,0.1590/
       data qx2/0.8705,0.8990,0.9146,0.9313,0.9685,0.9894,1.0000/ !for 25 g/kg
       data qy2/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2349/
       data qx3/0.8072,0.8365,0.8526,0.8697,0.9079,0.9294,0.9527, !for 20 g/kg
     *          0.9784,1.0000/
       data qy3/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *          0.2891,0.3194/
       data qx4/0.7281,0.7585,0.7751,0.7928,0.8322,0.8544,0.8784, !for 15 g/kg
     *          0.9049,0.9340,0.9666,1.0000/
       data qy4/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *          0.2891,0.3288,0.3728,0.4174/
       data qx5/0.6213,0.6530,0.6703,0.6888,0.7299,0.7529,0.7780, !for 10 g/kg
     *          0.8054,0.8357,0.8694,0.9075,0.9512,1.0000/  
       data qy5/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *          0.2891,0.3288,0.3728,0.4219,0.4776,0.5392/
       data qx6/0.5316,0.5643,0.5823,0.6014,0.6438,0.6676,0.6934, !for 7 g/kg
     *   0.7217,0.7528,0.7876,0.8268,0.8716,0.9239,0.9865,1.0000/
       data qy6/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *   0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.6341/
       data qx7/0.4503,0.4840,0.5025,0.5221,0.5658,0.5902,0.6167, !for 5 g/kg
     *   0.6457,0.6777,0.7133,0.7535,0.7994,0.8529,0.9168,0.9961/
       data qy7/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *   0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.7109/
       data qx8/0.3980,0.4324,0.4512,0.4712,0.5156,0.5404,0.5674, !for 4 g/kg
     *   0.5969,0.6294,0.6656,0.7063,0.7529,0.8072,0.8720,0.9523/
       data qy8/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *   0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.7109/
       data qx9/0.3327,0.3678,0.3870,0.4074,0.4528,0.4781,0.5056, !for 3 g/kg
     *   0.5357,0.5688,0.6057,0.6472,0.6946,0.7498,0.8157,0.8973/
       data qy9/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *   0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.7109/
       data qx10/0.2440,0.2802,0.2999,0.3209,0.3675,0.3935,0.4218, !for 2 g/kg
     *    0.4526,0.4866,0.5244,0.5669,0.6154,0.6719,0.7392,0.8225/
       data qy10/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *    0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.7109/
       data qx11/0.1834,0.2203,0.2404,0.2618,0.3092,0.3357,0.3644, !for 1.5 g/kg
     *    0.3958,0.4303,0.4687,0.5119,0.5612,0.6185,0.6868,0.7713/
       data qy11/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *    0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.7109/
       data qx12/0.1012,0.1389,0.1595,0.1815,0.2300,0.2571,0.2864, !for 1 g/kg
     *    0.3185,0.3538,0.3930,0.4371,0.4874,0.5458,0.6155,0.7015/
       data qy12/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *    0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.7109/
       data qx13/0.0317,0.0702,0.0912,0.1135,0.1630,0.1906,0.2205, !for 0.7 g/kg
     *    0.2532,0.2891,0.3290,0.3738,0.4249,0.4843,0.5550,0.6423/
       data qy13/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *    0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.7109/

       data nr/15,15,15,15,15,15,13,11,9,7,5,2/
       data rx1/0.9697,0.9758,0.9782,0.9807,0.9861,0.9889,0.9917, !for 35 C
     *   0.9944,0.9968,0.9987,0.9995,0.9982,0.9929,0.9793,0.9475/
       data ry1/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *   0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.7109/
       data rx2/0.8863,0.8891,0.8896,0.8900,0.8899,0.8893,0.8880, !for 30 C
     *   0.8858,0.8821,0.8762,0.8667,0.8515,0.8263,0.7835,0.7094/
       data ry2/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *   0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.7109/
       data rx3/0.8030,0.8016,0.7996,0.7971,0.7899,0.7848,0.7780, !for 25 C
     *   0.7691,0.7570,0.7404,0.7171,0.6835,0.6343,0.5627,0.4632/
       data ry3/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *   0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.7109/
       data rx4/0.7197,0.7130,0.7080,0.7019,0.6859,0.6750,0.6614, !for 20 C
     *   0.6440,0.6217,0.5925,0.5537,0.5024,0.4352,0.3506,0.2492/
       data ry4/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *   0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.7109/
       data rx5/0.6363,0.6236,0.6149,0.6047,0.5781,0.5606,0.5392, !for 15 C
     *   0.5128,0.4799,0.4389,0.3882,0.3263,0.2529,0.1685,0.0741/
       data ry5/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *   0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.7109/
       data rx6/0.5530,0.5333,0.5206,0.5058,0.4680,0.4437,0.4148, !for 10 C
     *   0.3802,0.3390,0.2904,0.2337,0.1690,0.0967,0.0173,0.0000/
       data ry6/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *   0.2891,0.3288,0.3728,0.4219,0.4776,0.5418,0.6179,0.6366/
       data rx7/0.4697,0.4427,0.4258,0.4063,0.3578,0.3277,0.2928, !for 5 C
     *          0.2525,0.2066,0.1547,0.0970,0.0337,0.0000/
       data ry7/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *          0.2891,0.3288,0.3728,0.4219,0.4776,0.5094/
       data rx8/0.3863,0.3521,0.3312,0.3075,0.2501,0.2157,0.1770, !for 0 C
     *          0.1339,0.0862,0.0341,0.0000/
       data ry8/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *          0.2891,0.3288,0.3728,0.4025/
       data rx9/0.3030,0.2621,0.2378,0.2106,0.1470,0.1100,0.0696, !for -5 C
     *          0.0256,0.0000/
       data ry9/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2528,
     *          0.2891,0.3073/
       data rx10/0.2197,0.1732,0.1461,0.1166,0.0491,0.0112,0.0000/ !for -10 C
       data ry10/0.0397,0.0837,0.1075,0.1328,0.1885,0.2194,0.2286/ 
       data rx11/0.1363,0.0855,0.0567,0.0257,0.0000/ !for -15 C
       data ry11/0.0397,0.0837,0.1075,0.1328,0.1525/ 
       data rx12/0.0530,0.0000/ !for -20 C 
       data ry12/0.0397,0.0831/

c-----------------------------------------------------------------------
c* To set domain area.

       call set(0.2,0.8,0.1,0.9,0.,1.,0.,1.,1)

c-----------------------------------------------------------------------
c* To set blue color for plotting line.

       call gsplci(5)
       call gstxci(5)

c-----------------------------------------------------------------------
c* To plot tilted line(potential temperature).

       call curve(x1,y1,nxy(1))
       call curve(x2,y2,nxy(2))
       call curve(x3,y3,nxy(3))
       call curve(x4,y4,nxy(4))
       call curve(x5,y5,nxy(5))
       call curve(x6,y6,nxy(6))
       call curve(x7,y7,nxy(7))
       call curve(x8,y8,nxy(8))
       call curve(x9,y9,nxy(9))
       call curve(x10,y10,nxy(10))
       call curve(x11,y11,nxy(11))
       call curve(x12,y12,nxy(12))
       call curve(x13,y13,nxy(13))
       call curve(x14,y14,nxy(14))
       call curve(x15,y15,nxy(15))
       call curve(x16,y16,nxy(16))
       call curve(x17,y17,nxy(17))
       call curve(x18,y18,nxy(18))
       call curve(x19,y19,nxy(19))

c-----------------------------------------------------------------------
c* To set green color for plotting mixing ratio line.

       call gsplci(3)
       call gstxci(3)
       call setusv('LW',3000)

c-----------------------------------------------------------------------
c* To plot pseudo-adiabatic line

       call curve(rx1,ry1,nr(1))
       call curve(rx2,ry2,nr(2))
       call curve(rx3,ry3,nr(3))
       call curve(rx4,ry4,nr(4))
       call curve(rx5,ry5,nr(5))
       call curve(rx6,ry6,nr(6))
       call curve(rx7,ry7,nr(7))
       call curve(rx8,ry8,nr(8))
       call curve(rx9,ry9,nr(9))
       call curve(rx10,ry10,nr(10))
       call curve(rx11,ry11,nr(11))
       call curve(rx12,ry12,nr(12))
       call setusv('LW',1000)

c-----------------------------------------------------------------------
c* To set black color for plotting line.

       call gsplci(1)
       call gstxci(1)

c-----------------------------------------------------------------------
c* To plot mixing line

       ipat=ishift(21845,1)
       call dashdb(ipat)
       call curved(qx1,qy1,nq(1))
       call curved(qx2,qy2,nq(2))
       call curved(qx3,qy3,nq(3))
       call curved(qx4,qy4,nq(4))
       call curved(qx5,qy5,nq(5))
       call curved(qx6,qy6,nq(6))
       call curved(qx7,qy7,nq(7))
       call curved(qx8,qy8,nq(8))
       call curved(qx9,qy9,nq(9))
       call curved(qx10,qy10,nq(10))
       call curved(qx11,qy11,nq(11))
       call curved(qx12,qy12,nq(12))
       call curved(qx13,qy13,nq(13))

c-----------------------------------------------------------------------
c* To write temperature characters.

       write(ca2,'(i2)')30
       call pwritx(0.9386,0.0714,ca2,2,10,0,1)
       write(ca2,'(i2)')20
       call pwritx(0.8671,0.1429,ca2,2,10,0,1)
       write(ca2,'(i2)')10
       call pwritx(0.7957,0.2143,ca2,2,10,0,1)
       write(ca1,'(i1)')0
       call pwritx(0.7243,0.2857,ca1,1,10,0,1)
       write(ca3,'(i3)')-10
       call pwritx(0.6629,0.3571,ca3,3,10,0,1)
       write(ca3,'(i3)')-20
       call pwritx(0.5914,0.4286,ca3,3,10,0,1)
       write(ca3,'(i3)')-30
       call pwritx(0.52,0.5,ca3,3,10,0,1)
       write(ca3,'(i3)')-40
       call pwritx(0.4486,0.5714,ca3,3,10,0,1)
       write(ca3,'(i3)')-50
       call pwritx(0.3771,0.6429,ca3,3,10,0,1)
       write(ca3,'(i3)')-60
       call pwritx(0.3057,0.7143,ca3,3,10,0,1)
       write(ca3,'(i3)')-70
       call pwritx(0.2343,0.7857,ca3,3,10,0,1)
       write(ca3,'(i3)')-80
       call pwritx(0.1629,0.8571,ca3,3,10,0,1)
       write(ca3,'(i3)')-90
       call pwritx(0.0914,0.9286,ca3,3,10,0,1)

c-----------------------------------------------------------------------
c* To plot zero value temperature line.

       call setusv('LW',2000)
       xx(1)=0.3333
       xx(2)=0.705
       yy(1)=0.0
       yy(2)=0.2788
       call curve(xx,yy,2)
       xx(1)=0.725
       xx(2)=1.0
       yy(1)=0.2938
       yy(2)=0.5
       call curve(xx,yy,2)

       return
       end

       subroutine flag1(x,y,speed,theta,iuv,len)
c***********************************************************************
c subroutine name  : flag                                              *
c                                                                      *
c description      : To plot wind flag or vector for single point.     *
c                                                                      *
c I/O parameters   :                                                   *
c  input:  name       type        description                          *
c          x          real        the x-coordinate.                    *
c          y          real        the y-coordinate.                    *
c          speed      real        the wind speed or u-component.       *
c          theta      real        the wind direction or v-component.   *
c          iuv        integer     = 1, (speed,theta) to plot flag in   *
c                                      unit of knots;                  *
c                                 = 2, (u,v) to plot flag in unit of   *
c                                      knots;                          *
c                                 =-1, (speed,theta) to plot vector in *
c                                      unit of meters/sec;             *
c                                 =-2, (u,v) to plot flag in unit of   *
c                                      meters/sec.                     *
c          len        integer     the flag or vector length, the       *
c                                 default value is 20.                 *
c                                                                      *
c called fun./sub. : getset,frstpt,vector --- NCAR GRAPHICS            *
c                                                                      *
c***********************************************************************

      PARAMETER (SWP=7.5/11., DXL=0.2, SPDMIN=3.)
      PARAMETER (DEG120=120.,N2=2, PI=3.1415926)
      PARAMETER (V10=10., HFLN=0.5)
      PARAMETER (N3=N2+1, DEG60=PI-PI/180.*DEG120)
      DATA SPV/-999./
      CALL GETSET(r11,r12,r13,r14,X11,X22,Y11,Y22,N15)
      n11=1024*r11
      n12=1024*r12
      n13=1024*r13
      n14=1024*r14
      RLENG=ABS(LEN)
      IF(LEN.EQ.0)RLENG=25.
      RLENGX=RLENG/(N12-N11)*(X22-X11)
      RLENGY=RLENG/(N14-N13)*(Y22-Y11)
      IF(IABS(IUV) .LE. 1)THEN
          THE=(90. - THETA) * PI / 180.
          SPD=SPEED
          IF(SPD.LE.0.)GOTO 50
      ELSE
          U=SPEED
          V=THETA
          SPD=SQRT( U*U + V*V )
          IF(SPD .EQ. 0.)GOTO 50
          THE=ATAN2( -V, -U)
      ENDIF
      IF(SPD.EQ.SPV)GOTO 50
      IF(LEN.LT.0)SPD=SPD*2
      IF(SPD.EQ.SPV)GOTO 50
      CALL FRSTPT(X,Y)
      IF(IUV.LT.0)GOTO 30

c-----------------------------------------------------------------------
c* To plot wind flag.

      SP120X=RLENGX * SWP
      SP120Y=RLENGY * SWP
      XL= RLENGX * COS(THE)
      YL= RLENGY * SIN(THE)
      DLX = XL * DXL
      DLY = YL * DXL
      IF(SPD.GE.83)THEN
        XL=XL*1.5
        YL=YL*1.5
      ENDIF
      X2=X+XL
      Y2=Y+YL
      CALL VECTOR(X2,Y2)
      IF(SPD .LT.SPDMIN)GOTO 50
      D1=THE - DEG60
      XE= SP120X * COS(D1)
      YE= SP120Y * SIN(D1)
              IF(SPD.GE.8.)THEN

c-----------------------------------------------------------------------
c* To plot for 50 knots.

      LS1= (SPD+2) / 50
      DO 10 I=1,LS1
      X22=X2-DLX+XE
      Y22=Y2-DLY+YE
      CALL VECTOR(X22,Y22)
      DO 8 K=1,N2
      AX=DLX*K/N3
      AY=DLY*K/N3
      CALL FRSTPT(X2-AX,Y2-AY)
   8  CALL VECTOR(X22,Y22)
      X2=X2 - DLX
      Y2=Y2 - DLY
      CALL VECTOR(X2,Y2)
      X2=X2 - DLX
      Y2=Y2 - DLY
      CALL FRSTPT(X2,Y2)
  10  CONTINUE
      REM=SPD-LS1*50
      IF(REM .LT. 3.)GOTO 50

c-----------------------------------------------------------------------
c* To plot for 10 knots.

      LS2= (REM+2) / 10
      DO 20 I=1,LS2
      CALL VECTOR(X2+XE,Y2+YE)
      X2=X2 - DLX
      Y2=Y2 - DLY
      CALL FRSTPT(X2,Y2)
  20  CONTINUE

c-----------------------------------------------------------------------
c* To plot for 5 knots. 

      REM= SPD - INT((SPD+2)/10) * 10
      IF(REM .LT. 3.)GOTO 50
              ELSE
      X2=X + XL * 0.6
      Y2=Y + YL * 0.6
      CALL FRSTPT(X2,Y2)
              ENDIF
      CALL VECTOR(X2+XE/2, Y2+YE/2)
      GOTO 50
  30  CONTINUE

c-----------------------------------------------------------------------
c* To plot wind vector.

      THE=THE+PI
      XY=SPD/V10
      X2=X+XY*COS(THE)*RLENGX
      Y2=Y+XY*SIN(THE)*RLENGY
      CALL VECTOR(X2,Y2)
      THE1=THE+PI/6
      X1=-COS(THE1)*RLENGX*HFLN*XY
      Y1=-SIN(THE1)*RLENGY*HFLN*XY
      CALL VECTOR(X2+X1,Y2+Y1)
      THE1=THE-PI/6
      X1=-COS(THE1)*RLENGX*HFLN*XY
      Y1=-SIN(THE1)*RLENGY*HFLN*XY
      CALL FRSTPT(X2,Y2)
      CALL VECTOR(X2+X1,Y2+Y1)
  50  CONTINUE
      RETURN
      END

       subroutine defcolor
c***********************************************************************
c subroutine       : defcolor                                          *
c                                                                      *
c description      : Define a set of RGB color triples for colors 0    *
c                    through 5. Background Color indice 0 is white,    *
c                    foreground color indice 1 is black, color indice  *
c                    2 is red for plotting temperature, color indice 3 *
c                    is green for plotting mixing ratio, color indice  *
c                    4 is yellow for filling base map, and color indice*
c                    5 is blue for plotting other.                     *
c                                                                      *
c program language : Fortran 77                                        *
c                                                                      *
c data file        : none                                              *
c                                                                      *
c parameters       :                                                   *
c          name,      type,       description                          *
c          rgbv(3,5)  real array  RGB color triples.                   *
c                                                                      *
c called fun./sub. : gscr(NCAR G.K.S. color define.)                   *
c                                                                      *
c***********************************************************************

       dimension rgbv(3,5)

       data rgbv / 0.000 , 0.000 , 0.000 ,   ! foreground black color.
     +             1.000 , 0.000 , 0.000 ,   ! red color.
     +             0.000 , 0.700 , 0.000 ,   ! green color.
     +             1.000 , 1.000 , 0.000 ,   ! yellow color.
     +             0.000 , 0.000 , 1.000 /   ! blue color.

       call gscr(1,0,1.,1.,1.)                ! backgroud white color.
       do i=1,5
          call gscr(1,i,rgbv(1,i),rgbv(2,i),rgbv(3,i))
       enddo
       return
       end

      subroutine flag(xpt,ypt,speed,theta,iuv,vlen,vsca)
c
c  iuv .ge.0,  plot wind flag
c      .lt.0,  plot wind vector
c
c  iabs(iuv) .le.1, speed is wind speed
c                   theta is wind direction
c            .eq.2, speed is u component
c                   theta is v component
c
c  icrc =1 draw a circle at the orgion
c       =2 draw a square at the orgion
c       =0 none of above
c
c  ihd head pattern for vector
c       =0 no head (single bar)
c       =1 open head
c       =2 half closed head
c       =3 closed head
c       =4 arrow head
c       =5 dark half closed head
c       =6 dark closed head
c       =7 dark arrow head
c
c  bar: length of bar
c  dbar: spacing between bars
c  ndark: number of lines to make flag or head dark
c  v10 : a normalize factor for vector to use
c
c  vlen  length of wind flag or wind vector(in crt unit, i.e. 1-1023)
c
c. liu  sep. 89
c
      parameter(bar=0.5, dbar=0.15, radus=0.06, pi=3.14159265358979)
      parameter(deg120=120., ndark=2, hfln=0.25, ihd=3)
      parameter(hdang=pi/7.5, d2r=pi/180., deg60=pi-deg120*d2r)
      common/baseline/xbase,base
 
      v10=vsca
      call getset(rn11,rn12,rn13,rn14,x11,x22,y11,y22,n55)
c     fac=real(rn14-rn13)/real(rn12-rn11)*(x22-x11)/(y22-y11)
      vleng=abs(vlen)
      if(vlen .eq. 0.)vleng=25.
      vlengx=vleng/1024./(rn12-rn11)*(x22-x11)
      vlengy=vleng/1024./(rn14-rn13)*(y22-y11)
      rad=radus*vlengx
      x=xpt
      y=ypt
      icrc=0
      if(icrc.eq.1)then
         call frstpt(x+rad,y)
         do 10 deg=10.,360.,30.
         call vector(x+rad*cos(deg*d2r), y+rad*sin(deg*d2r))
   10    continue
      else if(icrc.eq.2)then
         call frstpt(x-rad,y-rad)
         call vector(x-rad,y+rad)
         call vector(x+rad,y+rad)
         call vector(x+rad,y-rad)
         call vector(x-rad,y-rad)
      endif
      if(iabs(iuv) .le. 1)then
         if(speed .le. 0.)goto 90
         u=-speed*sin(theta*d2r)
         v=-speed*cos(theta*d2r)
      else
         u=speed
         v=theta
      endif
      spd=sqrt(u*u+v*v)
      if(spd .eq. 0.)goto 90
      the=atan2(-v,-u)-xbase
c         the=atan2(-v*fac,-u)
      if(vlen .lt. 0.)spd=spd*1.9438445
      if(icrc.eq.1)then
         if(iuv.ge.0)then
            x=xpt+rad*cos(the)
            y=ypt+rad*sin(the)
         else
            x=xpt+rad*cos(the+pi)
            y=ypt+rad*sin(the+pi)
         endif
      endif
      call frstpt(x,y)
      if(iuv .lt. 0)goto 50
c
c  plot wind flag
c
      spd120x=vlengx*bar
      spd120y=vlengy*bar
      xl=vlengx*cos(the)
      yl=vlengy*sin(the)
      dlx=xl*dbar
      dly=yl*dbar
      x2=x+xl
      y2=y+yl
      call vector(x2,y2)
      spd1=spd+2.5
      l50=spd1/50.
      l10=(spd1-l50*50)/10.
      l5=(spd1-l50*50-l10*10)/5.
      if(spd .lt. 2.5)goto 90
      d1=the-deg60
      xe=spd120x*cos(d1)
      ye=spd120y*sin(d1)
      if(spd .ge. 7.5)then
c
c  for 50 knots
c
         dlxx=dlx*1.5
         dlyy=dly*1.5
         do 30 i=1,l50
         x22=x2-dlxx+xe
         y22=y2-dlyy+ye
         call vector(x22,y22)
         do 20 k=1,ndark
         ax=(dlxx*k)/(ndark+1)
         ay=(dlyy*k)/(ndark+1)
         call frstpt(x2-ax,y2-ay)
  20     call vector(x22,y22)
         x2=x2-dlxx
         y2=y2-dlyy
         call vector(x2,y2)
         x2=x2-dlx
         y2=y2-dly
         call frstpt(x2,y2)
  30     continue
c
c  for 10 knots
c
         do 40 i=1,l10
         call vector(x2+xe,y2+ye)
         x2=x2-dlx
         y2=y2-dly
         call frstpt(x2,y2)
  40     continue
         if(l5.eq.0)goto 90
      else
c
c  for 5 knots
c
         x2=x+xl*0.6
         y2=y+yl*0.6
c         x2=x+xl
c         y2=y+yl
c
         call frstpt(x2,y2)
      endif
      call vector(x2+xe/2, y2+ye/2)
      goto 90

  50  continue
c
c  plot wind vector
c
      the=the+pi
      xy=spd/v10
      xyhd=xy
      if(xyhd.lt.1.0)xyhd=1.0
      if(xyhd.gt.1.5)xyhd=1.5
      x2=x+xy*cos(the)*vlengx
      y2=y+xy*sin(the)*vlengy
      the1=the+hdang
      x10=-cos(the1)*vlengx*hfln*xyhd
      y10=-sin(the1)*vlengy*hfln*xyhd
      the1=the-hdang
      x1=-cos(the1)*vlengx*hfln*xyhd
      y1=-sin(the1)*vlengy*hfln*xyhd
      if(ihd.eq.0)then
c
c  no head
c
         call vector(x2,y2)
      else if(ihd.eq.1)then
c
c  open head
c
         call vector(x2,y2)
         call vector(x2+x10,y2+y10)
         call frstpt(x2,y2)
         call vector(x2+x1,y2+y1)
      else if(ihd.eq.2)then
c
c half closed head
c
         call vector(x2,y2)
         call vector(x2+x10,y2+y10)
         call vector(x2+(x10+x1)/2.,y2+(y10+y1)/2.)
      else if(ihd.eq.3)then
c
c  close head
c
         call vector(x2,y2)
         call vector(x2+x10,y2+y10)
         call vector(x2+x1,y2+y1)
         call vector(x2,y2)
      else if(ihd.eq.4)then
c
c  blank arrow head
c
         x22=x+xy*cos(the)*vlengx*0.8
         y22=y+xy*sin(the)*vlengy*0.8
         call vector(x22,y22)
         call vector(x22+x10,y22+y10)
         call vector(x2,y2)
         call vector(x22+x1,y22+y1)
         call vector(x22,y22)
      else if(ihd.eq.5)then
c
c dark half closed head
c
         ddx=(x1-x10)/(ndark*4.)
         ddy=(y1-y10)/(ndark*4.)
         xm=(x10+x1)/2.
         ym=(y10+y1)/2.
         call vector(x2,y2)
         do i=1,ndark
         call vector(x2+x10+(i-1)*ddx,y2+y10+(i-1)*ddy)
         call vector(x2+xm -(i-1)*ddx,y2+ym -(i-1)*ddy)
         call vector(x2,y2)
         end do
      else if(ihd.eq.6)then
c
c  dark close head
c
         call vector(x2,y2)
         ddx=(x1-x10)/(ndark*2.)
         ddy=(y1-y10)/(ndark*2.)
         do i=1,ndark
         call vector(x2+x10+(i-1)*ddx,y2+y10+(i-1)*ddy)
         call vector(x2+x1 -(i-1)*ddx,y2+y1 -(i-1)*ddy)
         call vector(x2,y2)
         end do
      else if(ihd.eq.7)then
c
c  dark arrow head
c
         x22=x+xy*cos(the)*vlengx*0.8
         y22=y+xy*sin(the)*vlengy*0.8
         drx=(x2-x22)/ndark
         dry=(y2-y22)/ndark
         ddx=x10/ndark
         ddy=y10/ndark
         ddxx=x1/ndark
         ddyy=y1/ndark
         call vector(x22,y22)
         do i=1,ndark
         call vector(x22+x10-(i-1)*ddx,y22+y10-(i-1)*ddy)
         call vector(x2-(i-1)*drx,y2-(i-1)*dry)
         call vector(x22+x1-(i-1)*ddxx,y22+y1-(i-1)*ddyy)
         call vector(x22,y22)
         end do
      endif
  90  continue
      return
      end
      subroutine sort(npt,p,z,t,td,wd,ws,spv)
c
c  rearrange the data in decending order
c
      dimension p(npt),z(npt),t(npt),td(npt),wd(npt),ws(npt)
 
      do 20 i=1,npt
      do 10 j=i,npt
      if(i.eq.j)goto 10
      if(p(i).ge.p(j))goto 10
      call switch(p(i),p(j))
      call switch(z(i),z(j))
      call switch(t(i),t(j))
      call switch(td(i),td(j))
      call switch(wd(i),wd(j))
      call switch(ws(i),ws(j))
  10  continue
  20  continue
      return
      end
      subroutine switch(a,b)
      c=a
      a=b
      b=c
      return
      end

      subroutine readsnd(input,stn,iy,im,id,ih,pp,tt,tdd,wdd,wss,zz,np)
      dimension pp(1),tt(1),tdd(1),wdd(1),wss(1),zz(1)
      character*5 stn,input*100

      open(11,file=input,form='formatted',status='old')

c     read(11,11)stn,rlat,rlon,iy,im,id,ih,n
      read(11,11)stn,itime,ix,id,ih,n
   11 format(3x,a5,4x,2f5.2,2x,4i2,2x,i3)
      print*,n
c     write(21,'(a5,4i3.2)')stn,iy,im,id,ih

      ii=0
      do i=2,n-1
      read(11,12)p,z,t,td,wd,ws
   12 format(2x,f5.0,f5.0,2x,f4.0,2x,f4.0,2x,f3.0,f3.0)
      
      if(p.le.-9990)then
         p=-999.
      else
         p=p*0.1
      endif

      if(z.le.-9990.)z=-999.

      if(t.le.-990.)then
         t=-999.
      else
         t=t*0.1
      endif

      if(td.le.-990. .or. t.le.-990.)then
         td=-999.
      else
         td=t-td*0.1
      endif

      if(wd.le.-90.)wd=-999

      if(ws.le.-90. .or. wd.le.-990.)then
         ws=-999.
      endif

c     write(21,21)p,t,td,ws,wd,z
c  21 format(3x,6f7.1)
      ii=ii+1
      pp(i)=p
      tt(i)=t
      tdd(ii)=td
      wdd(ii)=wd
      wss(ii)=ws
      zz(ii)=z
      enddo
      np=ii
      return
      end
