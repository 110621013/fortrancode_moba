'reinit'
'open totalstationdata.ctl'
'set display color white'
'c'

#'set rgb  16    0    0  255'
#'set rgb  17   55   55  255'
#'set rgb  18  110  110  255'
#'set rgb  19  165  165  255'
#'set rgb  20  220  220  255'

#'set rgb  21    0  255    0'
#'set rgb  22   55  255   55'
#'set rgb  23  110  255  110'
#'set rgb  24  165  255  165'
#'set rgb  25  220  255  220'

#'set rgb  26  255    0    0'
#'set rgb  27  255   55   55'
#'set rgb  28  255  110  110'
#'set rgb  29  255  165  165'
#'set rgb  30  255  220  220'


tt=1
#while(tt<97)
'c'
'enable print totalstationdata_temp_'tt'.gmf'
'set t 'tt
'set lon 119.3 122.2'
'set lat 21.5 25.5'
'set vpage 0 8.5 0 11'
'set parea 0.5 7.8 0.5 5.5'
'set clevs 2 7 15 20 25 30 35 40 45 50 55 60'
'set ccols 20 19 18 17 25 24 23 22 30 29 28 27'

#'set gxout stnmark'
#'set cmark 3'
#'set digsize 0.25'
'set mpdset twncounty'
'd temp'
#if (tt<10)
'draw title temp_20160123~26_0'tt'Z(c)'
#else
#'draw title stp_20160123_0'tt'Z((10^-6g)/m^3)'
#endif
'set gxout vector'
'set arrscl 1 10'
'd u;v'

*'set ccols 20 19 18 17 25 24 23 22 30 29 28 27'
#'run 106601015_hw7_cbarn 1 1 7.9 5.5'
*'set ccols 20 19 18 17 25 24 23 22 30 29 28 27'
'set ccols 1'
'/homework/grads/tool/vec.gs  u;v -SCL 0.5 10 -P 7.7 1.5'

'print'
'disable print'
'printim totalstationdata_temp_'tt'.gif gif'

tt=tt+1
#endwhile
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
tt=1
#while(tt<97)
'c'
'enable print totalstationdata_stp_'tt'.gmf'
'set t 'tt
'set lon 119.3 122.2'
'set lat 21.5 25.5'
'set vpage 0 8.5 0 11'
'set parea 0.5 7.8 0.5 10.5'
'set clevs 2 7 15 20 25 30 35 40 45 50 55 60'
'set ccols 25 24 23 22 20 19 18 17 29 28 27 26'

'set gxout stnmark'
'set cmark 3'
'set digsize 0.25'
'set mpdset twncounty'
'set parea  0.5 7.8 5.6 10.5'
'd stp'

'draw title stp_20160123~26_'tt'Z(hpa)'
'set gxout vector'
'set arrscl 1 10'
'set ccolor 1'
'd u;v'

'set clevs 2 7 15 20 25 30 35 40 45 50 55 60'
*'set ccols 20 19 18 17 25 24 23 22 30 29 28 27'
'run 106601015_hw7_cbarn 1 1 7.9 5.5'
*'set ccols 20 19 18 17 25 24 23 22 30 29 28 27'
'set ccols 1'
'run /homework/grads/tool/vec.gs  u;v -SCL 0.5 10 -P 7.7 1.5'

'print'
'disable print'
'printim totalstationdata_stp_'tt'.gif gif'

tt=tt+1
#endwhile
