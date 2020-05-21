;**************************************************
; Concepts illustrated:
;   - Drawing Skew-T plots
;   - Thinning the wind barbs in a Skew-T plot
;   - Customizing the background of a Skew-T plot
;**************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/skewt_func.ncl"
;**************************************************
 begin

; --- Read Data ----------------------------------------;
  diri = "./"
  fili = "4669902014081900.txt"
  nlvl = 122
  ncol = 10
  TestData = asciiread (diri+fili , (/nlvl,ncol/), "float")
; order: Surface is 'bottom'  eg: 1000,950,935,897,...

  p    = TestData (:,3)    ; pressure     [mb / hPa]
  tc   = TestData (:,5)    ; temperature  [C]
  tdc  = TestData (:,6)    ; dew pt temp  [C]
  z    = TestData (:,4)    ; geopotential [gpm]
  wspd = TestData (:,8)    ; wind speed   [knots or m/s]
  wdir = TestData (:,7)    ; meteorological wind dir

  tc@_FillValue   = 999.9  ; missing data
  tdc@_FillValue  = 999.9
  wdir@_FillValue = 999
  wspd@_FillValue = 999.9

;*************************
; create plot
;*************************
  wks  = gsn_open_wks ("png", "skewT_0819")

; --- Create background skew-T and plot sounding----------------

  skewtOpts                   = True     ;set background
  skewtOpts@DrawColAreaFill   = True    ; default is False
  skewtOpts@tiMainString      = "sounding data 0819"
  skewtOpts@DrawHeightScaleFt = False
  skewtOpts@DrawFahrenheit    = False   ; default is True
  skewtOpts@vpXF              = 0.1

  dataOpts                  = True    ; options describing data and ploting
  dataOpts@colTemperature   = "Black"
  dataOpts@colDewPt         = "Red"
  dataOpts@colCape          = "orange"

  dataOpts@Wthin            = 3        ; plot every n-th wind barb
  skewt_bkgd = skewT_BackGround (wks, skewtOpts)
  skewt_data = skewT_PlotData   (wks, skewt_bkgd, p,tc,tdc,z \
                                    , wspd,wdir, dataOpts)
  draw (skewt_bkgd)
  draw (skewt_data)
  frame(wks)


 end

