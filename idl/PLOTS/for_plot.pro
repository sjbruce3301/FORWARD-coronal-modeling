;+**********************************************************************
pro for_plot,IntMap,GridPramsStruct,ObsPramsStruct=ObsPramsStruct,StokesStruct=StokesStruct,xplotrange=xplotrange,yplotrange=yplotrange,mapname=mapname,$
	     dodisk=dodisk,colortable=colortable,cpos=cpos,plotlog=plotlog,imax=imax,imin=imin,psplot=psplot,eps=eps,$
	     usecolor=usecolor,axiscolor=axiscolor,docont=docont,nocontcolor=nocontcolor,noclabel=noclabel,nlev=nlev,working_dir=working_dir,$
             c_charsize=c_charsize,winnumber=winnumber,nwinx=nwinx,nwiny=nwiny,units=units,bgcolor=bgcolor,$
	     dispexp=dispexp,dispgamma=dispgamma,rpow=rpow,nobangletitle=nobangletitle,addturbtitle=addturbtitle,$
	     whitedisk=whitedisk,occult=occult,upoccult=upoccult,nulldatacolor=nulldatacolor,sunedge=sunedge,rfilter=rfilter,$
	     charsize=charsize,charthick=charthick,noerase=noerase,title=title,xtitle=xtitle,ytitle=ytitle,nowidgmess=nowidgmess,nodata
;
;Name: FOR_PLOT
;
;Purpose: To plot model output, or data put into
;		FORWARD map format (by for_plotfits)
;
;Input 
;	IntMap - Map structure containing intensity
;
;Keyword INPUTS
;
; These are determined in FOR_DRIVE
;
;       XPLOTRANGE/YPLOTRANGE - Range of map to plot (default is
;                               entire map)
;       MAPNAME - name of plot file
;	DODISK - if set, force occult=0, and sunedge=dodisk
;
; These are inputs explicitly to FOR_PLOT
;
;       PLOTLOG - plot in log scale (Default no)
;	COLORTABLE - show intensity color table bar (Default yes)
;       IMAX, IMIN - max and min for intensity
;       PSPLOT make postscript file; EPS make eps (default X plots, no output)
;       USECOLOR - color table value (Default 0, greyscale - can also use red/blue for
;		positive/negative data; warning dont do plotlog if so!  other color tables
;		may work, but generally should have black on the bottom and white on the top)
;	AXISCOLOR - plot color of axis 
; 	BGCOLOR -- background color -- default white (255)
;       DOCONT - add contours (Default)
;	NOCONTCOLOR - turn off contour colors (Default no)
;	NoCLAB - don't put labels on contours (Default no)
;	NLEV - number of contour levels (Default 11)
; 	C_CHARSIZE - contour character size
;       WINNUMBER - window to plot 
;	NWINX/NWINY size of window
;  	WHITEDISK -- occulting disk color (0 black, 1 white, others are rainbow eg 5 is yellow (DEFAULT -1 not on)
; 	OCCULT - disk size can be greater than 1
; 	UPOCCULT - upper limit above which no data plot
; 	NULLDATACOLOR -- nulldata color (Default not on ('');  0 black; 1 is white, others are rainbow; 
;          negative turns on signal/noise greater than one being defined as null)
;  	SUNEDGE -- set to draw yellow sun edge (Default yes)
;	RFILTER - enhance structure at higher radii. Choice NO_FILTER
;	        RPOWER_FILTER (Gilly, 2021)
;		NRGF_FILTER (Fineschi/Morgan et al 2006)
;		QUARTERPOWER_FILTER
;		GAMMA_FILTER
;			with parameters DISPGAMMA, DISPEXP
;		AIA_FILTER (only for AIA DATA)
;			note aia_filter applied to data itself
;			within FOR_PLOTFITS - others dealt with below
;
;       CHARSIZE, CHARTHICK
;	NOERASE -- no erase
;	TITLE,XTITLE,YTITLE - plot title
;	UNITS - for Coronal Polarimeter and EUV Imager data, allows toggling between units
;
;       NOWIDGMESS - don't plot widget dialog, use message
; 	NOBANGLETITLE -- don't plot bangle/cmer title
;	ADDTURBTITLE -- include info on turbulence spatial resolution

;
;  Outputs:
;	NODATA -- if no data is passed, this flag gets set
;
;
;Common Blocks: none
;
;
; Called by FOR_DRIVE, FOR_PLOTFITS
;
; Calls FOR_NRGF, AIA_LCT, FOR_FIXUNITS
; 	PLOT_MAP, PLOT_IMAGE
;	PSA (not currently implemented)
;
; REQUIRES PACKAGE AIA
;
; MODIFICATION HISTORY:
;       17-Mar-2010 Created S. E. Gibson
;               adapted from ISSI_CAVITY_MINMAX codes
;		5-Apr-2010 Added call to device,color=UseColor for postscript TAK
;		16-May-2009 Added Color, Background, and Title keywords and added Bottom keyword to plot_image call
;		31-May-2010 Changed default window to current window TAK
;		18-June-2010 Fixed problem with windowing. If !d.window gt 32 the window may sometimes be 
;				deleted and replaced with a window with a differeent number. 
;		26-Jul-2010 Added NoCLabel keyword
;		22-Oct-2010 Added nocontcolor option
;		29-Jan-2011 Added sunedge
;	        23-May-2011 Replace L0 (Stonyhurst) with CMER which is now saved
;		27-July-2011 Sorted out color table issues, restructured code SEG
;				also nulldata and underdisk redefined
;               7-Nov-2011 -changed size of psplot so the title
;               doesn't get cut off anymore
;                          -changed number of contours to an odd number
;                          so zero-cont is plotted
;                          -  sunedge color set to 5 (before if
;                            whitedisk, they were same color)                             
;		1-Feb-2012 set it to only overplot nulldata and underdisk if 
;			not default color
;		27-Mar-2011 Set up so that it will use standard AIA color tables for AIA images. TAK
;		10-Apr-2012 Rplaced PRINT statements with MESSAGE statements. TAK	 
;		06-Jun-2012 Fixed nulldatacolor=8, whitedisk=8 to force black disk e.g. for color tables
;		with blue as bottom intensity color. LR/SEG
;		Nov 2012 made sure imin wasnt grabbing very small numbers that should have been zero
;		Jan 2013 moved mapname adjustments to codes that set it (for_drive, for_plotfits, plot_comp_quickinv)
;		17-Feb-2013 If window size not set minimum is now 256. TAK
;		21-Feb-2013 -- decided to try no longer associating window size at all 
;			with grid size, just defaulting to 256
;		26-Oct-2013 -- forced turn off of plotlog if negative data
;
; Version 2.0 July 2014
;
;		5-Feb-2016 - replaced all IntMap with UseMap -- when occult set it was
;			overwriting original map and so forcing an unnecessary recalculation
;		24-Feb-2016 - fixed bug related to SUNEDGE. TAK
;		06-Feb-2017 - fixed bug where nulldatanoise was not selecting all points
;			title now includes Bang and Cmer for all
;		 	Also changed nulldisk/underdisk default so that would be
;			 imin (since it is not always same as zero), that is, bottom of color bar;  
;			 or special case color table 42 or 41 middle imin/imax=white
;						or 66 middle imin/imax=black
;			Also got rid of .98 restriction on nulldata fillin
;		12-May-2017 -- forced device decomposed=0 in case someone has their system set to 1
;				- also, changed how Noise is used.  It no longer is multiplied by
;				 a random number generator in FOR_PERSONALITY so that what comes out of
;				there and is assigned to QuantMap.Noise is the magnitude of the relative error (noise/signal)
;				not a random number scaled to that magnitude.  This is better because the test for noise/signal > relerr (points not to plot)
;				is done by comparing to the magnitude, while before it was compared to the scaled values which would be generally smaller 
;				and so would be an overestimate of "goodness" of data.
;				The ability to make plots with noisy looking data is now dealt with only here in FOR_PLOT, through a keyword 
;				NORANDOM=0 (default = 0, plot with random noise)
;	 	6-Feb-2018 -- got rid of making zero data null data
;		18-Apr-2018 -- added capability to change signal to noise ratio via MinNos 
;		11-Jul-2018 -- changed it so color bar possible even if noerase
;			also passed charsize through to color bar
;			also made EPS plots not have red "Plane of Sky" text
;		26-Jul-2018 -- added BGCOLOR, adjusted options for contour colors
;		25-Aug-2018 -- went back to making zero data null data for case plotlog=1
;		October 2018-- fixed bug where noise > 1 not relerr were excluded
;			also made changes to allow noise being plotted on Carrington maps
;		January 2019 -- made adjustment for Az to range +/- 90 
;			only apply for IR, not UV
;		June 2019 -- used slash for PC compatibility
;		Sept 2019 -- commented out psplot and usethick
;		August 2020 -- added gamma_filter, dispexp,dispgamma
;		November 2020 -- added check for color table 41 with 42
;	        January 2021 -- added rpower_Filter (Gilly),rpow
;		February 2021 -- allowed PHysical Diagnostics to use rfilter (at least from line command for_drive)
;		Aguust 2021 -- fixed offset in Carrington maps of dph/dth / 2.
;			most noticable in KCOR data maps
;	        Sept 2021 -- adjusted KCOR L1 display factor
;			Also adjusted color bar left vs right and ps file size
;		Nov 2021 -- print statement for ps 
;		Dec 2021 -- clarifications for random error
;			and update for UV relative error
;		Jan 2022 -- change name MINNOS to ERRTRUNC
;		Mar 2022 -- uncommented line which stopped rfilter for physical diagnostics
;			(so undid change of Feb 2021) because causing problems when moreplots set
;		Apr 2022 -- edited plane of sky text to allow Thomson Sphere
;		Jun 2022 -- removed "Plane of sky" text from physical diagnostic plots
;			added keyword NOBANGLETITLE to remove BANGLE and CMER from title
;			adjusted nulldata definition to not include under r=1 when occult < 0
;			shifted color bar for ps
;		Sep 2022 -- removed "longer" addition of !c that was creating odd spaces
;		Oct 2022 - added addturbtitle
;		put "Plane of sky" text back into physical diagnotic plots- 
;			that's how we know we are looking at slice at TS or X=0!
;		Nov 2022 - 
;			put POS text in title
;			and added info on DODISK and OCCULT and LOSOFFSET to it 
;			and removed warning of changing IMIN and IMAX for POS
;			   (because fixed problem in for_plotdefaults where
;				it was not searching on all POS)
;			Moved decomposed statement below eps set_plot
;			Also didn't force sunedge = dodisk any more
;		Jan 2023 - moved numbers to right of color bar for CARRMAP
;		Feb 2023-- replaced *title *titleold if set
;			to avoid overwriting user input and risking
;			multiple UNITS/addtitle addon in loops
;		April 2023 -- changed all the E8.2 with E9.2
;		Dec 2023 -- fixed problem where addxtitle was being added to custom x titles
;		Feb 2024 -- fixed bug where nlev=1 divided by zero
;		Aug 2024 -- added hooks for WLRAT

mdtor=!dpi/180d0

;
; set up plot stuff
;

usethick=1

slash=path_sep()

saveplotname=mapname & if n_elements(working_dir) eq 1 then if working_dir ne '' then saveplotname=working_dir+slash+mapname

if eps eq 1 then begin
  set_plot,'ps'
  psplot = 1
  nowidgmess=1
  axiscolor=0
  !p.font=0
endif

device,get_decomposed=userdevice
if userdevice eq 1 then begin
 print,'changing device,decomposed=0' 
 device,decomposed = 0
endif

if noerase eq 0. then begin
 if psplot eq 1 then begin
;
; note the below was done for reasons I forget,
; the problem is in a gang plot where noerase gets set
; might make things inconsistent
; so I am commenting out
;
; 	usethick=5

	if eps eq 1 then begin 
	   device,filename=saveplotname+'.eps',xs=6.,ys=8.*nwiny/nwinx,/inches,/encapsulated,/bold,/helvetica,bits=16,/color
; 
; need to get this to work
;
;           psa,/color,/encaps,xs=8.,ys=8.*nwiny/nwinx,/inches,filename=saveplotname+'.eps'
	endif else begin
	   set_plot,'ps'
	   !p.font=0
	   device,filename=saveplotname+'.ps',xs=6.,ys=6.*nwiny/nwinx,/inches,/bold,/helvetica,bits=16,/color
	endelse
        cd,current=thedirectory
        print,'saved ps file '+saveplotname+'.ps from directory '+thedirectory
 endif else begin
 	if strupcase(!version.os_family) eq 'UNIX' then set_plot,'x'
 	if strupcase(!version.os_family) eq 'WINDOWS' then set_plot,'win'
 	if winnumber lt 32 then window,winnumber,xs=nwinx,ys=nwiny,retain=2 $
 	else begin	;for !d.window ge 32 can't change 
 			;window size directly, must delete first.
;	THIS SHOULD NEVER HAPPEN FOR WIDGETS! because widget window
;  	will by definition have nwinx and nwiny sized window
;
 		if not (nwinx eq !d.x_size and nwiny eq !d.y_size) then begin
			print,'if you are running widgets, something is wrong with window display'
 			wdelete,winnumber
 			window,/free,xs=nwinx,ys=nwiny,retain=2
 		endif
 	endelse
 endelse
endif
;print,'for_plot',winnumber,' d.window',!d.window

UseMap=IntMap

;
; Set up customized color table
; with rainbow colors at bottom to use
; for contours, etc.
;

if usecolor ne 7777 then begin
  
  colorfile=file_dirname(GET_ENVIRON('FORWARD'))+slash+file_basename(GET_ENVIRON('FORWARD'))+slash+'PLOTS'+slash+'for_colors.tbl'
	
  if usecolor ne 66 then begin
   loadct,usecolor,file=colorfile, /silent
   if keyword_set(rfilter) then begin
     if strupcase(rfilter) eq 'GAMMA_FILTER' then gamma_ct,dispgamma
   endif
  endif else begin
;
; special 66 red/blue shifted color table adapted from mike galloy
;  allows plotting Azimuth from -90 to 90
;
   ncolors = 256
   loadct, 6, /silent, ncolors=ncolors
   tvlct, r, g, b, /get
   r[0:ncolors - 1] = shift(r[0:ncolors - 1], ncolors / 2)
   g[0:ncolors - 1] = shift(g[0:ncolors - 1], ncolors / 2)
   b[0:ncolors - 1] = shift(b[0:ncolors - 1], ncolors / 2)
   tvlct, r, g, b
  endelse

endif else begin
;       if strpos(strupcase(UseMap.id),'AIA') ge 0 then begin
        if strpos(strupcase(ObsPramsStruct.Instrument),'AIA') ge 0 then begin
       		AIABands=[94,131,171,193,211,304,335]
       		IDband=intarr(n_elements(AIABands))
;       		for i=0,n_elements(AIABands)-1 do IDBand[i]=strpos(UseMap.id,trim(AIABands[i]))
       		for i=0,n_elements(AIABands)-1 do IDBand[i]=strpos(ObsPramsStruct.LineName,trim(AIABands[i]))
       		wp=where(idband ge 0,c)
       		if c eq 1 then aia_lct,wavelnth=AIABands[wp[0]],/load $
         		 else aia_lct,wavelnth=193,/load
       endif else  aia_lct,wavelnth=193,/load
endelse


if bgcolor eq 0 then begin
 red = [0,1,1,0,0,1,1,0]
 green = [0,1,0,1,0,1,0,1]
 blue = [0,1,0,0,1,0,1,1]
 tvlct, 255*red, 255*green, 255*blue 
endif else begin
 red = [0,255,175,0,0,175,175,0]
 green = [0,255,0,175,0,175,0,175]
 blue = [0,255,0,0,175,0,175,175]
 tvlct,red,green,blue
endelse

Bottom=8

!p.background=bgcolor

;
; make sure there is not a contradiction between
; DODISK and OCCULT
;

if is_number(dodisk) then begin
 if dodisk ne 0 then begin
  if is_number(occult) then if occult ge 1 then occult=0.
; this next messes up the azequi and anyway, 
;    it's good to keep sunedge as absolute measure of solar disk
;  if is_number(sunedge) then if sunedge ne 0 then sunedge=dodisk
 endif
endif

odid=0
udid=0
if is_number(occult) then begin
  if occult gt 0 then odid=1 
endif
if is_number(upoccult) then begin
  if upoccult gt 0 then udid=1
endif

;
; allow for case of negative occult -- don't want < 1 to be nulldata if there is nothing there
;
if odid eq 1 and udid eq 1 then underdisk = where(GridPramsStruct.Rpos le occult or UseMap.data eq -9999. or GridPramsStruct.Rpos ge upoccult) 
if odid eq 1 and udid eq 0 then underdisk = where(GridPramsStruct.Rpos le occult or UseMap.data eq -9999.) 
if odid eq 0 and udid eq 1 then underdisk = where(UseMap.data eq -9999. or GridPramsStruct.Rpos ge upoccult) 
if odid eq 0 and udid eq 0 then underdisk = where(UseMap.data eq -9999.)
if is_number(occult) then begin
 if occult lt 0 then underdisk = where((GridPramsStruct.Rpos le 1. and UseMap.data eq -8888.) or UseMap.data eq -9999)
endif

if min(underdisk) ne -1 then UseMap.data[underdisk]=-9999.

;
; set up titles
;

if keyword_set(title) then titleold=title
if keyword_set(xtitle) then xtitleold=xtitle
if keyword_set(ytitle) then ytitleold=ytitle

addtitle=''
addxtitle=''

if strupcase(UseMap.CType) eq 'DISK' then begin
 if strupcase(UseMap.xunits) eq 'DEGREES' then begin
  default,xtitle,'(Degrees -- Azimuthal Equidistant projection)'
  if xtitle eq '' then xtitle='(Degrees -- Azimuthal Equidistant projection)'
  default,ytitle,'(Degrees -- Azimuthal Equidistant projection)'
  if ytitle eq '' then ytitle='(Degrees -- Azimuthal Equidistant projection)'
 endif else begin
  default,xtitle,'(Y_pos Rsun)'
  if xtitle eq '' then xtitle='(Y_pos Rsun)'
  default,ytitle,'(Z_pos Rsun)'
  if ytitle eq '' then ytitle='(Z_pos Rsun)'
 endelse
endif else begin
 default,xtitle,'Carrington Longitude ('+UseMap.phunits+')'
 if xtitle eq '' then xtitle='Carrington Longitude ('+UseMap.phunits+')'
 default,ytitle,'Colatitude ('+UseMap.thunits+')'
 if ytitle eq '' then ytitle='Colatitude ('+UseMap.thunits+')'
endelse

relerr=1.
if keyword_set(ObsPramsStruct) then begin
 if strpos(strupcase(ObsPramsStruct.Instrument),'OMP') ge 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' $
        or strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' $
	or strupcase(ObsPramsStruct.Instrument) eq 'WL' then begin
           unitsuse='NULL' 
           if keyword_set(units) then unitsuse = units
	   if ObsPramsStruct.NoisePrams.DoNoise eq 1 or strupcase(unitsuse) eq 'PHOTONS' then begin
		addtitle = addtitle+'!c Aperture='+strtrim(string(ObsPramsStruct.NoisePrams.Aperture,format="(I5)"),2)+' cm, Efficiency='+strtrim(string(ObsPramsStruct.NoisePrams.Efficiency,format="(F4.2)"),2)
		addtitle = addtitle+'!c Integration='+strtrim(string(ObsPramsStruct.NoisePrams.Integration,format="(I5)"),2)+' sec, modulation efficiency '+strtrim(string(ObsPramsStruct.NoisePrams.ModEffQuant,format="(F4.2)"),2)
		addtitle = addtitle+'!c Pixel size= '+strtrim(string(ObsPramsStruct.NoisePrams.Resolution,format="(F7.2)"),2)+' arcsec'
		if ObsPramsStruct.NoisePrams.DoNoise eq 1 then addtitle = addtitle+', Background='+strtrim(string(ObsPramsStruct.NoisePrams.Background,format="(F4.2)"),2)+' ppm'
;
; ErrTrunc sets signal to noise ratio (relative error)
; ALSO have added possibility of providing a negative ErrTrunc which then will
;  then be interpreted as an absolute error
;
                if is_number(nulldatacolor) then if nulldatacolor lt 0 then begin
	 	 if ObsPramsStruct.NoisePrams.ErrTrunc ne 0. then begin
  			relerr=ObsPramsStruct.NoisePrams.ErrTrunc
			abserr=0.
			if relerr lt 0. then begin
			  abserr=abs(relerr)
;
; note absolute error needs to be in the same units as the quantity
;
			  addtitle = addtitle+'!c truncated above absolute error='+strtrim(string(abserr,format="(E9.2)"),2)+' '+strtrim(string(UseMap.BUnit),2) 
			endif else $
			  addtitle = addtitle+'!c signal/noise error ON QUANTITY truncated at '+strtrim(string(1./relerr,format="(E9.2)"),2)
		 endif
                endif
	   endif
 endif

 if ((strupcase(ObsPramsStruct.Instrument) eq 'RADIO') or $
  (strupcase(ObsPramsStruct.Instrument) eq 'FARADAY')) then begin
  addtitle='!c Freq='+strtrim(string(ObsPramsStruct.Frequency_MHz),2)+' MHz'
  if ObsPramsStruct.DoGyro eq 1 then addtitle=addtitle+', with gyrores.'
 endif
 if tag_exist(ObsPramsStruct,'fcor') then if ObsPramsStruct.FCor eq 1 then addtitle='!c with F corona'

endif

;if strpos(UseMap.ID,'DATA') ge 0 then addtitle=addtitle+'!c B angle='+strtrim(UseMap.b0,2)+' Central Meridian='+strtrim(UseMap.cmer,2)
if keyword_set(nobangletitle) eq 0 then addtitle=addtitle+'!c B angle='+strtrim(UseMap.b0,2)+' Central Meridian='+strtrim(UseMap.cmer,2)
if keyword_set(addturbtitle) eq 1 then addtitle=addtitle+'!c turbulence resolution='+strtrim(addturbtitle,2)


;
; identify location of real data
;

;test=where(UseMap.Data*0. ne 0. or UseMap.Data eq 0.)
test=where(UseMap.Data*0. ne 0.)
if min(test) ne -1 then begin
	if keyword_set(nowidgmess) then message,/info,'there should not be NaN data.  Stopping' else d=dialog(/WARNING,'there should not be NaN data. Stopping')
	stop
endif

;
; nulldata has been set to -8888
; underdisk to -9999
; 
; we will take care of these in the plotting below
;

nodata=0
if plotlog eq 1 then realpts=where(UseMap.data ne -8888. and UseMap.data ne -9999. and UseMap.data ne 0.) $
  else realpts=where(UseMap.data ne -8888. and UseMap.data ne -9999.)

if min(realpts) eq -1 then begin
	if keyword_set(nowidgmess) then message,/info,'sorry, no non-zero data' else d=dialog(/WARNING,'sorry no non-zero data')
	nodata=1
	goto,label3
endif

nulldata=where(UseMap.data eq -8888. or UseMap.Noise eq -8888. $
  or (UseMap.data*0. ne 0. and GridPramsStruct.Rpos gt 0)$ 
  or (UseMap.Noise*0. ne 0. and GridPramsStruct.Rpos gt 0))

;
; Take care of restriction for relative or absolute error - flagged by negative nulldatacolor.
;   bear in mind here that UseMap.Noise is sigma/UseMap.data -- relative error in data
; 
;  Azimuth is a special case, because we are interested in the error relative to the departure
;  from radial or tangential, depending on whether it is a magnetic or electric dipole transition
;   for V/IR radial, it's departure from zero becayse Stokes linear polarization is in radial frame
;   so nothing needs to be done. But for UV, we need to remove 90 degrees
;
;
if is_number(nulldatacolor) then if nulldatacolor lt 0 then begin
 relerr_ref=UseMap.Noise
 abserr_ref=abs(UseMap.Noise*UseMap.Data)
 if (strpos(strupcase(ObsPramsStruct.LineName),'AZ') ge 0) then begin
  noise_temp=relerr_ref*UseMap.Data
  if strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' then begin
    relerr_ref[realpts]=noise_temp[realpts]/(UseMap.Data[realpts]-90.)
  endif 
 endif
 relerr_ref=abs(relerr_ref)
 if relerr gt 0 then begin
  if plotlog eq 1 then realpts=where(UseMap.data ne -8888. and UseMap.data ne -9999. and UseMap.data ne 0. and (relerr_ref le relerr)) $
  else realpts=where(UseMap.data ne -8888. and UseMap.data ne -9999. and (relerr_ref le relerr))
  nulldata=where(UseMap.data eq -8888. or UseMap.Noise eq -8888. $
    or (UseMap.data*0. ne 0. and GridPramsStruct.Rpos gt 0)$ 
    or (UseMap.Noise*0. ne 0. and GridPramsStruct.Rpos gt 0)$
    or (relerr_ref gt relerr and UseMap.data ne 0. and UseMap.Noise ne 0. and UseMap.data ne -9999. and UseMap.Noise ne -9999.))
 endif else begin
;
; absolute error
;
  if plotlog eq 1 then realpts=where(UseMap.data ne -8888. and UseMap.data ne -9999. and UseMap.data ne 0. and (abserr_ref le abserr)) $
  else realpts=where(UseMap.data ne -8888. and UseMap.data ne -9999. and (abserr_ref le abserr))
  nulldata=where(UseMap.data eq -8888. or UseMap.Noise eq -8888. $
    or (UseMap.data*0. ne 0. and GridPramsStruct.Rpos gt 0)$ 
    or (UseMap.Noise*0. ne 0. and GridPramsStruct.Rpos gt 0)$
    or (abserr_ref gt abserr and UseMap.data ne 0. and UseMap.Noise ne 0. and UseMap.data ne -9999. and UseMap.Noise ne -9999.))
 endelse
endif

if min(realpts) eq -1 then begin
	if keyword_set(nowidgmess) then message,/info,'sorry, no data with sufficient SNR' else d=dialog(/WARNING,'sorry no data with sufficient SNR')
	nodata=1
	goto,label3
endif

if min(nulldata) ne -1 then UseMap.data[nulldata]=0.d0
if min(underdisk) ne -1 then UseMap.data[underdisk]=0.d0

;
; change Stokes reference to be consistent with keyword RotAz -- only for polarimeters
;

if keyword_set(ObsPramsStruct) and keyword_set(GridPramsStruct) and (strpos(strupcase(ObsPramsStruct.Instrument),'OMP') ge 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG'  $
        or strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS') $
	and (strpos(strupcase(ObsPramsStruct.LineName),'Q') ge 0 or strpos(strupcase(ObsPramsStruct.LineName),'U') ge 0 or strpos(strupcase(ObsPramsStruct.LineName),'AZ') ge 0) then begin

;
; CoMP data will have been transformed to radial frame in FOR_PLOTFITS (also P angle taken care of there for Q and U)
; and model will have been transformed to radial frame in FOR_INTENSINT
;

 thns=GridPramsStruct.ThPos/mdtor
 testwest=where(GridPramsStruct.ThPos/mdtor gt 180.d0)
 if min(testwest) ge 0. then thns[testwest]=thns[testwest]-180.

 thew=GridPramsStruct.ThPos/mdtor+90.d0
 testsouth=where(GridPramsStruct.ThPos/mdtor gt 90.d0 and GridPramsStruct.ThPos/mdtor le 270.d0)
 if min(testsouth) ge 0. then thew[testsouth]=thew[testsouth]-180.d0
 testquad=where(GridPramsStruct.ThPos/mdtor gt 270.d0)
 if min(testquad) ge 0. then thew[testquad]=thew[testquad]-360.

 th45=GridPramsStruct.ThPos/mdtor+45.d0
 test45south=where(GridPramsStruct.ThPos/mdtor gt 45.d0 and GridPramsStruct.ThPos/mdtor le 225.d0)
 if min(test45south) ge 0. then th45[test45south]=th45[test45south]-180.d0
 test45quad=where(GridPramsStruct.ThPos/mdtor gt 225.d0)
 if min(test45quad) ge 0. then th45[test45quad]=th45[test45quad]-360.

 if strupcase(ObsPramsStruct.RotAz) ne 'RADIAL' then begin
   if strupcase(ObsPramsStruct.RotAz) eq 'N-S' then begin
     thetaadd=thns
     addxtitle=addxtitle+'!c (ref. N-S)'
     usetype=2
   endif 
   if strupcase(ObsPramsStruct.RotAz) eq '45' then begin
     thetaadd=th45
     addxtitle=addxtitle+'!c (ref. 45)'
     usetype=6
   endif 
   if strupcase(ObsPramsStruct.RotAz) eq 'E-W' then begin
     thetaadd=thew
     addxtitle=addxtitle+'!c (ref. E-W)'
     usetype=3
   endif
 endif else begin
   addxtitle=addxtitle+'!c (ref. local vertical/radial)'
   thetaadd=0. + thns*0.d0
   usetype=0
 endelse

 if usetype ne 0 then begin
;
; this deals with differences in StokesStruct model vs data output
;  see FOR_INTCOMPCLOSE (model) and FOR_READCOMPFITS (data)
;
  if tag_exist(StokesStruct,'Q') then begin
   if strpos(strupcase(ObsPramsStruct.LineName),'AZ') ge 0 then begin
     UseMap.data=UseMap.data+thetaadd
     UseMap.data=UseMap.data mod 180.
   endif
   if strpos(strupcase(ObsPramsStruct.LineName),'Q') ge 0 or strpos(strupcase(ObsPramsStruct.LineName),'U') ge 0 then begin
     for_changeref,GridPramsStruct.ThPos,StokesStruct.Q,StokesStruct.U,Qprime,Uprime,type=usetype
     if strpos(strupcase(ObsPramsStruct.LineName),'Q') ge 0 then UseMap.data=Qprime
     if strpos(strupcase(ObsPramsStruct.LineName),'U') ge 0 then UseMap.data=Uprime
     if strpos(strupcase(ObsPramsStruct.LineName),'QOI') ge 0 then UseMap.data=Qprime/StokesStruct.I
     if strpos(strupcase(ObsPramsStruct.LineName),'UOI') ge 0 then UseMap.data=Uprime/StokesStruct.I
   endif
  endif else begin
   stazuse=StokesStruct.Az+thetaadd
   stazuse = stazuse mod 180.
   if strpos(strupcase(ObsPramsStruct.LineName),'AZ') ge 0 then UseMap.data=stazuse
   if strpos(strupcase(ObsPramsStruct.LineName),'Q') ge 0 or strpos(strupcase(ObsPramsStruct.LineName),'U') ge 0 then begin
     if strpos(strupcase(ObsPramsStruct.LineName),'Q') ge 0 then UseMap.data=StokesStruct.P*cos(stazuse*mdtor*2.d0)
     if strpos(strupcase(ObsPramsStruct.LineName),'U') ge 0 then UseMap.data=StokesStruct.P*sin(stazuse*mdtor*2.d0)
     if strpos(strupcase(ObsPramsStruct.LineName),'QOI') ge 0 then UseMap.data=StokesStruct.P*cos(stazuse*mdtor*2.d0)/StokesStruct.I
     if strpos(strupcase(ObsPramsStruct.LineName),'UOI') ge 0 then UseMap.data=StokesStruct.P*sin(stazuse*mdtor*2.d0)/StokesStruct.I
   endif
  endelse
 endif
endif 

if keyword_set(ObsPramsStruct) and keyword_set(GridPramsStruct) and (strpos(strupcase(ObsPramsStruct.Instrument),'OMP') ge 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' $
        or strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS') $
  then lambdatitle='Central wavelength range '+strtrim(string(min(StokesStruct.centwave(where(stokesstruct.centwave ne 0))),format="(F10.2)"),2)+' - '+strtrim(string(max(StokesStruct.centwave(where(stokesstruct.centwave ne 0))),format="(F10.2)"),2) else lambdatitle='' 

;
; if coronal polarimeter or EUV imager, put into proper units
; (note if AIA_rfilter data won't change anything)
; also will put something into BUnit if that tag not set
;

modelon=1
if strpos(UseMap.ID,'DATA') ge 0 then modelon=0
unitsuse='NULL'
if keyword_set(units) then unitsuse = units
for_fixunits,unitsuse,UseMap,ObsPramsStruct,StokesStruct=StokesStruct,model=modelon

;
; add noise
; 

nonoisedata=UseMap.data

;
; changed this so now will only add a randomly varying noise (magnitude of photon noise relative error) if 
; norandom = 0.
;

norandom=0
if tag_exist(ObsPramsStruct.NoisePrams,'NoRandom') then norandom=ObsPramsStruct.NoisePrams.NoRandom
if norandom eq 0 then begin
 numrealpts=n_elements(realpts)
 if numrealpts ne 0 then if strupcase(UseMap.CType) ne 'USER' then UseMap.data[realpts]=UseMap.data[realpts]+((randomu(seed,numrealpts,/normal)))*UseMap.Noise[realpts]*UseMap.data[realpts]
endif

;
; radial filter if set
; unless AIA DATA, which would otherwise have already been done
;

dorfilter=0

if keyword_set(rfilter) then if strupcase(ObsPramsStruct.Instrument) eq 'PHYSICAL DIAGNOSTICS' then rfilter='NULL'
if keyword_set(rfilter) then if strupcase(rfilter) eq 'NRGF_FILTER' or strupcase(rfilter) eq 'QUARTERPOWER_FILTER' $
or strupcase(rfilter) eq 'RPOWER_FILTER' or strupcase(rfilter) eq 'GAMMA_FILTER' and strupcase(UseMap.CType) eq 'DISK' then dorfilter=1

;print,'dorfilter=',dorfilter
;print,'rfilter=',rfilter
if dorfilter eq 1 then begin
       imageold=UseMap.data
       if strupcase(rfilter) eq 'NRGF_FILTER' then begin
;
; filter requies uniform dx/dy pixels
; 
        width=n_elements(UseMap.data[*,0])
        height=n_elements(UseMap.data[0,*])
        xctr=(width-1.)/2.-UseMap.xc/UseMap.dx
        yctr=(height-1.)/2.-UseMap.yc/UseMap.dy
        r0=1.d0/UseMap.dx
	if UseMap.dx eq UseMap.dy and xctr gt 0 and yctr gt 0 and xctr lt width and yctr lt height then begin
	  if min(nulldata) ne -1 then UseMap.data[nulldata]=-8888.
	  if min(underdisk) ne -1 then UseMap.data[underdisk]=-9999.
	  if min(nulldata) ne -1 then nonoisedata[nulldata]=-8888.
	  if min(underdisk) ne -1 then nonoisedata[underdisk]=-9999.
          xctruse=xctr
          yctruse=yctr
          r0use=r0
          for_nrgf,UseMap.data,xctruse,yctruse,r0use,imgflt
          UseMap.data=imgflt
          for_nrgf,nonoisedata,xctr,yctr,r0,imgflt
          nonoisedata=imgflt
        endif else begin
          UseMap.data=abs(imageold)^(.25)
          nonoisedata=abs(nonoisedata)^(.25)
          print,'NRGF filter requires uniform pixels and full Sun, doing QUARTERPOWER'
          rfilter='QUARTERPOWER_FILTER' 
        endelse
       endif else begin
	if strupcase(rfilter) eq 'RPOWER_FILTER' then begin
          UseMap.data=imageold*GridPramsStruct.RPos^rpow
          nonoisedata=nonoisedata*GridPramsStruct.RPos^rpow
	endif
	if strupcase(rfilter) eq 'QUARTERPOWER_FILTER' then begin
          UseMap.data=abs(imageold)^(.25)
          nonoisedata=abs(nonoisedata)^(.25)
	endif 
	if strupcase(rfilter) eq 'GAMMA_FILTER' then begin
	  if min(nulldata) ne -1 then imageold[nulldata]=sqrt(-1.)
	  if min(underdisk) ne -1 then imageold[underdisk]=sqrt(-1.)
	  if min(nulldata) ne -1 then nonoisedata[nulldata]=sqrt(-1.)
	  if min(underdisk) ne -1 then nonoisedata[underdisk]=sqrt(-1.)
 	  if modelon eq 0 then begin
	   display_factor =  1d6
 	   disp_min=-2d-9
 	   disp_max=1d-6
;
; check for early data where this doesn't work
;
	   test=where(finite(nonoisedata) eq 1)
 	   test_display_factor = (1d-2/median(abs(nonoisedata[test])))
	   if test_display_factor lt 1d4 then begin
	    display_factor=test_display_factor
; 	    disp_min=min(nonoisedata)
; 	    disp_max=max(nonoisedata)
	    disp_min=0.
	    disp_max=1./test_display_factor
	   endif
 	  endif else begin
	   test=where(finite(nonoisedata) eq 1)
 	   display_factor = (1d-2/median(abs(nonoisedata[test])))
 	   disp_min=min(nonoisedata)
 	   disp_max=max(nonoisedata)
	  endelse
	  scaled_image = double(bytscl((display_factor * imageold) ^ dispexp, $
            min=display_factor * disp_min, $
            max=display_factor * disp_max,/nan))
          UseMap.data=scaled_image
	  nonoisedata = double(bytscl((display_factor * nonoisedata) ^ dispexp, $
            min=display_factor * disp_min, $
            max=display_factor * disp_max,/nan))
	endif 
       endelse
endif

if min(nulldata) ne -1 then UseMap.data[nulldata]=0.
if min(underdisk) ne -1 then UseMap.data[underdisk]=0.
if min(nulldata) ne -1 then nonoisedata[nulldata]=0.
if min(underdisk) ne -1 then nonoisedata[underdisk]=0.

;
; Determine intensity minimum, maximum
; using nonoisedata!
; 

;
; for now a kluge to allow plots of Blos
; comment if not using; eventually make permanent
;
;Blosnorm=sqrt(!dpi)/2.d0
;
; linewidth in units nm - game

;game=StokesStruct.I
;if min(underdisk) ne -1 then game[underdisk]=0.
;if min(nulldata) ne -1 then game[nulldata]=0.
;test=where(game ne 0.)
;if min(test) ne -1 then begin
;  game[test]=game[test]/StokesStruct.CentI[test]
;endif else begin
;  game=game/StokesStruct.CentI
;endelse
;game=game/10.
;print,minmax(game*sqrt(!dpi))
;zeesens=8.1e-6
;Blosnorm=Blosnorm*game/zeesens
;UseMap.data=Blosnorm*UseMap.data
;nonoisedata=Blosnorm*nonoisedata
;UseMap.BUnit='Gauss'
;UseMap.ID=UseMap.ID+'converted to Blos'
;
;
; comment to here
;

if min(realpts) ne -1 then realdata=nonoisedata[realpts]

if min(realdata) eq max(realdata) then begin
	if keyword_set(nowidgmess) then message,/info, 'uniform data' else d=dialog(/WARNING,'uniform data')
	goto,label3
endif

;
; turn off plotlog if negative data
;

if min(realdata) le 0. and plotlog ne 0. then begin
	 message,/info,'Negative or zero values: turning off log scale'
	 plotlog=0.
endif

if plotlog ne 0. then begin
;
;  if usecolor eq 42 or 41 then message,/info,'Shouldnt use red/blue color table for log plots'
;
  if datatype(imax) eq 'STR' then $
	if imax eq 'scaled to data' then imax=max(alog10(realdata))
  if datatype (imin) eq 'STR' then $ 
	if imin eq 'scaled to data' then imin=min(alog10(realdata))

;
; put in fix if choice of imin, imax is going to lead to nothing being plotted
; 

  if imin gt max(alog10(realdata)) or imax lt min(alog10(realdata)) then begin
;
;
;   if strpos(strupcase(UseMap.ID),'POS') le 0 then begin
    if keyword_set(nowidgmess) then message,/info,'imin,imax out of range of data, resetting' else d=dialog(/WARNING,'imin,imax out of range of data, resetting')
;   endif
   imin=min(alog10(realdata))
   imax=max(alog10(realdata))
  endif

;
; put in fix if imin, imax dominated by glitch in data
; - may uncomment in future if needed
;
;
;  if strpos(UseMap.ID,'DATA') ge 0 then begin
;   testtop=mean(alog10(realdata))+3.*stddev(alog10(realdata))
;   if imax gt testtop then imax=testtop
;   testbot=mean(alog10(realdata))-3.*stddev(alog10(realdata))
;   if imin lt testbot then imin=testbot
;  endif

endif else begin
;
  if datatype (imax) eq 'STR' then $
	if imax eq 'scaled to data' then imax=max(realdata)
  if datatype (imin) eq 'STR' then $
	if imin eq 'scaled to data' then imin=min(realdata)
;
; for red-blue 42 or 41 (blue-red) color table 
; We need to make symmetric about 0 so it corresponds to white
; if scaling to data
;
  if (usecolor eq 42 or usecolor eq 41) and imax eq max(realdata) and imin eq min(realdata) then $
	if -1.*imin gt imax then imax=-imin else imin = -imax
;
; put in fix if choice of imin, imax is going to lead to nothing being plotted
; 

  if imin gt max((realdata)) or imax lt min((realdata)) then begin
;   if strpos(strupcase(UseMap.ID),'POS') le 0 then begin
    if keyword_set(nowidgmess) then message,/info,'imin,imax out of range of data, resetting' else d=dialog(/WARNING,'imin,imax out of range of data, resetting')
;   endif
   imin=min((realdata))
   imax=max((realdata))
  endif

;  if strpos(UseMap.ID,'DATA') ge 0 then begin
;   testtop=mean((realdata))+3.*stddev((realdata))
;   if imax gt testtop then imax=testtop
;   testbot=mean((realdata))-3.*stddev((realdata))
;   if imin lt testbot then imin=testbot
;  endif

endelse

;print,'imin=',imin
;print,'imax=',imax

;
; Set up contour levels and colors
;

imax=double(imax)
imin=double(imin)
if nlev eq 1 then $
 clevels=(imax-imin)/2.+imin $
 else clevels=indgen(nlev)*(imax-imin)/(nlev-1)+imin

if nocontcolor eq 1 then ccolors=[1,1,1,1,1,1,1,1]
if nocontcolor ne 1 then ccolors=[1,2,3,4,5,6,7,8]

ncont=size(UseMap.data)
ncontx=ncont[1]
nconty=ncont[2]

;
; allow for Azimuth radial color table running +/-90
;

if keyword_set(ObsPramsStruct) and keyword_set(GridPramsStruct) and (strpos(strupcase(ObsPramsStruct.Instrument),'OMP') ge 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' $
;        or strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' $
       ) and strpos(strupcase(ObsPramsStruct.LineName),'AZ') ge 0 and strupcase(ObsPramsStruct.RotAz) eq 'RADIAL' and (usecolor eq 42 or usecolor eq 41 or usecolor eq 4 or usecolor eq 66) then begin 
  test=where(usemap.data ge 90.) 
  if min(test) ne -1 then usemap.data[test]=usemap.data[test] - 180.
endif

;
;  MAKE PLOTS
;

if title eq '' then begin
	title=UseMap.ID+addtitle
;        if plotlog ne 0 then title=title + '!c LOG'
        if keyword_set(rfilter) then if (strupcase(rfilter) ne 'NULL' and strupcase(rfilter) ne 'NO_FILTER' and strupcase(rfilter) ne 'NOFILTER') and strupcase(UseMap.CType) eq 'DISK' then title=title + '!c '+strupcase(rfilter)

   if keyword_set(ObsPramsStruct) and modelon then begin
     postitle=''
     if abs(ObsPramsStruct.Pos) gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'PHYSICAL DIAGNOSTICS' then begin
      if abs(ObsPramsStruct.Pos) eq 2 then $
          postitle='!c Thomson Sphere Sky Surface' $
           else $
          postitle='!c Plane of Sky (X=0) Sky Surface' 
      if strupcase(GridPramsStruct.GridType) eq 'PLANEOFSKY' then begin
       if strupcase(string(dodisk)) eq 'NULL' then begin
         postitle=postitle+' Plot !c Occult at '+strtrim(string(occult),2)+' Rsun'
       endif else begin
        if dodisk eq 0 then begin
         postitle=postitle+' Plot !c Occult at '+strtrim(string(occult),2)+' Rsun'
        endif else begin
          postitle=postitle+' Plot !c Disk at '+strtrim(string(dodisk),2)+' Rsun'
        endelse
       endelse
      endif
      minlos=min(GridPramsStruct.LosOffset)
      maxlos=max(GridPramsStruct.LosOffset)
      if minlos eq maxlos and (minlos ne 0) then $
       postitle=postitle+' LosOffset = '+strtrim(string(minlos),2)+' Rsun'
     endif
     title=title+postitle
   endif
endif

colorbartitle=strtrim(string(UseMap.BUnit),2) 
if strupcase(ObsPramsStruct.LineName) eq 'WLRAT' then colorbartitle=colorbartitle+' '+strtrim(string(ObsPramsStruct.Wavelength_Ang),2)+','+strtrim(string(ObsPramsStruct.Wavelength2_Ang),2)+' Angstroms'

if keyword_set(rfilter) then if strupcase(rfilter) ne 'NO_FILTER' and strupcase(rfilter) ne 'NOFILTER' and strupcase(rfilter) ne 'NULL' and strupcase(rfilter) ne 'AIA_RFILTER' then begin
     if strupcase(rfilter) ne 'NRGF_FILTER' $
      then colorbartitle='FILTERED: (Orig. units '+colorbartitle+')' else $
      colorbartitle='FILTERED'
endif
if plotlog ne 0 then colorbartitle=colorbartitle + ' LOG'
xtitle=xtitle+'!c'+'!c UNITS: '+colorbartitle
if keyword_set(xtitleold) eq 0 then xtitle=xtitle+addxtitle
;xtitle=xtitle+'!c'+lambdatitle

if strpos(strupcase(title),'ZZ') ge 0 then title='' 
if strpos(strupcase(xtitle),'ZZ') ge 0 then xtitle='' 
if strpos(strupcase(ytitle),'ZZ') ge 0 then ytitle=''

if strupcase(UseMap.CType) eq 'DISK' then begin

   !y.margin=[6,5]
;
; added this so nulldata and occult shows up either at true minimum of color table, ;
;  or, for color table 42-41-white/66-black (middle of imin/imax)
; (otherwise, since set to zero will be plotted at the data minimum value
;  which may be e.g. higher than the set imin)

   if min(nulldata) ne -1 then begin
    if usecolor eq 42 or usecolor eq 41 or usecolor eq 66 then UseMap.data[nulldata]=(imin+imax)/2. else UseMap.data[nulldata]=imin
    if plotlog eq 1 then UseMap.data[nulldata]=10^UseMap.data[nulldata]
   endif
   if min(underdisk) ne -1 then begin
    if usecolor eq 42 or usecolor eq 41 or usecolor eq 66 then UseMap.data[underdisk]=(imin+imax)/2. else UseMap.data[underdisk]=imin
    if plotlog eq 1 then UseMap.data[underdisk]=10^UseMap.data[underdisk]
   endif

;  Cartesian Plane of Sky Plot:
   if noclabel then c_lab=0 else c_lab=replicate(1,nlev)
   if plotlog ne 0. then begin
;               Log Plot
      if docont ne 2 then plot_map,UseMap,/log,dmin=10^imin,dmax=10^imax,title=title,bot=Bottom,$
               xtitle=xtitle,ytitle=ytitle,charsize=charsize,charthick=charthick,noerase=noerase,$
               xrange=xplotrange,yrange=yplotrange,color=axiscolor
      if docont eq 1 then begin
      		plot_map,UseMap,/log,lev=10^clevels,/over,$
                     c_lab=c_lab,c_col=ccolors,c_charsize=c_charsize,c_thick=usethick
      endif
      if docont eq 2 then plot_map,UseMap,/log,dmin=10^imin,dmax=10^imax,title=title,bot=Bottom,$
               xtitle=xtitle,ytitle=ytitle,charsize=charsize,charthick=charthick,noerase=noerase,$
               xrange=xplotrange,yrange=yplotrange,color=axiscolor,/contour,$
      		lev=10^clevels,c_lab=c_lab,c_col=ccolors,c_charsize=c_charsize,c_thick=usethick
   endif else begin
;     Linear Plot
       if docont ne 2 then plot_map,UseMap,dmin=imin,dmax=imax,title=title,bot=Bottom,$
               xtitle=xtitle, ytitle=ytitle,charsize=charsize,charthick=charthick,noerase=noerase, $
               xrange=xplotrange,yrange=yplotrange,color=axiscolor
       if docont eq 1 then begin
      		plot_map,UseMap,lev=clevels,/over, c_lab=c_lab,c_col=ccolors,c_charsize=c_charsize,c_thick=usethick
       endif
       if docont eq 2 then plot_map,UseMap,dmin=imin,dmax=imax,title=title,bot=Bottom,$
               xtitle=xtitle, ytitle=ytitle,charsize=charsize,charthick=charthick,noerase=noerase, $
               xrange=xplotrange,yrange=yplotrange,color=axiscolor,$
		lev=clevels,c_lab=c_lab,c_col=ccolors,c_charsize=c_charsize,c_thick=usethick,/contour
   endelse

   NewMap=UseMap
   if min(underdisk) ne -1 then begin
    if is_number(whitedisk) then if whitedisk ne -1 then begin
     showme=0
     if is_number(occult) then if occult gt 0 then showme=1
     if is_number(upoccult) then if upoccult gt 0 then showme=1
;     if whitedisk ne 0 or showme eq 1 then begin	
     if showme eq 1 then begin	
	usewhitedisk=whitedisk
;        if whitedisk eq 0 then usewhitedisk = 1
        NewMap.data=0.*NewMap.data
	NewMap.data[underdisk]=10.
;   	plot_map,NewMap,lev=[10.],/over,c_col=[usewhitedisk],/cell_fill,xrange=.95*xplotrange,yrange=.95*yplotrange
   	plot_map,NewMap,lev=[10.],/over,c_col=[usewhitedisk],/cell_fill,xrange=xplotrange,yrange=yplotrange
     endif
    endif
   endif
   if min(nulldata) ne -1 then begin
    if is_number(nulldatacolor) then begin
	usenulldatacolor=abs(fix(nulldatacolor))
        NewMap.data=0.*NewMap.data
	NewMap.data[nulldata]=10.
;   	plot_map,NewMap,lev=[10.],/over,c_col=[usenulldatacolor],/cell_fill,xrange=.98*xplotrange
   	plot_map,NewMap,lev=[10.],/over,c_col=[usenulldatacolor],/cell_fill,xrange=xplotrange
    endif
   endif
   if is_number(sunedge) && sunedge ne 0 then begin
	n=500
	thet=(findgen(n)/(n-1))*2.d0*!dpi
	circx=sin(thet)*sunedge
	circy=cos(thet)*sunedge
	colthick=5
;
; this was causing problems because
; in a gang plot psplot = 0 for all but
; the first one because of how I set it up
;  for now just commenting out
;
;	if psplot eq 1 then colthick=9
	oplot,circx,circy,thick=colthick,color=5
   endif 
   if is_number(occult) then begin
     if occult lt 0 then begin
	n=500
	thet=(findgen(n)/(n-1))*2.d0*!dpi
	circx=sin(thet)*abs(occult)
	circy=cos(thet)*abs(occult)
	colthick=3
	if psplot eq 1 then colthick=10
	oplot,circx,circy,thick=colthick,color=2,linestyle=2
    endif
   endif
   if is_number(upoccult) then begin
     if upoccult lt 0 then begin
	n=500
	thet=(findgen(n)/(n-1))*2.d0*!dpi
	circx=sin(thet)*abs(upoccult)
	circy=cos(thet)*abs(upoccult)
	colthick=3
	if psplot eq 1 then colthick=3
	oplot,circx,circy,thick=colthick,color=5,linestyle=2
    endif
   endif
 
;
; commented below because now incorporated in title
;   textcolor=1
;   if is_number(dodisk) and is_number(whitedisk) then if dodisk eq 0 then textcolor=abs(whitedisk-1) 
;   if strupcase(ObsPramsStruct.Instrument) eq 'PHYSICAL DIAGNOSTICS' then textcolor=3
;   if keyword_set(ObsPramsStruct) and modelon then begin
;;     if is_number(ObsPramsStruct.Pos) eq 0 or is_number(dodisk) eq 0 then stop
;;     if ObsPramsStruct.Pos gt 0 and strupcase(ObsPramsStruct.Instrument) ne 'PHYSICAL DIAGNOSTICS' then begin
;     if abs(ObsPramsStruct.Pos) gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'PHYSICAL DIAGNOSTICS' then begin
;      if abs(ObsPramsStruct.Pos) eq 2 then $
;          title='Thomson Sphere !c Sky Surface' $
;           else $
;          title='Plane of Sky (X=0) !c Sky Surface' 
;      if strupcase(string(dodisk)) eq 'NULL' then begin
;         if eps eq 0 then xyouts,0.45,0.5,title+' Plot !c Occult at '+strtrim(string(occult),2)+' Rsun',/normal,color=textcolor 
;      endif else begin
;       if dodisk eq 0 then begin
;        if eps eq 0 then xyouts,0.45,0.5,title+' Plot !c Occult at '+strtrim(string(occult),2)+' Rsun',/normal,color=textcolor 
;       endif else begin
;        if eps eq 0 then xyouts,0.45,0.5,title+' Plot !c Disk at '+strtrim(string(dodisk),2)+' Rsun',/normal,color=5,charthick=2,charsize=1.4
;       endelse
;      endelse
;     endif
;   endif

;   if keyword_set(ObsPramsStruct) and modelon then if strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' then xyouts,.4,0.45,'CAUTION !c UV Spectropolarimetry !c BEING TESTED',/normal,color=textcolor
   
;   if colortable eq 1 and keyword_set(noerase) eq 0 then begin
;
; changed this since sometimes we want color bar even
; in multi plot. This means if it is a multi plot, colortable=0
; required if want only one bar.  Will change default based on 
; noerase.
;
   if colortable eq 1 then begin
     if !p.multi[2] le 1 and !p.multi[1] le 1 then begin
      if psplot eq 0 then begin
        default,cpos,[0.87,0.05,0.9,0.95]
      endif else begin
        default,cpos,[0.88,0.15,0.91,0.85]
      endelse
     endif else begin
      if !p.multi[1] gt 1 then default,cpos,[1./!p.multi[1],0.05,1.03/!p.multi[1],0.95]
;
; may need to tweak for eps...
     endelse
      if strupcase(UseMap.BUnit) eq 'DEGREES' then  for_colorbar,bot=Bottom,/right,/vertical,minrange=imin,maxrange=imax,format='(f9.2)',position=cpos,color=axiscolor,charsize=charsize $
      else for_colorbar,bot=Bottom,/right,/vertical,minrange=imin,maxrange=imax,format='(g9.2)',position=cpos,color=axiscolor,charsize=charsize
   endif
endif else begin
   !y.margin=[6,5]
; x.margin constrained by aspect ratio in image_plot
;  Carrington Plot:
   if min(nulldata) ne -1 then begin
    if usecolor eq 42 or usecolor eq 41 or usecolor eq 66 then UseMap.data[nulldata]=(imin+imax)/2. else UseMap.data[nulldata]=imin
   endif
   if plotlog eq 1 then tmp=alog10(UseMap.data)
   if plotlog eq 0. then tmp=UseMap.data
  
    if docont ne 2 then plot_image,tmp,scale=[UseMap.dph,UseMap.dth],$
              origin=[UseMap.ph0+180.+UseMap.dph/2.,UseMap.th0+UseMap.dth/2.],$
              max=imax,min=imin,bot=Bottom,$
              ytitle=ytitle, xtitle=xtitle,title=title,/xsty,/ysty,$
              xtickint=90.,ytickint=45.,xminor=3,yminor=3,$
	      yr=[180.,360.],ytickname=['180','135','90','45','0'],$
	      xr=[180.+(UseMap.ph0),540.+(UseMap.ph0)],$
              charsize=charsize,charthick=charthick,noerase=noerase,color=axiscolor
   if docont ne 0 then begin
      x=findgen(ncontx)*UseMap.dph+UseMap.ph0+180. 
      y=findgen(nconty)*UseMap.dth+UseMap.th0
      if noclabel then c_lab='' else c_lab=replicate(1,nlev)
      if docont eq 1 then contour,tmp,x,y,/xsty,/ysty,/over,levels=clevels,$
              c_lab=c_lab,c_col=ccolors,c_charsize=c_charsize
      if docont eq 2 then contour,tmp,x,y,/xsty,/ysty,levels=clevels,$
              c_lab=c_lab,c_col=ccolors,c_charsize=c_charsize,/over
   endif
   if min(nulldata) ne -1 then begin
    if is_number(nulldatacolor) then begin
	usenulldatacolor=abs(fix(nulldatacolor))
        newtmp=0.*tmp
	newtmp[nulldata]=10.
        x=findgen(ncontx)*UseMap.dph+UseMap.ph0+180. 
        y=findgen(nconty)*UseMap.dth+UseMap.th0
        contour,newtmp,x,y,/xsty,/ysty,levels=[10.],$
              c_col=[usenulldatacolor],/cell_fill,/over
    endif
   endif
   plots,[UseMap.ph0+270.,UseMap.ph0+270.],[UseMap.th0,UseMap.th0+180.],lines=2,color=3
   plots,[UseMap.ph0+450.,UseMap.ph0+450.],[UseMap.th0,UseMap.th0+180.],lines=2,color=3

   if colortable eq 1 and keyword_set(noerase) eq 0 then begin
     if !p.multi[2] le 1 and !p.multi[1] le 1 then begin
      default,cpos,[0.04,0.05,0.07,0.95]
     endif else begin
      if !p.multi[1] gt 1 then default,cpos,[1./!p.multi[1],0.05,1.03/!p.multi[1],0.95]
     endelse
      for_colorbar,bot=Bottom,color=axiscolor,/right,/vertical,minrange=imin,maxrange=imax,format='(g9.2)',position=cpos,charsize=charsize
   endif
endelse

label3:
if userdevice eq 1 then begin
 print,'changing back to device,decomposed=1' 
 device,decomposed = 1
endif

if keyword_set(titleold) then title=titleold else title=''
if keyword_set(xtitleold) then xtitle=xtitleold else xtitle=''
if keyword_set(ytitleold) then ytitle=ytitleold else ytitle=''

end

