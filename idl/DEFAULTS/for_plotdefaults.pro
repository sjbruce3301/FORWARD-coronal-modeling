pro for_plotdefaults,magmod,modelname,gridtype=gridtype,dodisk=dodisk,noerase=noerase,instrument=instrument,line=line,donoise=donoise,$
        rotaz=rotaz,pos=pos,colortable=colortable,plotlog=plotlog,imax=imax,imin=imin,usecolor=usecolor,axiscolor=axiscolor,bgcolor=bgcolor,$
        docont=docont,nocontcolor=nocontcolor,noclabel=noclabel,nlev=nlev,c_charsize=c_charsize,$
        winnumber=winnumber,nwinx=nwinx,nwiny=nwiny,$
        nulldatacolor=nulldatacolor,sunedge=sunedge,whitedisk=whitedisk,$
        charsize=charsize,charthick=charthick,xtitle=xtitle,ytitle=ytitle,title=title,$
        fieldlines=fieldlines,bscale=bscale,bcolor=bcolor,bthick=bthick,bminuse=bminuse,narrow=narrow,nstarrow=nstarrow,$
        pscale=pscale,stklines=stklines,stkcolor=stkcolor,stkthick=stkthick,sminuse=sminuse,units=units,$
        arrowave=arrowave,azequi=azequi,distobs=distobs,$
	dispgamma=dispgamma,dispexp=dispexp,$
	rpow=rpow,nobangletitle=nobangletitle,addturbtitle=addturbtitle,$
	PlotInputs=PlotInputs

  
common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops


;
; this program sets defaults for the forward calculation plotting
;
;  Inputs 
;	MAGMOD, MODELNAME defined in FOR_MODELDEFAULTS
;	GRIDTYPE defined in FOR_GRIDDEFAULTS
;	DODISK defined in FOR_LOSDEFAULTS
;	INSTR, LINE,DONOISE defined in FOR_OBSDEFAULTS
;
;   Keywords defined below
;
;  Optional output keyword PlotInputs gathers all keyword inputs
;       useful for widget setup
;
; Called by FOR_DRIVE, FOR_WIDGET_EVENT, FOR_WIDGET_OUTPUT_EVENT, FOR_WIDGET_PLOT_EVENT, FOR_WIDGET
;
;  Written 2013 Sarah Gibson
;  Version 2.0 July 2014
;
;  Feb 2016 -- Fixes for LOSEM, COLDEN; BSCALE, WHITEDISK -SEG
;  March 2016 - fixed bug where POS could be a non-string
;  Feb 2017 - made default EXPFAC color table 42
;  July 2017 - replaced AZ with STOKESAZ checks; also moved
; 	defaults for AZ out of the forcecolor-linerun conditional;
;	since more universal than that (was doing strange things from command line)
; August 2017 -- moved AZ lines below imin/imax default setting 
;	(note linerun is test for line command AND imin/imax/plotlog not set)
; Jan 2018: included check for instrument NONE as
;      well as WL - SEG
;  June 2018: added benergy, etc
;  July 2018: 
;   changed defaults for UV Stokes AZ
;    moved AZ imin imax back into forcecolor-linerun conditional, but outside POS
;    Also reordered/combined colortable, usecolor,units, plotlog and imin/imax for clarity
;    removed linerun condition for EXPFAC forced color=42
;    and added some comments throughout
;  August 2018:
;    changed usecolor default for UV Az to 42 and imin/imax to 80/100
;    and fieldlines default bcolor for 0/1 depending on bgcolor 1/0
; September 2018: AZ imin/imax seemed to be outside forcecolor(despite comment on July 2018
;  so restored forcing to defaults for forcecolor and linerun set
;  January 2019
;   Made change to for_plot to only do the +/- 90 for IR not UV
;   And so here changed default usecolor for UV to 6
;  May 2019
;   changed units of UV
;  July 2020
;   set up for KCOR scaling, added new variables dispexp and dispgamma
;  November 2020-- made XPOLF, XPOLB, XPOLG 41
;		also Vx (for now maintaining DOPPLER 42, which is spectroscopic convention of positive=away)
;			both should have red shift going away from observer
;		also changed plotlog=0 to default not force for 41, 42, various vector quantities
; January 2021 -- added RPOW for RPOWER_FILTER option
; August 2021 -- forcecolor/linerun + CARRMAP resets imin/imax scaled to data and plotlog=0
; 	removed KCOR imin/imax plotlog forcing
; September 2021 -- made adjustments to account for magmod=-1, -2 for DATA
;	adjusted check for instrument='NONE' to instr and line='NONE'
;	expanded conditional test for STOKESQOI etc
; December 2021 -- removed STOKESQ, U plot limits +/- 1
; January 2022 -- added TPOLF, TPOLB, TPOLG angular distance from TS functionality
; February 2022 -- changed default color scale for TPOLB, TPOLF to greyscale
; March 2022 -- changed TPOLF and TPOLB to TPOLU and TPOLS for unsigned and signed
;	at the moment, I don't have a way to determine TPOLS but in future I may, so it is a placeholder
;  	also made TPOLU grey scale default
; March 2022 -- allowed StkLines to be displayed for CoMP data (abs(magmod))
; April 2022 passed through azequi
; May 2022 passed through distobs
; Jun 2022-  added keyword nobangletitle
; Oct 2022- added keyword addturbtitle
; Nov 2022- fixed bug where it was only checking POS=1 (added posuse to avoid
;		possible problem with string)
;	    Also changed rpow default from 3 to 2
; Dec 2022 - added NEVIII
;		also made default for P linear and updated units definition (P=PB/TB)
; Feb 2023- changed instr to instrument
;	removed mapstart from forcecolor
; Mar 2023 -- set imin/imax to 'scaled to data' for all but CoMP
; Aug 2023-- changed imin imax defaults for Dopplervlos and linewidth
; Jan 2025 -- added OPEN topology option
; Feb 2024 -- added MGIX
;	changed default linewidth for DATA to 54, 74 - for UCoMP -- CoMP won't look good though
;	cross-checked double to be consistent with VAL
; Mar 2024 -- changed range on LoI to -2 instead of -2.4 to be consistent with UCoMP
;	created forcecolor_not_rfilter to allow for docont not changing with rfilter
;       Jun 2024 -- fixed BEN_ check; checked adding BPOS column variables covered
;	Jul 2024 -- replaced Bsun with Bsun_center  
;        -- also removed PPM as option for WL as ambiguous
;               and replace with Bsun_mean
; Jul 2024 -- added PR as white light diagnostic
; Aug 2024 -- added hooks for WLRAT, cleaned up P, PR color

if keyword_set(azequi) then sunedge_use=atan(1./distobs)/!dtor  else sunedge_use=1.d00


;if exist(whitedisk) then print,whitedisk
;if exist(dodisk) then print,dodisk

datastart=flag.dta
modelstart=flag.mdl
mapstart=flag.rmp
obsstart=flag.obs
noisestart=flag.noise

;
; forcecolor is a way to determine if the defaults need to be reset if 
;  a change has been made to model, observation type, etc.
;  the flags are set in the widget programs.
;  once they are checked, the flags will be unset -- in most cases this happens
;  in the widget codes (note however flag.noise is reset below)
;

forcecolor=0
forcecolor_not_rfilter=0
;
; this unnecessarily overwrites map plot defaults
;
;if datastart eq 1 or datastart eq 2 or modelstart eq 1 or mapstart eq 1 or obsstart eq 1 or obsstart eq 2 or keyword_set(noisestart) then forcecolor=1
if datastart eq 1 or datastart eq 2 or modelstart eq 1 or obsstart eq 1 or obsstart eq 2 or keyword_set(noisestart) then forcecolor=1
if datastart eq 1 or datastart eq 2 or modelstart eq 1 or obsstart eq 1 or keyword_set(noisestart) then forcecolor_not_rfilter=1

flag.noise=0

;
;   inputs to FOR_PLOT
;
;	NOTE - these are all essentially repeated in FOR_PLOTFITS and 
;		PLOT_COMP_QUICKINV

;
;   First determine if called from line command as opposed to widget
;    AND imin/imax/plotlog not set - if so, will force defaults for those
;

linerun=0
if n_elements(imin) eq 0 and n_elements(imax) eq 0 and n_elements(plotlog) eq 0 then linerun=1

;
; UNITS
; 
;  Intensities for EUV/XRAY imagers and polarimeters can be changed via keyword UNITS
;  Implemented through FOR_FIXUNITS, called in FOR_PLOTS
;  Also possible to apply FOR_FIXUNITS directly to a FORWARD map to change
;  units within the map itself (map tag BUnit will reflect the change)
;

UnitsVal='nodisplay'
dostoke=0
if strpos(strupcase(instrument),'OMP') ge 0 or strpos(strupcase(instrument),'CORMAG') ge 0 $
 or strpos(strupcase(instrument),'OVI') ge 0 or strupcase(instrument) eq 'LYA' $
 or strpos(strupcase(instrument),'NEVIII') ge 0 or strpos(strupcase(instrument),'MGIX') ge 0 $
 or strupcase(instrument) eq 'WL' or (strupcase(instrument) eq 'NONE' and strupcase(line) eq 'NONE') $
 then begin
  if strupcase(instrument) eq 'WL' or (strupcase(instrument) eq 'NONE' and strupcase(line) eq 'NONE') then begin
	if strupcase(line) eq 'P' or strupcase(line) eq 'XPOLF' $
		or strupcase(line) eq 'XPOLB' or strupcase(line) eq 'XPOLG'  $
		or strupcase(line) eq 'PR' $
		or strupcase(line) eq 'WLRAT' $
	        or strupcase(line) eq 'TPOLS' $
		or strupcase(line) eq 'TPOLU' $
		or strupcase(line) eq 'TPOLG' then begin
          if strupcase(line) eq 'P' or strupcase(line) eq 'PR' or strupcase(line) eq 'WLRAT' then begin
  	   if strupcase(line) eq 'P' then units='Fraction Polarized (pB/TB)'
  	   if strupcase(line) eq 'PR' then units='Polarization Ratio'
  	   if strupcase(line) eq 'WLRAT' then units='Ratio WL two wavelengths'
           default,usecolor,0
	  endif else begin
	    if strupcase(line) eq 'TPOLU' then default,usecolor,0
	    default,usecolor,41
	    if strpos(line,'TP') lt 0 then units='Solar Radii' else units='Radians'
	  endelse 
          UnitsVal='nodisplay'
        endif else begin
	 UnitsVal=['10^-8 Bsun_center','10^-8 Bsun_mean'] 
         default,units,'10^-8 Bsun_center'
         if strupcase(units) ne '10^-8 BSUN_CENTER' and strupcase(units) ne '10^-8 BSUN_MEAN' then units='10^-8 Bsun_center'
        endelse
  endif else begin
   dostoke=1
   default,units,'NULL'
   if strpos(strupcase(line),'OI') lt 0 then begin
    if strpos(strupcase(line),'DOPPLERVLOS') lt 0 and strpos(strupcase(line),'LINEWIDTH') lt 0 and strpos(strupcase(line),'AZ') lt 0 then begin 
	UnitsVal=['PPM','ERG/CM2/S/SR','ERG/CM2/S/ARCSEC2','PHOTONS'] 
        if strpos(strupcase(instrument),'OVI') ge 0 or strupcase(instrument) eq 'LYA' or strpos(strupcase(instrument),'NEVIII') ge 0 or strpos(strupcase(instrument),'MGIX') ge 0 then begin
          default,units,'ERG/CM2/S/ARCSEC2'
    	  if strupcase(units) ne 'PPM' and strupcase(units) ne 'ERG/CM2/S/SR' and strupcase(units) ne 'ERG/CM2/S/ARCSEC2' and strupcase(units) ne 'PHOTONS' then units='ERG/CM2/S/ARCSEC2'
	endif else begin
          default,units,'PPM'
    	  if strupcase(units) ne 'PPM' and strupcase(units) ne 'ERG/CM2/S/SR' and strupcase(units) ne 'ERG/CM2/S/ARCSEC2' and strupcase(units) ne 'PHOTONS' then units='PPM'
        endelse
;
; we used to not have physical units of UV spectropolarimetric intensities
;  now they are ok -- but PPM will be sensitive to our choice of parameter CHROMORAD
;   other units (erg/cm2/s/sr or erg/cm2/s/arcsec2 or photons) should not care
;
;        if strpos(strupcase(instrument),'OVI') ge 0 or strupcase(instrument) eq 'LYA' then begin
;        if strpos(strupcase(instrument),'OVI') ge 0 or strpos(strupcase(instrument),'NEVIII') ge 0 then begin
;	  units='ERG/CM2/S/SR'
;	  UnitsVal='nodisplay'
; 	endif
;
; until we figure out a way to appropriately model equivalent width in CoMP data
; only PPM is sensible for I, Q, U, (V) data
;
; and note CorMag data is PPM
;
        if strupcase(modelname) eq 'DATA' then begin
          UnitsVal='nodisplay'
          if strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG' $
;
; no uv data yet
;          or strpos(strupcase(instrument),'OVI') ge 0 or strpos(strupcase(instrument),'NEVIII') ge 0 $
;          or strupcase(instrument) eq 'LYA' $
		then Units='PPM' else Units='DN'
        endif       
    endif else begin 
      units='KM/SEC'
      if strpos(strupcase(line),'AZ') ge 0 then units='DEGREES'
    endelse
   endif else units='FRACTION INTENSITY'
  endelse
endif else begin
  if strupcase(instrument) eq 'AIA' or strupcase(instrument) eq 'EIT' or strupcase(instrument) eq 'SWAP' or strupcase(instrument) eq 'XRT' or strpos(strupcase(instrument),'EUVI') ge 0 or strupcase(instrument) eq 'TRACE' then begin
        default,units,'DN/S/DETPIX'
        if strupcase(units) ne 'DN/S/DETPIX' and strupcase(units) ne 'DN/S/MAPPIX' then units='DN/S/DETPIX'
	if flag.rfil eq 1 then UnitsVal='nodisplay' else UnitsVal=['DN/S/DETPIX','DN/S/MAPPIX']
  endif else units='NULL' 
endelse

;
;       keyword USECOLOR (integer) - color table value 
;		default=0 = greyscale 
;		Then standards like 3 (heatscale)
;		- can also use red/blue (42) for
;               positive/negative data; warning dont do plotlog if so!  
;		(or blue/red (41))
;		Also 7777 for AIA color scaling (will choose based on value of keyword LINE)
;		If 7777 listed for non-AIA, will do AIA 193 color
;		Also polarimetry azimuth - 6 (0 - 180) and 66 (-90 to 90) or for UV, 66 (0 - 180) -- unless set to color table 41 or 42 then (80 - 100)
;

if strpos(strupcase(instrument),'COMP') ge 0 or strupcase(instrument) eq 'RADIO' or $
          strpos(strupcase(instrument),'OVI') ge 0 or strpos(strupcase(instrument),'NEVIII') ge 0 or strpos(strupcase(instrument),'MGIX') ge 0 $
          or strupcase(instrument) eq 'LYA' or $
          strupcase(instrument) eq 'WL' or (strupcase(instrument) eq 'NONE' and strupcase(line) eq 'NONE') or $
	strupcase(instrument) eq 'FARADAY' or strupcase(instrument) eq 'CORMAG' then begin
  if strupcase(line) eq 'STOKESI' and strupcase(instrument) eq 'CORMAG' then if forcecolor then usecolor=8 else default,usecolor,8
  if strupcase(line) eq 'STOKESI' and (strpos(strupcase(instrument),'OVI') ge 0 $
     or strpos(strupcase(instrument),'NEVIII') ge 0 or strpos(strupcase(instrument),'MGIX') ge 0 $
     or strupcase(instrument) eq 'LYA') then if forcecolor then usecolor=0 else default,usecolor,0
  if strupcase(line) eq 'STOKESI' and strupcase(instrument) eq 'RADIO' then if forcecolor then usecolor=3 else default,usecolor,3
  if strupcase(line) eq 'XPOLF' or strupcase(line) eq 'XPOLB' or strupcase(line) eq 'XPOLG' $ 
    or strupcase(line) eq 'TPOLG' or strupcase(line) eq 'TPOLS' $
      then if forcecolor then usecolor=41 else default,usecolor,41
  if strupcase(line) eq 'FR' or strupcase(line) eq 'ROTATION (ANGLE)' then if forcecolor then usecolor=42 else default,usecolor,42
  if strupcase(line) eq 'RM' or strupcase(line) eq 'ROTATION MEASURE' then if forcecolor then usecolor=42 else default,usecolor,42
  if strupcase(line) eq 'STOKESV' then if forcecolor then usecolor=42 else default,usecolor,42
  if strpos(strupcase(line), 'VOI') ge 0 then if forcecolor then usecolor=42 else default,usecolor,42
  if strupcase(line) eq 'DOPPLERVLOS' then if forcecolor then usecolor=42 else default,usecolor,42
  if strupcase(line) eq 'STOKESQ' then if forcecolor then usecolor=42 else default,usecolor,42
  if strupcase(line) eq 'STOKESU' then if forcecolor then usecolor=42 else default,usecolor,42
  if strpos(strupcase(line), 'QOI') ge 0 then if forcecolor then usecolor=42 else default,usecolor,42
  if strpos(strupcase(line), 'UOI') ge 0 then if forcecolor then usecolor=42 else default,usecolor,42
  if strupcase(line) eq 'LINEWIDTH' then if forcecolor then usecolor=4 else default,usecolor,4
  if strpos(strupcase(line),'AZ') ge 0 then begin
   if keyword_set(rotaz) then begin
    if strupcase(rotaz) eq 'RADIAL' then begin
     if (strpos(strupcase(instrument),'OVI') ge 0 $
       or strpos(strupcase(instrument),'NEVIII') ge 0 $
        or strpos(strupcase(instrument),'MGIX') ge 0 or strupcase(instrument) eq 'LYA') then begin
      if forcecolor then usecolor=42 else default,usecolor,42
     endif else begin
      if forcecolor then usecolor=66 else default,usecolor,66
     endelse
    endif else if forcecolor then usecolor=4 else default,usecolor,4
   endif else begin
     if (strpos(strupcase(instrument),'OVI') ge 0 $ 
       or strpos(strupcase(instrument),'NEVIII') ge 0 $
       or strpos(strupcase(instrument),'MGIX') ge 0 $
       or strupcase(instrument) eq 'LYA') then begin
      if forcecolor then usecolor=42 else default,usecolor,42
     endif else begin
      if forcecolor then usecolor=66 else default,usecolor,66
     endelse
   endelse
  endif
  if strpos(strupcase(line),'VOI') lt 0 $
	and strpos(strupcase(line),'QOI') lt 0 $
	and strpos(strupcase(line),'UOI') lt 0 $
	and strupcase(line) ne 'STOKESV' $
	and strupcase(line) ne 'STOKESQ' $
	and strupcase(line) ne 'STOKESU' and strupcase(line) ne 'STOKESI' $
	and strupcase(line) ne 'DOPPLERVLOS' $
	and strupcase(line) ne 'LINEWIDTH' and strpos(strupcase(line),'AZ') lt 0 $
	and strupcase(line) ne 'FR' and strupcase(line) ne 'RM' $
	and strupcase(line) ne 'ROTATION MEASURE' and strupcase(line) ne 'ROTATION (ANGLE)' $
	and strupcase(line) ne 'XPOLF' and strupcase(line) ne 'XPOLB' and strupcase(line) ne 'XPOLG' $
	and strupcase(line) ne 'TPOLG' and strupcase(line) ne 'TPOLS' $
	then begin
    if forcecolor then usecolor=0 else default,usecolor,0
  endif
endif
if strupcase(instrument) eq 'AIA' or ((strpos(strupcase(instrument),'COMP') ge 0 ) and strupcase(line) eq 'STOKESI' or strupcase(line) eq 'I') then if forcecolor then usecolor=7777 else default,usecolor,7777
if strupcase(instrument) ne 'AIA' and strpos(strupcase(instrument),'COMP') lt 0 $
	and strupcase(instrument) ne 'CORMAG' and strupcase(instrument) ne 'RADIO' $
        and strpos(strupcase(instrument),'OVI') lt 0 $
          and strpos(strupcase(instrument),'NEVIII') lt 0 $ 
	  and strpos(strupcase(instrument),'MGIX') lt 0 $
          and strupcase(instrument) ne 'LYA' $
	and strupcase(line) ne 'XPOLF' and strupcase(line) ne 'XPOLB' and strupcase(line) ne 'XPOLG' $
	and strupcase(line) ne 'TPOLG' and strupcase(line) ne 'TPOLS' $
	and strupcase(instrument) ne 'FARADAY' then if forcecolor then usecolor=0
if strupcase(line) eq 'VX' then if forcecolor then usecolor=41 else default,usecolor,41
if strupcase(instrument) eq 'EIT' and strpos(strupcase(line),'195') ge 0 then if forcecolor then usecolor=8 else default,usecolor,8

if strupcase(line) eq 'EXPFAC' or strupcase(line) eq 'OPEN' then begin
 if forcecolor then usecolor=42 else default,usecolor,42
endif

default,usecolor,0

;
;       keyword PLOTLOG (0/1) - plot in log scale
;

plotlogval='tog'

if forcecolor then plotlog=1

;
; these are better done in linear but could have log option (don't go negative)
;
if flag.rfil gt 0 or strupcase(line) eq 'TEMP' or strupcase(line) eq 'XPOLF' or  strupcase(line) eq 'XPOLB' or strupcase(line) eq 'XPOLG' or strupcase(line) eq 'P' $
   or strupcase(line) eq 'TPOLU' or strupcase(line) eq 'PR' or $
   strupcase(line) eq 'TPOLS' or strupcase(line) eq 'TPOLG' then begin
  default,plotlog,0
  if forcecolor then plotlog = 0
endif

;
; these go negative so shouldnt have log option at all
;
if strupcase(line) eq 'BX' or strupcase(line) eq 'BY' or strupcase(line) eq 'BZ' or strupcase(line) eq 'BTH' or strupcase(line) eq 'BPH' or strupcase(line) eq 'VX' or strupcase(line) eq 'VY' or strupcase(line) eq 'VZ' or strupcase(line) eq 'VTH' or strupcase(line) eq 'VPH' or strupcase(line) eq 'R' or strupcase(line) eq 'THMOD' or strupcase(line) eq 'THOBS' or strupcase(line) eq 'PHMOD' or strupcase(line) eq 'PHOBS' or strpos(strupcase(line),'AZ') ge 0  or strupcase(line) eq 'NINST' then begin
    default,plotlog,0
    if forcecolor then plotlog = 0
    plotlogval='nodisplay'
endif 

default,plotlog,1

;
;   	keywords DISPEXP, DISPGAMMA - exponent and gamma for image display
;		only set for rfilter=GAMMA_FILTER (flag.rfil 4)
;       if sent from KCOR, change to .7, .7
;

default,dispexp,1.d0
default,dispgamma,0.5d0
default,dispexpval,'nodisplay'
default,dispgammaval,'nodisplay'
if flag.rfil eq 4 then begin
   dispexpval='double'
   dispgammaval='double'
   if forcecolor and strupcase(instrument) eq 'KCOR' then begin
    dispexp=.7d0
    dispgamma=.7d0
   endif
endif 

;   	keyword RPOW
;		only set for rfilter=RPOWER_FILTER (flag.rfil 5)

default,rpow,2.d0
default,rpowval,'nodisplay'
if flag.rfil eq 5 then begin
   rpowval='double'
endif

;
; 	keyword NOBANGLETITLE
;		don't plot B ANGLE and CMER in plot title
;
;

default,nobangletitle,0
NoBangleTitleVal='tog'

;
; 	keyword ADDTURBTITKE
;		plot turbulence spatial resolution in plot title
;	ONLY for model TURBHY
;
;

if strupcase(modelname) eq 'TURBHY' then begin
 default,addturbtitle,1
 default,nobangletitle,1
 AddTurbTitleVal='tog'
endif else begin
 addturbtitle=0
 AddTurbTitleVal='nodisplay'
endelse
 

;
;       keywords IMAX, IMIN (strings) - max and min for intensity
;		These will default to the min and max of data in FOR_PLOT
;		For now we give them 'scaled to data' input
;	

default,imax,'scaled to data'
default,imin,'scaled to data'

if is_number(imax) eq 0 then if imax eq '' then imax='scaled to data'
if is_number(imin) eq 0 then if imin eq '' then imin='scaled to data'

if obsstart eq 3 then begin
 imin='scaled to data'
 imax='scaled to data'
endif

if strpos(strupcase(line),'AZ') ge 0 then begin
;print,'begin',imin,imax
 if (forcecolor or linerun or string(imin) eq 'scaled to data' or string(imax) eq 'scaled to data') then begin
;
; this allows user to adjust imin, imax as wished,
;  but will never scale to data
;
; note the defaults have changed for STOKESAZ RADIAL,
; new color table 66 has limits +/- 90
; but only for IR
; 
  if strpos(strupcase(instrument),'OVI') ge 0 $
     or strpos(strupcase(instrument),'NEVIII') ge 0 $
     or strpos(strupcase(instrument),'MGIX') ge 0 $
     or strupcase(instrument) eq 'LYA' then begin
   imin=0.
   imax=180.
  endif else begin
   imin=-90.
   imax=90.
  endelse
;
; but, other orientations (e.g., N-S) will still go from 0-180
;
  if keyword_set(rotaz) then begin
     if (strupcase(rotaz) ne 'RADIAL' and strupcase(rotaz) ne 'NULL') $
          then begin
      imin=0.
      imax=180.
     endif
  endif
; and can go back to 6 and 0-180 if wanted for RADIAL also
; but note if we do choose 42 will shift to range 80-100

  if usecolor eq 6 or usecolor eq 42 then begin
   if usecolor eq 42 then begin
    imin=80
    imax=100
   endif else begin
     imin=0.
     imax=180.
   endelse
  endif
 endif
;print,'end',imin,imax

endif else begin
 if (forcecolor or linerun) and strupcase(gridtype) eq 'CARRMAP' then begin
  imin='scaled to data'
  imax='scaled to data'
  plotlog=0
 endif
 if (forcecolor or linerun) and strupcase(gridtype) eq 'PLANEOFSKY' then begin
  imin='scaled to data'
  imax='scaled to data'
  if strupcase(line) eq 'EXPFAC' then begin
   imin=-2.
   imax=2.
  endif
  if strupcase(line) eq 'OPEN' then begin
   imin=-1.
   imax=1.
  endif
  if is_number(pos) then posuse=pos else posuse=0.
  if posuse eq 0. then begin
   if strpos(strupcase(instrument),'OMP') ge 0 or (strupcase(instrument) eq 'CORMAG' and strupcase(modelname) ne 'DATA') $
          or strpos(strupcase(instrument),'OVI') ge 0 $
          or strpos(strupcase(instrument),'NEVIII') ge 0 $
           or strpos(strupcase(instrument),'MGIX') ge 0 $
	   or strupcase(instrument) eq 'LYA' $
	then begin
    if strupcase(line) eq 'DOPPLERVLOS' then begin
     imin=-10.
     imax=10.
    endif
    if strupcase(line) eq 'LINEWIDTH' then begin
     if strupcase(modelname) eq 'DATA' then begin
      imin=54.
      imax=74.
     endif else begin
      imin=25.
      imax=55.
     endelse
     plotlog=0
    endif
    if strpos(strupcase(line),'LOI') ge 0 then begin
     if strpos(strupcase(instrument),'OMP') ge 0 then begin
;      imin=-2.3
      imin=-2.0
      imax=-.3
     endif else begin
      imin='scaled to data'
      imax='scaled to data'
     endelse
     plotlog=1
    endif
    if strupcase(line) eq 'STOKESL' then begin
     plotlog=0
     if strupcase(modelname) eq 'DATA' then begin
      imin=0.
      imax=1.
     endif
    endif
    if strupcase(line) eq 'STOKESQ' or strupcase(line) eq 'STOKESU' then begin
     plotlog=0
     if strupcase(modelname) eq 'DATA' then begin
;
; removing because want to see when bigger than 1
;
;      imin=-1.
;      imax=1.
     endif
    endif
    if strpos(strupcase(line), 'QOI') ge 0 then begin
     imin=-.3
     imax=.3
     plotlog=0
    endif
    if strpos(strupcase(line), 'UOI') ge 0 then begin
     imin=-.3
     imax=.3
     plotlog=0
    endif
 
    if strupcase(line) ne 'LINEWIDTH' and strupcase(line) ne 'DOPPLERVLOS' and strpos(strupcase(line), 'OI') lt 0 and strupcase(line) ne 'STOKESQ' and strupcase(line) ne 'STOKESU' and strupcase(line) ne 'STOKESL' then begin
     imin='scaled to data'
     imax='scaled to data'
    endif
   endif

   if strpos(strupcase(units),'DN/S/DETPIX') ge 0 then begin
    if strupcase(instrument) eq 'XRT' and flag.rfil eq 0 then begin
     if strupcase(line) eq 'AL-MESH' then begin
      imin=-1
      imax=3.5
      plotlog=1
     endif
     if strupcase(line) eq 'TI-POLY' then begin
      imin=-1
      imax=2.5
      plotlog=1
     endif
    endif
    if strupcase(instrument) eq 'SWAP' and flag.rfil eq 0 then begin
     if line eq 174 then begin
      imin=-1.
      imax=3.5
      plotlog=1
     endif
    endif
    if strupcase(instrument) eq 'EIT' and flag.rfil eq 0 then begin
     if line eq 171 then begin
      imin=-1.
      imax=3.5
      plotlog=1
     endif
     if line eq 195 then begin
      imin=-1.
      imax=3.5
      plotlog=1
     endif
     if line eq 284 then begin
      imin=-2.
      imax=2.3
      plotlog=1
     endif
    endif
    if strupcase(instrument) eq 'KCOR' and flag.rfil eq 4 then begin
     dispgamma=.7
     dispexp=.7
     plotlog=0
    endif
    if strupcase(instrument) eq 'AIA' and flag.rfil eq 0 then begin
     if line eq 94 then begin
      imin=-1.
      imax=2.5
      plotlog=1
     endif
     if line eq 131 then begin
      imin=-.5
      imax=3.
      plotlog=1
     endif
     if line eq 171 then begin
      imin=.5
      imax=4.
      plotlog=1
     endif
     if line eq 193 then begin
      imin=1.
      imax=4.5
      plotlog=1
     endif
     if line eq 211 then begin
      imin=.5
      imax=4.
      plotlog=1
     endif
     if line eq 335 then begin
      imin=-.5
      imax=3.
      plotlog=1
     endif
    endif
   endif
;  end not POS conditional
  endif
; end forcecolor/line command conditional
 endif
; end else not AZ
endelse

imin=string(imin)
imax=string(imax)


;
;	keyword BGCOLOR (integer) color of background
;

default,bgcolor,1

;
;	keyword AXISCOLOR (integer) color of axis
;

if bgcolor eq 1 then default,axiscolor,0 else default,axiscolor,1
;
;       keyword DOCONT (0/1/2) - add contours
;		docont = 0 no contours
;		docont = 1 contours on image
;		docont = 2 (or higher) contours but no image
;
; 	default contours for PLANEOFSKY, no contours for CARRMAP
;	   contour no image for noerase, but
;	   but no contours if doing a multiplot/noerase - can make next plot in another panel.
;
;
;       keyword COLORTABLE (0/1) - color bar showing values of image intensity
;		default on
;	     turn it off when overplotting (will otherwise overlay on original plot)
;		unless doing a multiplot - then can make the next plot in another panel.

if noerase eq 1 and flag.noerase eq 0 and !p.multi[1] le 1 and !p.multi[2] le 1 then begin
  docont=2 
  colortable=0
endif else begin
  if strupcase(line) eq 'EXPFAC' or strupcase(line) eq 'OPEN' then default,docont,0
  if strupcase(gridtype) eq 'PLANEOFSKY' and strupcase(instrument) eq 'PHYSICAL DIAGNOSTICS' then default,docont,1 else default,docont,0
  if (noerase eq 0 and flag.noerase eq 1) or (noerase eq 0 and forcecolor_not_rfilter) then begin
   colortable=1
   if strupcase(gridtype) eq 'PLANEOFSKY' and strupcase(instrument) eq 'PHYSICAL DIAGNOSTICS' then docont=1 else docont=0
   if strupcase(line) eq 'EXPFAC' or strupcase(line) eq 'OPEN' then docont=0
  endif
endelse
default,colortable,1

;print,'docont',docont
;print,'modelname',docont
;print,'forcecolor',forcecolor
;print,'datastart',datastart
;print,'modelstart',modelstart
;print,'obsstart',obsstart


;       keyord NOCONTCOLOR (0/1) - dont use color for contours 

default,nocontcolor,0

;       keyword NoCLAB  (0/1)- don't put labels on contours 

default,noclabel,0

;
;	keyword NLEV (integer) number of contour levels

default,nlev,11

;       keyword WINNUMBER (integer) - window to plot

default,winnumber,!d.window>0

;
; 	keywords NWINX/NWINY (integers) - size of window
;	Default  750
;

if strupcase(gridtype) eq 'USER' or strupcase(gridtype) eq 'USERINPUT' then begin
  nwinx=0
  nwiny=0
endif
if strupcase(gridtype) eq 'PLANEOFSKY' then begin
   if datastart then begin
     nwinx=750
     nwiny=750
   endif
   if !p.multi[1] le 1 and !p.multi[2] le 1 then begin
    if  keyword_set(nwinx) and  keyword_set(nwiny) eq 0 then nwiny=nwinx
    if  keyword_set(nwiny) and  keyword_set(nwinx) eq 0 then nwinx=nwiny
    default,nwinx,750
    default,nwiny,750
   endif else begin
    if  keyword_set(nwinx) and  keyword_set(nwiny) eq 0 then nwiny=nwinx*(double(!p.multi(2))/double(!p.multi(1)))
    if  keyword_set(nwiny) and  keyword_set(nwinx) eq 0 then nwinx=nwiny*(double(!p.multi(1))/double(!p.multi(2)))
     default,nwinx,750*!p.multi[1]
     default,nwiny,750*!p.multi[2]
   endelse

endif

if strupcase(gridtype) eq 'CARRMAP' then begin
   
;
; force 2 to 1 aspect ratio 
; take into consideration if gang plot mode
;
   if !p.multi[1] le 1 and !p.multi[2] le 1 then begin
    if  keyword_set(nwinx) and  keyword_set(nwiny) eq 0 then nwiny=nwinx/2.
    if  keyword_set(nwiny) and  keyword_set(nwinx) eq 0 then nwinx=2.*nwiny
    if keyword_set(nwinx) and keyword_set(nwiny) then begin
     scale=(double(nwinx)/720.d0)>1.d0
;    print,nwinx,nwiny,scale,(double(nwinx)/double(nwiny))
     if (double(nwinx)/double(nwiny)) ne 2. then begin   
      nwinx = 720.d0*scale
      nwiny = 360.d0*scale
;      print,'changed',nwinx,nwiny
     endif
    endif else begin
     default,nwinx,720
     default,nwiny,360
    endelse
   endif else begin
    if  keyword_set(nwinx) and  keyword_set(nwiny) eq 0 then nwiny=nwinx/2.*(double(!p.multi[2])/double(!p.multi[1]))
    if  keyword_set(nwiny) and  keyword_set(nwinx) eq 0 then nwinx=2.*nwiny*(double(!p.multi[1])/double(!p.multi[2]))
    if keyword_set(nwinx) and keyword_set(nwiny) then begin
     nwinxtest=nwinx/!p.multi[1]
     scale=(double(nwinxtest)/720.d0)>1
     if (double(nwinx)/double(nwiny))*(double(!p.multi[2])/double(!p.multi[1])) ne 2. then begin   
      nwinx = 720.d0*scale*!p.multi[1]
      nwiny = 360.d0*scale*!p.multi[2]
     endif
    endif else begin
     default,nwinx,360*!p.multi[1]
     default,nwiny,180*!p.multi[2]
    endelse
   endelse

endif
 
;
; 	keyword NULLDATACOLOR (integer)  -- nulldata color 
;               Default = '' will be central color of color bar
;		0 black; 1 is white, others are rainbow
;		if set to negative, will not plot noise with SNR < 1
;		  (default will be to plot these points black 
;			 or light blue for red/blue color table;
;			 this can be changed by changing NULLDATACOLOR
;			 to some other negative number)
;

;print,'donoise=',donoise
;print,'forcecolor',forcecolor
;if keyword_set(nulldatacolor) then print,'nulldatacolor=',nulldatacolor

if keyword_set(donoise) eq 1 then begin
  if usecolor eq 42 or usecolor eq 41 or usecolor eq 6 or usecolor eq 66 then default,nulldatacolor,-7 else default,nulldatacolor,-8
endif
if keyword_set(donoise)  eq 1 and forcecolor then begin
  if usecolor eq 42 or usecolor eq 41 or usecolor eq 6 or usecolor eq 66 then nulldatacolor=-7 else nulldatacolor=-8
endif
default,nulldatacolor,''
if forcecolor and keyword_set(donoise) ne 1 then nulldatacolor=''
;default,nulldatacolor,6

;print,'nulldatacolor=',nulldatacolor

;       keyword SUNEDGE (double)  - set to draw yellow sun outline at r=Rsun


if strpos(strupcase(instrument),'OMP') ge 0 or strpos(strupcase(instrument),'CORMAG') ge 0 $
 or strpos(strupcase(instrument),'OVI') ge 0 $
   or strpos(strupcase(instrument),'NEVIII') ge 0 $
   or strpos(strupcase(instrument),'MGIX') ge 0 $
   or strupcase(instrument) eq 'LYA' $
 or strupcase(instrument) eq 'PHYSICAL DIAGNOSTICS' or strpos(strupcase(instrument),'LOSEM') ge 0$
 or strpos(strupcase(instrument),'COLDEN') ge 0 or strpos(strupcase(instrument),'BENERGY') ge 0 or strpos(strupcase(instrument),'B_') ge 0 or strpos(strupcase(instrument),'BEN_') ge 0 or strpos(strupcase(instrument),'BB_') ge 0$
 or strupcase(instrument) eq 'WL' or (strupcase(instrument) eq 'NONE' and strupcase(line) eq 'NONE') or strupcase(instrument) eq 'RADIO' then begin
    if n_elements(sunedge) ne 0 then $    
          if strupcase(string(sunedge)) eq 'NULL' then sunedge=1.d0
    default,sunedge,sunedge_use
    if forcecolor then sunedge=sunedge_use
endif else begin
    if n_elements(sunedge) ne 0 then $    
          if strupcase(string(sunedge)) eq 'NULL' then sunedge=0.d0
    default,sunedge,0.d0
    if forcecolor then sunedge=0.d0
endelse

sunedge=double(sunedge)

;
;       keyword WHITEDISK (integer) - plots occulting disk in white/color instead of grey(black)
;

if n_elements(whitedisk) ne 0 then $    
          if strupcase(string(whitedisk)) eq 'NULL' then whitedisk=-1
;default,whitedisk,5 (yellow)
default,whitedisk,-1
if forcecolor then whitedisk=-1


;
; if not PlaneOfSky, or DoDisk set, various things are unnecessary
;

if strupcase(gridtype) ne 'PLANEOFSKY' then begin
       whitedisk='NULL'
       sunedge='NULL'
endif else begin
 if is_number(dodisk) then begin
  if dodisk ne 0 then begin
       whitedisk='NULL'
;       sunedge='NULL'
  endif
 endif
endelse

;       keyword CHARSIZE, CHARTHICK (double)

if strupcase(gridtype) eq 'PLANEOFSKY' then mult = 750.d0 else mult = 720.d0
if strupcase(modelname) eq 'DATA' then mult = 720.d0
if !p.multi(1) ne 0 then begin
  mult=mult*!p.multi(1)
endif 

charsizestart=1.2d0 
if keyword_set(charsize) eq 0 or forcecolor then $
                charsize=charsizestart;*nwinx/mult
!p.charsize=charsize
;print,charsize

if keyword_set(charthick) eq 0 then $
                if !p.charthick eq 0 then charthick=1.5d0 else charthick=!p.charthick


;	keyword C_CHARSIZE  contour label character size, default, 2/3 of plot charsize
	
default,c_charsize,2.d0*charsize/3.d0

;
;	keyword TITLE,XTITLE,YTITLE (string)
;	  will be set in FOR_PLOT
;

default,title,''
default,xtitle,''
default,ytitle,''

if noerase eq 1 and flag.noerase eq 0 and !p.multi[1] le 1 and !p.multi[2] le 1 then begin 
 title='ZZ'
 xtitle='ZZ'
 ytitle='ZZ'
endif 
if noerase eq 0 and flag.noerase eq 1 then begin 
 if title eq 'ZZ' then title=''
 if xtitle eq 'ZZ' then xtitle=''
 if ytitle eq 'ZZ' then ytitle=''
endif
;print,noerase,flag.noerase


;
;  Inputs to for_plotfieldlines
;
 
;
;  	**THIS IS ACTUALLY USED IN FOR_DRIVE -- decides
;	whether FOR_PLOTFIELDLINES runs
;
;       keyword FIELDLINES (0/1) - overplot with small fieldline vectors (plane of sky
;                       component of magnetic field.)
;
;

if n_elements(fieldlines) ne 0 then $    
          if strupcase(string(fieldlines)) eq 'NULL' then fieldlines=0
default,fieldlines,0

;
;       keyword BSCALE (double) - Scale factor for the vector overplot of fieldlines
;  	Default 1., implies scaled to data, calculated in FOR_PLOTFIELDLINES
;
 
if n_elements(bscale) ne 0 then $    
          if strupcase(string(bscale)) eq 'NULL' then bscale='scaled to data'
default,bscale,'scaled to data'
if datatype(bscale) ne 'STR' then bscale=double(bscale)

;
;       keyword BCOLOR (integer) - color of fieldlines
;	Default red unless StokesAz then white or black depending on bgcolor
;

    if strpos(strupcase(line),'AZ') lt 0 then begin 
       if n_elements(bcolor) ne 0 then $    
          if strupcase(string(bcolor)) eq 'NULL' or forcecolor then bcolor=2
       default,bcolor,2
    endif else begin
       if n_elements(bcolor) ne 0 then $    
          if strupcase(string(bcolor)) eq 'NULL' or forcecolor then begin
           if bgcolor eq 0 then bcolor=1 else bcolor=0
          endif
       if bgcolor eq 0 then default,bcolor,1 else default,bcolor,0
    endelse

;
;	keyword BTHICK (double) - thickness of vectors
;	Default 1
;

if n_elements(bthick) ne 0 then $    
          if strupcase(string(bthick)) eq 'NULL' then bthick=1.d0
  default,bthick,1.d0
  bthick=double(bthick)

;
;       keyword BMINUSE (double) - minimum field strength of plane of sky field to plot vector for
;

if n_elements(bminuse) ne 0 then $    
          if strupcase(string(bminuse)) eq 'NULL' then bminuse=0.d0
  default,bminuse,0.d0
  bminuse=double(bminuse)

;
; 	keyword NARROW (integer)
;	spacing of vectors 
; 	note number of points to skip varies with grid size, thus ngrid/nsarrow is
; 	number of points to skip.  
;

if n_elements(narrow) ne 0 then $    
          if strupcase(string(narrow)) eq 'NULL' then narrow=40
default,narrow,40

;
;  Inputs to for_stokesfieldlines
;

;
;  	**THIS IS ACTUALLY USED IN FOR_DRIVE -- decides
;	whether FOR_PLOTSTOKESLINES runs
;
;       keyword STKLINES (0/1) - overplot with linear polarization vectors
;		(subject to 180 degree ambiguity and 90 degree ambiguity
;		across Van Vleck angle)
;

if n_elements(stklines) ne 0 then $    
          if strupcase(string(stklines)) eq 'NULL' then stklines=0
default,stklines,0

;
; 	keyword NSTARROW (integer)
;	spacing of vectors 
; 	note number of points to skip varies with grid size, thus ngrid/nsarrow is
; 	number of points to skip.  
;

if n_elements(nstarrow) ne 0 then $    
          if strupcase(string(nstarrow)) eq 'NULL' then nstarrow=40
default,nstarrow,40

;
;       keyword PSCALE (double) - The scale factor for plotting the fieldline vector onto
;                the plane of sky
;               if set to zero, does fixed length lines, if less than zero,
;               scales to absolute value (but fixed length), if greater than zero
;               then field line length reflects POS strength, scaled by Pscale
;  	Default assignend in FOR_PLOTSTOKESLINES
;	This is scaled to data
;
 
if n_elements(pscale) ne 0 then $    
          if strupcase(string(pscale)) eq 'NULL' then pscale='scaled to data'
default,pscale,'scaled to data'
if datatype(pscale) ne 'STR' then pscale=double(pscale)

;
;       keyword STKCOLOR (integer) - color of vectors (default green)
;

    if strpos(strupcase(line),'AZ') lt 0 then begin 
       if n_elements(stkcolor) ne 0 then $    
          if strupcase(string(stkcolor)) eq 'NULL' or forcecolor then stkcolor=4
       default,stkcolor,4
    endif else begin
       if n_elements(stkcolor) ne 0 then $    
          if strupcase(string(stkcolor)) eq 'NULL' or forcecolor then stkcolor=3
       default,stkcolor,3
    endelse

;
;       keyword STKTHICK (double) - thickness of vectors (default 1.5)
;

if n_elements(stkthick) ne 0 then $    
          if strupcase(string(stkthick)) eq 'NULL' then stkthick=1.5d0
  default,stkthick,1.5d0
  stkthick=double(stkthick)

;
;       keyword SMINUSE (double) -- minimum magnitude of plane of sky to plot vector
;

if n_elements(sminuse) ne 0 then $    
          if strupcase(string(sminuse)) eq 'NULL' then sminuse=0.d0
  default,sminuse,0.d0
  sminuse=double(sminuse)

;
;       keyword UNITS 
;	    ('ERG/CM2/S/SR','ERG/CM2/S/ARCSEC2','PPM','PHOTONS')
;	        CORONAL POLARIMETER I, Q, U, V only
;
;	 	if set to 'ERG/CM2/S/SR', CoMP type data will 
;		be plotted as in CLE (Judge&Casini) code,
;		other choices are 'ERG/CM2/S/ARCSEC2',
;		if set to 'PPM' (default) will provide in units in
;		parts-per-million (PPM) of Sun central brightness
;		at central wavelength
;
;               NOTE -- PPM is in units relative to Bsun 
;	          (Sun central brightness), and so
;               is independent of telescope.
;               For cases ERG/CM2/S/SR and ERG/CM2/S/ARCSEC2
;               NO TELESCOPE INFORMATION will be taken into consideration.
;               However, PHOTONS will be calculated taking into
;               consideration telescope information in ObsPramsStruct.NoisePrams
; 		and having it set should open the DoNoise widget
;
;		NOTE -- for DATA, for now since we are not calculating equivalent
;		width for CoMP data we do not allow conversion of I, Q, U, V
;		from PPM for DATA
;
;		WARNING: when plotting e.g. V/I or L/I units cancel out.
;
;	    ('DN/S/DETPIX, DN/S/MAPPIX')
;		NEW: UNITS also used for EUV/XRAY imagers.
;		DN/S/DETPIX is relative to detector pixel size, so, 
;		changing resolution (e.g., NGRID, NGY) will not change
;		value of intensity of model or data plotted.
;		Every instrument will have a different detector pixel
;		size (DETPIX), and these are defined in 
;		OBSERVABLES/FOR_DETPIX_LOOKUP.PRO.
;
;		DN/S/MAPPIX is relative to map pixel size, so
;		this will change with NGRID/NGY.  However, for
;		this every similar type observations (e.g., 
;		171 Angstrom intensity in any of the EUV imagers)
;		will be the same scaling from instrument to instrument
;
;	keyword ARROWAVE (0/1)
; 		arrowave will average over skipped data in
; 		creating arrow length and direction
; 		default is  no averaging
; 		(mostly used for data plotting, e.g. by FOR_PLOTFITS and PLOTCOMP_QUICKINV)
;

if n_elements(arrowave) ne 0 then $    
          if strupcase(string(arrowave)) eq 'NULL' then arrowave=0
  default,arrowave,0

; 	must be null if magnetic fields not in model

FieldLinesVal = 'tog'

if magmod ne 1 then begin
   
   FieldLinesVal='NULL'

   fieldlines='NULL'
   bscale='NULL'
   bcolor='NULL'
   bthick='NULL'
   bminuse='NULL'
   narrow='NULL'

endif

FieldLinesInputs={FieldLines:fieldlines,FieldLinesVal:FieldLinesVal,BScale:bscale,BScaleVal:'double',BColor:bcolor,BColorVal:'integer',BThick:bthick,BThickVal:'double',BMinUse:bminuse,BMinUseVal:'double',NArrow:narrow,NArrowVal:'integer'}


if abs(magmod) eq 1 and dostoke eq 1 then begin

   StkLinesVal='tog'

endif else begin

   StkLinesVal='NULL'

   stklines='NULL'
   pscale='NULL'
   stkcolor='NULL'
   stkthick='NULL'
   sminuse='NULL'
   arrowave='NULL'
   nstarrow='NULL'

endelse

StkLinesInputs={StkLines:stklines,StkLinesVal:StkLinesVal,PScale:pscale,PScaleVal:'double',StkColor:stkcolor,StkColorVal:'integer',StkThick:stkthick,StkThickVal:'double',SMinUse:sminuse,SMinUseVal:'double',ArrowAve:arrowave,ArrowAveVal:'tog',NStarrow:nstarrow,NStarrowVal:'integer'}

NWinYVal='integer'

if strupcase(modelname) eq 'DATA' then begin
  FieldLinesInputs.FieldLinesVal='NULL'
endif
if strupcase(gridtype) eq 'CARRMAP' then begin
  FieldLinesInputs.FieldLinesVal='NULL'
  StkLinesInputs.StkLinesVal='NULL'
  NWinYVal='nodisplay'
endif

;
; items that maybe don't need to be in widget
;

NullDataColorVal='integer'
;NullDataColorVal='nodisplay'

if is_number(whitedisk) eq 0 then begin
 whitedisk=-1
 whitediskval='nodisplay'
endif else whitediskval='integer'

DoContInputs={DoCont:docont,DoContVal:['0','1','2'],NoContColor:nocontcolor,NoContColorVal:'tog',NoCLabel:noclabel,NoCLabelVal:'tog',NLev:nlev,NLevVal:'integer',C_CharSize:c_charsize,C_CharSizeVal:'double'}

  PlotInputs={PlotLog:plotlog,PlotLogVal:plotlogval,Units:units,UnitsVal:UnitsVal,IMin:imin,IMinVal:'string',IMax:imax,IMaxVal:'string',$
        WinNumber:winnumber,WinNumberVal:'nodisplay',NWinX:fix(nwinx),NWinXVal:'integer',NWinY:fix(nwiny),NWinYVal:NWinYVal,$
	RPow:rpow,RPowVal:rpowval,$
	NoBangleTitle:nobangletitle,NoBangleTitleVal:nobangletitleval,$
	AddTurbTitle:addturbtitle,AddTurbTitleVal:addturbtitleval,$
	DispExp:dispexp,DispExpVal:dispexpval,DispGamma:dispgamma,DispGammaVal:dispgammaval,$
	UseColor:usecolor,UseColorVal:'integer',AxisColor:axiscolor,AxisColorVal:'integer',BGColor:bgcolor,BGColorVAl:'integer',ColorTable:colortable,ColorTableVal:'tog',$
        NullDataColor:nulldatacolor,NullDataColorVal:NullDataColorVal,SunEdge:sunedge,SunEdgeVal:'tog',WhiteDisk:whitedisk,WhiteDiskVal:WhiteDiskVal,$
        CharSize:charsize,CharSizeVal:'double',CharThick:charthick,CharThickVal:'double',Title:title,TitleVal:'string',XTitle:xtitle,XTitleVal:'string',YTitle:ytitle,YTitleVal:'string',DoContInputs:DoContInputs,DoContInputsVal:'structure',FieldLinesInputs:FieldLinesInputs,FieldLinesInputsVal:'structure',StkLinesInputs:StkLinesInputs,StkLinesInputsVal:'structure'}

;print,imin,imax,plotlog,usecolor
;print,'datastart=',datastart
;print,'modelstart=',modelstart
;print,'mapstart=',mapstart
;print,'obsstart=',obsstart
;print,'units=',units
;print,'unitsval=',unitsval
;print,whitedisk,'out'
;print,dodisk,'out'
;print,'for_plotdefaults pos',pos
end
