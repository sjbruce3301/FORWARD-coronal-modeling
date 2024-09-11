pro for_losdefaults,modelname,gridtype,rheight,limb,instrument,line,pos,azequi,$
        losuse=losuse,dodisk=dodisk,axisym=axisym,bang=bang,incres=incres,thetao=thetao,phio=phio,losmin=losmin,losint=losint,nlos=nlos,occult=occult,upoccult=upoccult,cmer=cmer,nostretch=nostretch,$
	LosInputs=LosInputs,nowidgmess=nowidgmess

 common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops


;
; this program sets all defaults for the forward calculation observer's position and line-of-sight integration
;
;  Optional output keyword LOSInputs gathers all keyword inputs
;       useful for widget setup
;
;	Inputs:
;
;	MODELNAME - keyword determined in for_modeldefaults
; 	GRIDTYPE,RHEIGHT,LIMB - keywords determined in for_griddefaults
; 	INSTRUMENT,LINE, POS - keyword determined in for_obsdefaults
;
;   inputs to FOR_LOS_PRAMS (called by FOR_GETSTRUCTS)
;      defined below
;
;      keyword CMER - keyword CMER (double - DEGREES) --  central meridian longitude (e.g. Carrington coordinates)
;       ---> GridPramsStruct tags CMer (changed to radians)
;      keyword PHIO - keyword PHIO (double - DEGREES) --  
;               allows a rotation of
;               model structure in phi, useful for fitting data.
;               or for Carrington maps for changing numbers on X axis
;       ---> GridPramsStruct tags PHIO (changed to radians)
;
;   Called by FOR_DRIVE, FOR_WIDGET_EVENT, FOR_WIDGET_PLOT_EVENT, FOR_WIDGET
;
;   Calls routine CHECKLOSWIDGET to determine if coming from widget
;       NOTPOS, NOTCARR, NORUSER -- determined in checkloswidget
;
;  Written Sarah Gibson 2013
;  Version 2.0 July 2014
;
;  Feb 2016: Fixes for OCCULT, DODISK, LOSINT, INCRES - SEG
;  Mar 2016: Allowed CMER, DODISK to be set for USER
; Jan 2018: included check for instrument NONE as well as WL - SEG
; Dec 2018: excluded radio from high res PSI calculation -- SEG
; April 2020: commented out weird kluge for resetting losmin, losint, nlos when change in losuse -
;    instead uses check for nlos='NULL" -- used to only happen if change from Physical Diagnostics
;    but now will also happen if change in losuse -- done within for_widget_plot_event.pro
; September 2020: made occult=1 default for TOMO and CROISSANT  model
;	also turned off parameter THETAO in widget display for TOMO and STRIA
; January 2021: made occult=6 and upoccult=32 defaults for STRIA
; September 2021: changed conditional ne CARRMAP to eq PLANEOFSKY because 
;  occult/dodisk were getting messed up for USER input
;  changed check for instrument='NONE' to line and instrument='NONE'
;  which required pass through of LINE
;	passed through nowidgmesss
; October 2021 changed CMER default for Gridtype='User' to 0 instead of 'null'
;	because it seems reasonable that the user would expect that default
;	which was actually putting the observer CMER at phuser+90
;       changes also made in for_griddefaults to be consistent with this
;	Also turned off force losint='null' for POS so that user can make smaller step
;	Also adjusted occult defaults for KCOR and CoMP to be 1.05 Rssun
; January 2022 -- passed through azequi; added test for it to switch to losuse=tau not xlos
;  February 2022 -- for AZEQUI, changed default LOS integratl range to almost pi/2
;   increased number of steps to 157 to preserve spacing near sun
;   also fixed bug where azequi = 'null' resulted in change to xlos
;	it didn't really matter because it was for case of DATA, which
;	has no integration along line of sight, but still
; March 2022 -- fixed bug where PSI default was lower for command line vs widget
;		and also doubled default
;		and updated/fixed the defaults for XLOS/TAU for Changing cases
;	also put in hooks for TURBHY
;	also added option tlos for losuse, centered on the Thomson Sphere
;	and simplified some of the losuse/dodisk logic
;		overwrote azequi=0 for USER
; April 2022
;	added POS=2 for Thomson Sphere
;       changed losint default to .001 for POS
;	reset losuse for obsstart nonzero
; 	added keyword nostretch
; May 2022
;	added defaults for TURBHY
;	updated treatment of losminuse,nlosuse
; Oct 2022
;	removed TLOS as option for all but WL for POS TS slice
; 	changed lower dodisk to 1.0001
;	disallowed dodisk unless XLOS
;	changed USER default cmer back to -90 so that most models
; 	 will be positioned centered on west limb
; 	made TLOS default for WL POS, XLOS otherwise
; Nov 2022
;	fixed bug (maybe) where pos='null'-->posuse=1 instead of 0
;	changed dodisk default to 1.0001 for PSI
;	also checked for DATA before making changes
;	and checked for abs(posuse), and cross-checked with TLOS/XLOS 
;	turned of NoStretch except for TAU, TLOS and not POS or DATA
;	turned off losint in Phys. Diag (but kept for POS because can
;	allow change of integration width)
;	Also forced reset of losint if changing to POS
;	Allowed dodisk to show up when LOS not XLOS because it just gives
;		error message in IDL window, useful because tells what you
;		need to get it -- otherwise you just have to know...
; Dec 2022 - added NEVIII
; Feb 2023 -- changed check on obsstart for change to LOSUSE to be 
;		obsstart=1, not obsstart ne 0, because was cchanging
;		LOSUSE for change in Stokes line, and forcing unnecessary
;		recalculation
; 		also turned thetaoval back on for turbhy
; Apr 2023 -- changed dodisk default to 1.0001 for PSI -- was 1.1 -- mistake?
;		should have only been 1.1 for POS -- but I think the "ne"
;		Physical Diagnostics should have been "eq"
; 		 Also commented out NEVIII occulting restrictions
;  May 2022 -- gave same capability for other UV specpol lines
;                       (OVI, LYA-- be careful esp. for LYA 
;                       -- just for tests because 
;                       generally chromo not treated in FORWARD)
; June 2023 -- added SYNCOM hooks
; July 2023 -- changed default LOSMIN to -pi/2 for all not just azequi
; Sept 2023 -- added AWSOM hooks
;	 changed AWSOM/PSIMAS dodisk default to 1.05 instead of 1.1
;	 also added check for obsstart so if change to e.g. AIA on widgt
;	 when in PSIMAS/AWSOM, will reset to the XLOS no occult case
; Oct 2023 -- made XLOS no occult default for IONDENS
; Nov 2023 -- made LYA defaults as before, no on disk
; Feb 2024 -- added MGIX
; Jul 2024 -- added CIRTOY hooks



widgregen=flag.widgregen
datastart=flag.dta
modelstart=flag.mdl
obsstart=flag.obs

for_checkloswidget,cmer=cmer,upoccult=upoccult,axisym=axisym,notpos,notcarr,notuser

if strupcase(gridtype) eq 'USERINPUT' or strupcase(gridtype) eq 'USER' then begin
  default,cmer,-90.d0
  azequi=0
endif

if strupcase(gridtype) eq 'PLANEOFSKY' then begin

;
; cmer default -90, so that model structure at
; zero longitude is centered at west(right) limb
; 
; only exception is FARADAY and RADIO which default to structure
; on the disk, so cmer=0
;
;
; this was actually pretty annoying, so commenting out
;

;        if strupcase(instrument) eq 'RADIO' or strupcase(instrument) eq 'FARADAY' then begin
;         default,cmer,0.d0
;         if obsstart eq 1 then cmer=0.d0
;        endif

; check if coming from widget non PLANEOFSKY
;
	if  notpos eq 1 then begin
	 if is_number(cmer) then begin
	   if cmer eq 180.d0 then cmer=-90.d0 
	 endif else cmer = -90.d0
	endif else default,cmer,-90.d0

endif

if strupcase(gridtype) eq 'CARRMAP' then begin

;
; cmer default 180 centers Carrington map plots on 180
;

; check if coming from widget non CARRMAP

	if  notcarr eq 1 then begin
	   if is_number(cmer) then begin
	     if cmer eq -90.d0 then cmer=180.d0 
           endif else cmer = 180.d0
	endif else default,cmer,180.d0

endif


;
;      keyword LOSUSE ('Tau'/'XLos'/'TLos') 
;	- line of sight angle TAU or distance along line of sight 
;		XLOS is parallel lines of sight
;	   and now added TLOS can be along non-parallel lines of sight
;		default 'Tau'
;	--> LosPramsStruct Tag LosUse (identical unless 
;		AZEQUI changes 'XLOS' to 'TLOS'
;		   or POS is set and TAU is set
;		   of polarization observables are selected for WL and XLOS is set
;
;	keyword NOSTRETCH -- if set and LOSUSE=TLOS, 
;		will not stretch TLOS step with elongation angle 
;		but rather stick to same LOSMIN and LOSINT for all
;		lines of sight
;		default,0


posuse=pos
; this should not happen - except maybe legacy save sets
if strupcase(string(pos)) eq 'NULL' then posuse=0.0

if strupcase(instrument) eq 'OBSERVABLES' or strupcase(instrument) eq 'PHYSICAL DIAGNOSTICS' or posuse ne 0 then begin
 if strupcase(instrument) ne 'WL' $
   and strupcase(instrument) ne 'NONE' then begin
    if obsstart ne 0 then losuse='XLOS' else default,losuse,'XLOS'
 endif else begin
    if obsstart ne 0 then losuse='TLOS' else default,losuse,'TLOS'
 endelse
endif

if modelstart ne 0 and strupcase(modelname) eq 'TURBHY' and strupcase(gridtype) eq 'PLANEOFSKY' then begin
 losuse='TLOS'
 undefine,nlos
 nostretch=1
 undefine,losmin
 undefine,losint
 widgregen=1
endif

if strupcase(modelname) eq 'TURBHY' and strupcase(gridtype) eq 'PLANEOFSKY' then default, losuse,'TLOS'

if strpos(strupcase(instrument),'OMP') lt 0 and strupcase(instrument) ne 'CORMAG' $
;   and strpos(strupcase(instrument),'OVI') lt 0 $
;   and strpos(strupcase(instrument),'NEVIII') lt 0 $
;   and strpos(strupcase(instrument),'MGIX') lt 0 $ 
   and strupcase(instrument) ne 'LYA' $
   and strupcase(instrument) ne 'WL' and strupcase(instrument) ne 'NONE' then begin
 if (modelstart ne 0 or obsstart eq 1) and (strupcase(modelname) eq 'PSIMAS' or strupcase(modelname) eq 'AWSOM' or strupcase(instrument) eq 'IONDENS') and strupcase(gridtype) eq 'PLANEOFSKY' then begin
  losuse='XLOS'
  undefine,nlos
  undefine,nostretch
  undefine,losmin
  undefine,losint
  widgregen=1
 endif
 if (strupcase(modelname) eq 'PSIMAS' or strupcase(modelname) eq 'AWSOM' or strupcase(instrument) eq 'IONDENS') and strupcase(gridtype) eq 'PLANEOFSKY' then default, losuse, 'XLOS' else default,losuse,'TAU'
 if strupcase(losuse) eq 'NULL' then if (strupcase(modelname) eq 'PSIMAS' or strupcase(modelname) eq 'AWSOM' or strupcase(instrument) eq 'IONDENS') and strupcase(gridtype) eq 'PLANEOFSKY' then losuse='XLOS' else losuse='TAU'
endif else begin
 if obsstart eq 1 then losuse='TAU' else default,losuse,'TAU'
 if strupcase(losuse) eq 'NULL' then if strupcase(modelname) eq 'TURBHY'  then losuse='TLOS' else losuse='TAU'
endelse

;
; POSUSE set forces TLOS if TAU set for WL
;   or if XLOS set for polarization diagnostics
;

checkpos=0
if (strupcase(losuse) eq 'TAU') and posuse ne 0 then checkpos=1
if (strupcase(losuse) eq 'XLOS') then begin
   if strupcase(line) eq 'XPOLF' or $
     strupcase(line) eq 'XPOLB' or $
     strupcase(line) eq 'XPOLG' or $
     strupcase(line) eq 'TPOLU' or $
     strupcase(line) eq 'TPOLS' or $
     strupcase(line) eq 'TPOLG' then begin
       checkpos=1
   endif
endif

if checkpos ne 0 $
   and strupcase(modelname) ne 'DATA' then begin
 if strupcase(instrument) ne 'WL' $
   and strupcase(instrument) ne 'NONE' then begin
  losuse='XLOS'
  print,'Changing to XLOS integration for POS'
  undefine,nlos
  undefine,nostretch
  undefine,losmin
  undefine,losint
  widgregen=1
 endif else begin
  losuse='TLOS'
  print,'Changing to TLOS integration for POS and polarization diagnostics'
  undefine,nlos
  undefine,nostretch
  undefine,losmin
  undefine,losint
  widgregen=1
 endelse
endif


; AZEQUI set forces LOSUSE=TAU if set to XLOS (non POS slice)
;	or changes XLOS to TLOS for RADIO
;

azuse=azequi
if strupcase(string(azequi)) eq 'NULL' then azuse=0

if azuse eq 1 and posuse eq 0 $
   and strupcase(modelname) ne 'DATA' then begin
 if strupcase(losuse) eq 'XLOS' then begin
  if strupcase(instrument) ne 'RADIO' and strupcase(instrument) ne 'FARADAY' then begin
   losuse='TAU'
   print,'Changing to TAU integration because AZEQUI is set'
  endif else begin
   losuse='TLOS'
   print,'Changing to TLOS integration because AZEQUI is set'
  endelse
  undefine,nlos
  undefine,nostretch
  undefine,losmin
  undefine,losint
  widgregen=1
 endif
endif
;
;		RADIO or FARADAY SET FORCES LOSUSE=XLOS (if LOSUSE eq TAU)
;			or to TLOS if AZEQUI set
;

if strupcase(instrument) eq 'RADIO' or strupcase(instrument) eq 'FARADAY' then begin
         if strupcase(losuse) eq 'TAU' then begin
	  if azuse ne 1 then begin
           print,'Changing to XLOS integration because RADIO is set'
           losuse = 'XLos'
	  endif else begin
           print,'Changing to TLOS integration because RADIO is set'
           losuse = 'TLos'
	  endelse
	  undefine,nlos
  	  undefine,nostretch
	  undefine,losmin
	  undefine,losint
          widgregen=1
         endif
endif
;
; if POS and TLOS then set to 2 so it will label things properly on plots
;

if (abs(posuse) eq 1) and (strupcase(losuse) eq 'TLOS') then begin
 posuse=2*posuse
 if strupcase(string(pos)) ne 'NULL' then pos=2*pos
endif
if (abs(posuse) eq 2) and (strupcase(losuse) eq 'XLOS') then begin
 posuse=posuse/2.
 if strupcase(string(pos)) ne 'NULL' then pos=pos/2.
endif

;
;      keyword DODISK (0/double) - default = 0; if dodisk set, properties are calculated on and integrated
;		above sphere of size dodisk (dodisk=1.d0 would be the photosphere)
;	--> LosPramsStruct Tag DoDisk (identical)
;
; only allowed for XLOS
;
;

if exist(dodisk) then dodiskold=dodisk else dodiskold='notset'
if strupcase(gridtype) eq 'PLANEOFSKY' or strupcase(gridtype) eq 'USER' then begin
  if strupcase(losuse) eq 'XLOS' then begin
    default,dodisk,1.d0 
  endif else begin
    dodisk=0.d0
    if is_number(dodiskold) then begin
       if dodiskold ne dodisk then print,'DODISK not allowed except for XLOS integration - unsetting'
    endif
  endelse
  if strupcase(string(dodisk)) eq 'NULL' then begin
    if strupcase(losuse) eq 'XLOS' then dodisk=1.d0 else dodisk=0.d0
  endif
  if dodisk lt 1. then dodisk=0.d0
  dodisk=double(dodisk)
  if strupcase(modelname) eq 'DATA' then dodisk='NULL'
  if strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG' $
;        or strpos(strupcase(instrument),'OVI') ge 0 $
;        or strpos(strupcase(instrument),'NEVIII') ge 0 $
;        or strpos(strupcase(instrument),'MGIX') ge 0 $
	 or strupcase(instrument) eq 'LYA' $
	or strupcase(instrument) eq 'KCOR' $
	or strupcase(instrument) eq 'WL' or (strupcase(instrument) eq 'NONE' and strupcase(line) eq 'NONE')$
	or strupcase(modelname) eq 'TOMO' or strupcase(modelname) eq 'CROISSANT' or strupcase(modelname) eq 'STRIA' or strupcase(modelname) eq 'SYNCOM' or strupcase(modelname) eq 'CIRTOY' $
 	or strupcase(modelname) eq 'TURBHY' $
	then begin
          dodisk = 'NULL'	
          if is_number(dodiskold) then begin
	   if dodiskold ne 0. then begin
	    if is_number(dodisk) then begin
             if dodiskold ne dodisk then $
              print,'DODISK not allowed for coronagraph integration - unsetting'
	    endif else $
              print,'DODISK not allowed for coronagraph integration - unsetting'
	   endif
	  endif
  endif
endif else begin
  if strupcase(limb) ne 'CMER' then dodisk='NULL' else dodisk=rheight
;   print,'limb in for_losdefaults early =',limb
;   print,'dodisk in for_losdefaults early =',dodisk
endelse

;
;      keyword OCCULT (double) - radial height at which to draw occulting disk
;               if negative value, plot dashed line and show all points
;               Default 1 (photosphere)
;	--> LosPramsStruct Tag Occult (identical unless dodisk set or not planeofsky)
;

if exist(occult) then occultold=occult else occultold='notset'
if strupcase(gridtype) eq 'PLANEOFSKY' then begin
  if strupcase(modelname) eq 'DATA' then begin
   if strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'KCOR' then begin
    default,occult,1.05d0 
    if exist(datastart) then if datastart eq 1 or datastart eq 2 then occult=1.05d0
   endif else begin
    default,occult,0.d0
    if exist(datastart) then if datastart eq 1 or datastart eq 2 then occult=0.d0
   endelse
   if strupcase(string(occult)) eq 'NULL' then occult=0.d0
  endif else begin
   default,occult,0.d0
   if strupcase(string(occult)) eq 'NULL' then occult=0.d0
   if strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG' $
;    or strpos(strupcase(instrument),'OVI') ge 0 $
;     or strpos(strupcase(instrument),'NEVIII') ge 0 $ 
;     or strpos(strupcase(instrument),'MGIX') ge 0 $
     or strupcase(instrument) eq 'LYA' $
    or strupcase(instrument) eq 'WL' or (strupcase(instrument) eq 'NONE' and strupcase(line) eq 'NONE')$
        or strupcase(modelname) eq 'TOMO' or strupcase(modelname) eq 'CROISSANT' or strupcase(modelname) eq 'CIRTOY' $
        or strupcase(modelname) eq 'STRIA' or strupcase(modelname) eq 'SYNCOM' $
        or strupcase(modelname) eq 'TURBHY' then begin
	 if strupcase(modelname) ne 'STRIA' then begin
	  if abs(occult) lt 1.d0 then occult = 1.0d0
	 endif else begin
	  if abs(occult) le 1.d0 then occult = 6.0d0
	 endelse
   endif
  endelse
  occult=double(occult)
endif else occult='NULL'

;
; need to make sure occult and dodisk are consistent with each other
;

if is_number(occult) then if abs(occult) lt 1. and is_number(dodisk) then begin
	if dodisk eq 0.d0 then begin
	 if strupcase(modelname) ne 'STRIA' and strupcase(modelname) ne 'SYNCOM' $
         then occult = 1.0d0 else occult = 6.0d0
        endif
endif
if is_number(dodisk) then if dodisk ge 1.d0 and is_number(occult) then begin
 	if occult ge 1.d0 then $
          if is_number(dodiskold) then occult=0.d0 else begin
           if strupcase(dodiskold) eq 'NULL' then occult=0.d0
           if strupcase(dodiskold) eq 'NOTSET' then dodisk=0.d0
          endelse
endif

;
; if numerical, often exactly dodisk=1 causes problesm
;	(many analytic models too)
;   especially for PSIMAS or AWSOM -- where all points lt 2-5d5 temperature
;	set to ~0 density -- this only matters 
;	for POS of Physical Diagnostics (integrated quantities ignore points)
;		so for those cases set default = 1.05
;
; (commenting if statement so analytic models set to 1.0001)
;if (strupcase(modelname) eq 'NUMCUBE' or strupcase(modelname) eq 'ADAPTCUBE' or strupcase(modelname) eq 'PSIMAS')  and exist(dodisk) then begin
  dodiskuse=1.0001d0
  if (strupcase(modelname) eq 'PSIMAS' or strupcase(modelname) eq 'AWSOM' or strupcase(instrument) eq 'IONDENS') and (posuse ne 0 or strupcase(instrument) eq 'PHYSICAL DIAGNOSTICS') then dodiskuse=1.05
  if strupcase(string(dodisk)) ne 'NULL' then if dodisk eq 1.d0 then dodisk = dodiskuse
  if is_number(occult) then if occult eq 1.d0 then occult = dodiskuse
;endif


if string(occultold) ne string(occult) and strupcase(string(occultold)) ne 'NOTSET' then widgregen=1
if string(dodiskold) ne string(dodisk) and strupcase(string(dodiskold)) ne 'NOTSET' then widgregen=1

;
;		DODISK SET FORCES LOSUSE=XLOS (if LOSUSE eq TAU)
;			or to TLOS if AZEQUI set 
;

;if strupcase(gridtype) eq 'PLANEOFSKY' then begin
   if is_number(dodisk) then dodisktest=dodisk else dodisktest=0.
   if dodisktest ne 0. and strupcase(modelname) ne 'DATA' then begin
         if strupcase(losuse) eq 'TAU' and strupcase(instrument) ne 'OBSERVABLES' and strupcase(instrument) ne 'PHYSICAL DIAGNOSTICS' then begin
	  if azuse ne 1 then begin
           print,'Changing to XLOS integration because DODISK is set'
           losuse = 'XLos'
	  endif else begin
           print,'Changing to TLOS integration because DODISK is set'
           losuse = 'TLos'
	  endelse
	  undefine,nlos
  	  undefine,nostretch
	  undefine,losmin
	  undefine,losint
          widgregen=1
         endif
   endif
;endif


;
;       keyword UPOCCULT (double) - radial height above which no data plotted
;		negative plot dashed line and show all points
;               Default 1000 - meaning no upper occulter
;		except for STRIA and SYNCOM which have default 32 
;	--> LosPramsStruct Tag UpOccult (identical unless not planeofsky)
;

if strupcase(gridtype) eq 'PLANEOFSKY' then begin
  if strupcase(modelname) ne 'STRIA' and strupcase(modelname) ne 'SYNCOM' $
     then upo_default = 1000.d0 else upo_default= 32.d0
  if exist(datastart) then if datastart eq 1 or datastart eq 2 then upoccult=upo_default
  if exist(modelstart) then if modelstart eq 1 then upoccult=upo_default
  if notpos eq 1 then upoccult=upo_default $
	 else default,upoccult,upo_default
  if upoccult eq 0.d0 then upoccult=upo_default
  upoccult=double(upoccult)
endif else upoccult='NULL'

;
;      keyword AXISYM (0/1)- 
;		default 0
;		useful if  model is axisymmetric
;		if axisym set, NLOS is halved and LOS integration doubled
;	--> LosPramsStruct Tag AxiSymMult (1.d0 if axisym=0, 2.d0 if axisym=1)
;

default,axisym,0
if strupcase(string(axisym)) eq 'NULL' then axisym = 0

;      keyword BANG (double - DEGREES) -- solar B angle 
;		Change of heliolatitude of observer
;		Default zero
;	--> LosPramsStruct Tag Bang (converted to radians)
;

default,bang,0.d0

;
;	keyword INCRES (0/1) - what to do above disk 
;		INCRES=1 means double resolution
;          	INCRES=0 means throw away half the points (the ones that would
;               have been behind the disk)

default,incres,1
;
; if coming from widget with Physical Diagnostic previously set
; (indicated by presence of 'NULL'), revert to 
; defaults
;
  if strupcase(string(incres)) eq 'NULL' then incres=1

;
;
;       keyword THETAO (double - DEGREES) similar to PHIO, allows
;               a rotation of model structure from equator to
;               a latitude THETAO in heliographic coordinates.
;
;               By FORWARD convention, model structures are centered on
;               theta=90 in heliographic coordinates.
;
;               So, by choosing coordinates (THETAO, PHIO) a model
;               structure is placed anywhere on the sun, and then
;               an integral can be performed
;               depending on the viewer's position (BANG, CMER)
;
;               NOTE THETAO is not really meaningful for global models
;               ***so turned off for PSIMAS and PFSSMOD  and AWSOM
;               and TOMO and STRIA and SYNCOM ***
;
;               (note PHIO can still be useful for global models
;               for making Carrington maps not centered on 180)
;
;               NOTE also- THETAO is NOT equivalent to P angle/roll
;                   except for the special case of BANG=0, CMER=-90, PHIO=0
;
;	--> LosPramsStruct Tag Thetao (converted to radians)



default,thetao,90.d0

;      keyword LOSMIN (double - GENERALLY NEGATIVE)  - minimum line-of-sight distance or angle
;		if XLOS (SOLAR RADII) 
;			Default x = -1., which is r = sqrt(2.) for LOS intersecting photosphere
;		if TAU (RADIANS) 
;			Default: -pi/2 
;	(pi/2 would be for infinite LOS; that is default if azequi is set)
;	(note used to have pi/4 default for all but azequi, but caused problems)
;               note xlos = 0 / tau = 0 is plane of the sky
;
;	--> LosPramsStruct Tag LosMin (identical unless adjusted to ensure integer NXLos)
;					or if unset because of change in LOSUSE)
;
;      keyword LOSINT (double) -  resolution along the line of sight
;		if XLOS (SOLAR RADII)
;			Default 0.02
;			except for PSIMAS/AWSOM then it is 0.005			
;		if TAU (RADIANS)
;			Default about 0.02
;		if TLOS (SOLAR RADII)
;			Default 0.02 [but scaled by elongation angle 
;					in for_losint]
;	--> LosPramsStruct Tag LosInt (identical unless unset)
;
;      keyword NLOS (integer)- number of points to integrate along line of sight
;		Default: number needed for symmetry about POS (LOSMIN may need to be tweaked)
;
;	--> LosPramsStruct Tag NLos (identical unless AXISYM set, then cut in half and rounded up)
;
;	ASYMMETRY ABOUT POS IS ALLOWED but requires user input of all three parameters (no defaults)
;	 	otherwise the program will assume symmetry is desired and modify parameters accordingly
;

;
; allow widget option of easily resetting symmetric integration
; by changing e.g. losmin and setting losint=0, or alternatively
; changing nlos and setting losint=0 
; or alternatively retrieving default spacing
;

if exist(losint) then begin
 if is_number(losint) then begin
  if losint eq 0 then begin
	undefine,losint
 	widgregen=1
  endif
 endif
endif

if n_elements(flag) gt 0 then flag.widgregen=widgregen

;
; set up default losmin, nlos
;

if strupcase(losuse) eq 'XLOS' or strupcase(losuse) eq 'TLOS' then begin
      if strupcase(losuse) eq 'XLOS' and dodisktest ne 0. and (strupcase(modelname) eq 'PSIMAS' or strupcase(modelname) eq 'AWSOM') and strupcase(instrument) ne 'RADIO' and strupcase(instrument) ne 'FARADAY' and azuse ne 1 then begin
        losminuse=-.5d0
        if strupcase(modelname) eq 'PSIMAS' then nlosuse=201 else nlosuse=501
      endif else begin
        losminuse =-1.d0
        nlosuse=101
      endelse
      if strupcase(losuse) eq 'TLOS' and azuse eq 1 then begin
       losminuse=-2.d0
       nlosuse=201
      endif
      if strupcase(modelname) eq 'TURBHY' then begin
       if azuse eq 1 then begin
        losminuse=-100.d0
        nlosuse=201
       endif else begin
        losminuse=-36.d0
        nlosuse=101
       endelse
       default,nostretch,1
      endif
endif else begin 
    if azuse eq 0 then begin
;      losminuse=-!dpi/4.d0
      losminuse=-1.57
;      nlosuse=79
      nlosuse=158
    endif else begin
      losminuse=-1.57
      nlosuse=158
    endelse
endelse

;
; if coming from widget with Physical Diagnostic previously set
; (april 21 2020) - or from other LOS type
; (indicated by presence of 'NULL'), revert to 
; defaults
;

if exist(losmin) then begin
  if strupcase(string(losmin)) eq 'NULL' and posuse eq 0 then begin
   losmin=losminuse
   nlos=nlosuse
   losint=-2.d0*losmin/(nlos-1.d0)
  endif
endif

;
; (april 21, 2020) - this created the weird bug where it reverted to defaults
;  if you give it exactly 1 or pi/4
; **note now I use pi/2**
; ***NOTE if bringing back, add AWSOM***
;
; first, if coming from widget, check for obviously coming from other LOS type
;
;if exist(losmin) then begin
; if notpos eq 1 or notcarr eq 1 or notuser eq 1 then begin
;  if strupcase(losuse) eq 'XLOS' and abs(losmin+!dpi/4.d0) lt 1d-2 then begin
;    if strupcase(modelname) eq 'PSIMAS' and strupcase(instrument) ne 'RADIO' and strupcase(instrument) ne 'FARADAY' then losmin=-.5d0 else losmin =-1.d0
;    if strupcase(modelname) eq 'PSIMAS' and strupcase(instrument) ne 'RADIO' and strupcase(instrument) ne 'FARADAY' then nlos=201 else nlos=101
;    losint=-2.d0*losmin/(nlos-1.d0)
;  endif
;  if strupcase(losuse) eq 'TAU' and (abs(losmin+1.d0) lt 1d-2 or abs(losmin+.5d0) lt 1d-2) then begin
;    losmin = -!dpi/4.d0 
;    nlos=79
;    losint=-2.d0*losmin/(nlos-1.d0)
;  endif
; endif
;endif

asymforce=0
if n_elements(nlos) ne 0 and n_elements(losint) ne 0 and n_elements(losmin) ne 0 then begin
 if is_number(nlos) eq 1 and is_number(losint) eq 1 and is_number(losmin) eq 1 then asymforce=1
endif

if (notpos eq 1 or notcarr eq 1 or notuser eq 1) and (asymforce eq 1) then begin
  if (abs(losint + 2.d0*losmin/(nlos-1.d0)) gt losint/1000.) then message,/info,'Warning! This integration wont be symmetric about the plane of sky.  If you want it symmetric, set LOSINT to zero and it will be recalculated'
endif

;
; if losmin is positive it implies the integration wont cross the POS
; this is only allowed if user specifies all three parameters
;

if n_elements(losmin) ne 0 and posuse eq 0 then begin
	if losmin gt 0. then begin
	 if asymforce eq 0 then begin
 	   print,'You gave positive losmin and did not specify both nlos and losint. Changing to negative'
	   losmin=-losmin
	 endif else print,'NOTE positive losmin means integration wont cross the POS'
 	endif
endif

; 
; if parameters are not set, choose to ensure symmetry about plane of sky
;

if asymforce eq 0 and posuse eq 0 then begin
   if keyword_set(nlos) ne 0 and keyword_set(losint) ne 0 then $
          losmin=-losint*(nlos-1.d0)/2.d0 $
	else default,losmin,losminuse
   if keyword_set(nlos) ne 0 then begin
       default,losint,-2.d0*losmin/(nlos-1.d0) 
   endif else begin
       default,losint,-2.d0*losmin/(nlosuse-1.d0)
   endelse
   default,nlos,round((-2.d0*losmin)/losint + 1.d0)
   losslop = -2.d0*losmin - losint*(nlos-1.d0)
   losmin = losmin+losslop/2.d0
endif

;
; note there is potential here for losmin to be less than -Pi/2
;

if strupcase(losuse) eq 'TAU' and posuse eq 0 then begin
   if abs(losmin) gt !dpi/2.d0 then begin
      if keyword_set(nowidgmess) then message,/info,'Bad Nlos/LosInt combo, reverting to defaults *STEPPING IN CONSTANT TAU ANGLE, LOSINT AND LOSMIN NEED TO BE IN RADIANS, LOSMIN LE PI OVER 2*' else d=dialog(/WARNING,'Bad Nlos/LosInt combo, reverting to defaults *STEPPING IN CONSTANT TAU ANGLE, LOSINT AND LOSMIN NEED TO BE IN RADIANS, LOSMIN LE PI OVER 2*')
      losmin=-1.57
      nlos=158
;      losint=0.02
      losint=-2.d0*losmin/(nlos-1.d0)
   endif
endif

if strupcase(LosUse) eq 'TAU' then LosValUse='double-RADIANS'
if strupcase(LosUse) eq 'XLOS' or strupcase(losuse) eq 'TLOS' then LosValUse='double-RSUN'


LosUseValFull=['Tau','XLos','TLos']
LosUseValElong=['Tau','TLos']
LosUseValLOS=['XLos','TLos']
LosUseValPOS='NULL'

LosUseVal=LosUseValFull

;
; for widget, put nulls into integration items if
; doing Physical Diagnostic
; or POS
;

if strupcase(instrument) eq 'OBSERVABLES' or strupcase(instrument) eq 'PHYSICAL DIAGNOSTICS' or posuse ne 0 then begin
;  losuse='NULL'
;    commented the above out because we need to maintain the information
;	about whether to plot physical diagnostics at X=0 or the Thomson Sphere
  LosUseVal=LosUseValLOS
  if obsstart ne 0 then losint=0.001 else default,losint,.001
  losmin='NULL'
  nlos='NULL'
  incres='NULL'
  if posuse ne 0 then begin
    if strupcase(line) eq 'XPOLF' or $
     strupcase(line) eq 'XPOLB' or $
     strupcase(line) eq 'XPOLG' or $
     strupcase(line) eq 'TPOLU' or $
     strupcase(line) eq 'TPOLS' or $
     strupcase(line) eq 'TPOLG' then begin
        LosUseVal=LosUseValPOS
	LosUse='null'
    endif
  endif
endif
  
CmerVal='double-DEGREES'
BangVal='double-DEGREES'
ThetaOVal='double-DEGREES'
PhiOVal='double-DEGREES'
NLosVal='integer'
DoDiskVal='double'
IncResVal=['0','1']
NoStretchVal='tog'
if strupcase(modelname) eq 'DATA' then begin
  LosUseVal='nodisplay' 
  LosValUse='nodisplay' 
  CmerVal='nodisplay'
  BangVal='nodisplay'
  ThetaOVal='nodisplay'
  PhiOVal='nodisplay'
  NLosVal='nodisplay'
  DoDiskVal='nodisplay'
  IncResVal='nodisplay'
  NoStretchVal='nodisplay'
endif
if strupcase(instrument) eq 'PHYSICAL DIAGNOSTICS' then LosValUse='nodisplay'


if strupcase(modelname) eq 'PFSSMOD' or strupcase(modelname) eq 'PSIMAS' or strupcase(modelname) eq 'AWSOM' or strupcase(modelname) eq 'TOMO' $
;	or strupcase(modelname) eq 'STRIA' or strupcase(modelname) eq 'SYNCOM' or strupcase(modelname) eq 'TURBHY' then ThetaOVal='nodisplay'
	or strupcase(modelname) eq 'STRIA' or strupcase(modelname) eq 'SYNCOM' $
        then ThetaOVal='nodisplay'
if is_number(dodisk) then if dodisk ne 0 then LosUseVal=LosUseValLOS
if is_number(dodisk) then begin
 if dodisk eq 0 then IncResVal='nodisplay' 
endif else if strupcase(dodisk) eq 'NULL' then IncResVal='nodisplay'
if strupcase(instrument) eq 'RADIO' then begin
  LosUseVal=LosUseValLOS
  IncResVal='nodisplay'
endif
if strupcase(instrument) eq 'FARADAY' then LosUseVal=LosUseValLOS
if azuse eq 1 and posuse eq 0 then LosUseVal=LosUseValElong
;if strupcase(gridtype) eq 'CARRMAP' or strupcase(losuse) ne 'XLOS' then begin
if strupcase(gridtype) eq 'CARRMAP' then begin
  DoDiskVal='nodisplay'
  IncResVal='nodisplay'
endif

if strupcase(losuse) eq 'XLOS' $
  or strupcase(instrument) eq 'PHYSICAL DIAGNOSTICS' or posuse ne 0 $
 then NoStretchVal='nodisplay'
default,nostretch,0

;
; in case dodisk changed-- change rheight with it
;

if strupcase(gridtype) eq 'CARRMAP' then begin
 rheightold=rheight
 if is_number(dodisk) then rheight=dodisk
 if rheight ne rheightold then print,'changing rheight to ',rheight
endif

LosInputs={LosUse:losuse,LosUseVal:LosUseVal,$
AxiSym:'NULL',AxiSymVal:'tog',$
Cmer:cmer[0],CmerVal:CmerVal,Bang:bang,BangVal:BangVal,LosMin:string(losmin),LosMinVal:LosValUse,LosInt:losint,LosIntVal:LosValUse,NLos:nlos,NLosVal:NLosVal,Occult:occult,OccultVal:'double-RSUN',UpOccult:upoccult,UpOccultVal:'double-RSUN',DoDisk:dodisk,DoDiskVal:DoDiskVal,IncRes:incres,IncResVal:incresval,PhiO:phio,PhiOVal:PhiOVal,ThetaO:thetao,ThetaOVal:ThetaOVal,NoStretch:nostretch,NoStretchVal:NoStretchVal}

;print,'dodisk in for_losdefaults=',dodisk
;print,'occult=',occult
;print,'for_losdefaults pos=',pos
;print,'for_losdefaults losuse=',losuse
;print,'losmin=',losmin
;print,'nlos=',nlos
;print,'for_losdefaults losint=',losint
;print,'azequi=',azequi
;print,'nostretch=',nostretch
;print,'rheight in for_losdefaults=',rheight
end
