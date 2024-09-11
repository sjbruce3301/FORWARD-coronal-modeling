pro for_griddefaults,modeluse,pos,instrument,line,$
        gridtype=gridtype,ruser=ruser,thuser=thuser,phuser=phuser,coorduser=coorduser,phio=phio,$
        xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,limb=limb,ngrid=ngrid,ngy=ngy,$
        losoffset=losoffset,rheight=rheight,azequi=azequi,distobs=distobs,nowidgmess=nowidgmess,$
	GridInputs=GridInputs

;
; this program sets all defaults for the forward calculation grids
;
;   inputs 
;
;       MODELUSE - keyword determined in for_modeldefaults
; 	LINE, INSTRUMENT, POS - keywords determined in for_obsdefaults
;
;   inputs to FOR_GET_GRID (called by FOR_GETSTRUCTS)
; 
;    keywords defined below
;
;  Optional output keyword GridInputs gathers all keyword inputs
;       useful for widget setup
;
;   Called by FOR_DRIVE, FOR_WIDGET_EVENT, FOR_WIDGET_PLOT_EVENT, FOR_WIDGET, FOR_NOISEDEFAULTS
;
;   Calls routine CHECKGRIDWIDGET to determine if coming from widget
;       NOTPOS, NOTCARR, NOTUSER -- determined in checkwidget
;
;  Written Sarah Gibson 2013
;  Version 2.0 July 2014
;
;  Feb-Mar 2016-- allow variation of XOFFSET for USER gridtype; related comments - SEG
;  Mar 10 2016 -- put in option of COORDUSER, and added to comments 
;  June 2016 -- fixed bug where xoffset was overwritten for PHYSICAL DIAGNOSTICS From line command - SEG
;  Nov 2016 -- added defaults for EXPFAC (expansion factor) line
;  Jan 2018 -- modified test to be sure xoffset not an array SEG
;  August 2020 -- testing capabilities for DATA Carrmap
;	also, added NUMCUBE to those with maxheight=3 default
;  Sept 2020 -- made CROISSANT, TOMO  maxheight=4.5 default
; Jun 2021 -- made STRIA maxheight=36
;  Aug 2021 - fixed bug where xx/yy min/max were being set to null
;  Sept 2021 - removed check for negative ngrid/ngy from CARRMAP 
;		consistent with change in FOR_WIDGET_EVENT
;		adjusted check for instrument='NONE' to instrument and line = 'NONE'
;  Sept 2021 -- testing AQEQUI
;	passed through nowidgmess
;  Oct 2021 -- removed changeaz test in checkgridwidget -- unnecessary
;	also removed text expecting non-numerical CMER -- now the 
;	 default for USER is 0.0, because that seems more reasonable for
;	 user to expect. Edited text in USER section accordingly.
;	also made default height for limb='CMER' Carrmap 1.0 instead of 1.05
; 	also turned off AzEqui in widget for POS=1 or Physical Diagnostics
;  Dec 2021 -- added distobs so observer's position could be changed 
;		updated order in gridinputs (azequi and distobs at end) to avoid
;		rerun of old save files.
;  Jan 2022 -- turned off AzEqui for data files
;  Mar 2022 -- made TURBHY maxheight=36
;  		changed xoffset to losoffset
;  Apr 2022
;		turned AzEqui and Distobs back on in widget 
;		 for POS=1 and Physical Diagnostics (and distobs for CARRMAP)
;		added POS=2 for Thomson Sphere
;		made default resolution for TURBHY 64X64
;  Jun 2022
;		made DATA distobs nodisplay
;  Oct 2022 
;		put in checks for losoffset
;  Nov 2022
;		removed losoffuser because unused
;		made physical diagnostic widget for losoffset be visible
;		commented out (or removed) is_number(pos) checks because
;		should always be numbers
;		made rheight default for limb='Cmer' be 1.0001 not 1
;		also removed maxheight as input keyword
;  Dec 2022 - added NEVIII
;  Apr 2022 -- removed limb only for NEVIII
;  May 2022 -- gave same capability for other UV specpol lines
;			(OVI, LYA-- be careful esp. for LYA 
;			-- just for tests because 
;			generally chromo not treated in FORWARD)
; June 2023 -- added SYNCOM hooks **commented out because not needed **
;
; Feb 2024 -- added MGIX
; Jul 2024 -- added hooks for CIRTOY

;if exist(xxmin) then print,'start xxmin,xxmax,yymin,yymax',xxmin,xxmax,yymin,yymax

;if keyword_set(losoffset) then losoffuser=losoffset else losoffuser='unset'

;
;       keyword GRIDTYPE (string - PlaneOfSky or CarrMap or UserInput) 
;		default PlaneOfSky
;	---> GridPramsStruct tag GridType (identical)
;
; 	keyword AZEQUI (0/1)
;	   whether to plot in Cartesian (X-Y-Z) or Azimuthal Equidistant projection
;		DEFAULT 0 (Cartesian)


for_checkgridwidget,rheight=rheight,ngy=ngy,ruser=ruser,notpos,notcarr,notuser

if strupcase(modeluse) ne 'DATA' then begin

 default,distobs,215.d0

 Case 1 of
        datatype(gridtype) eq 'UND' : gridtype = 'PLANEOFSKY'
        datatype(gridtype) eq 'STR': begin
	  if strupcase(gridtype) ne 'PLANEOFSKY' and strupcase(gridtype) ne 'CARRMAP' and $
	   strupcase(gridtype) ne 'USER' and strupcase(gridtype) ne 'USERINPUT' then $
           if keyword_set(nowidgmess) then message,/info,"must be 'PLANEOFSKY', 'CARRMAP', or 'USERINPUT' (or 'User')" else d=dialog(/WARNING, "must be 'PLANEOFSKY', 'CARRMAP', or 'USERINPUT' (or 'User')") 
	end
        else: if keyword_set(nowidgmess) then message,/info,'gridtype must be a string' else d=dialog(/WARNING,'gridtype must be a string')
 endcase

;
;	Defaults needed for UserInput
;
; 	Need to have set ruser,thuser, phuser  (doubles)
; 		where thuser and phuser are in DEGREES
;		ruser in SOLAR RADII
;	   SPHERICAL COORDINATES
;		ruser should be radius from center of sun
; 		thuser should be colatitude (not polar angle)
; 		phuser should be longitude (not central meridian)
;	   OBSERVER
;		if not explicitly set, will default to cmer=0, bang=0 (defaults set in for_losdefaults)
;	   TWO CHOICES FOR COORDINATE SYSTEMS:
;           coorduser='helio' is Carrington heliographic (or model) coordinates
;	       (see e.g. Thompson, W. T., A&A, 2005)
;	       phuser is Carrington longitude, thuser is heliographic colatitude 
;		 (0 solar north pole, 180 south).  
;		This is the coordinate system plotted e.g. in for_drive,line='thmod'/'phmod'
;	       Useful for example in reference to structures that can be mapped onto
;	         a Carrington heliographic grid, e.g. structures on the solar disk
;		 or to probe a model at a particular point.
;           coorduser='observer' is also spherical; but oriented relative to a plane of sky defined by an 
;		an observer's central meridian (cmer) and B angle (bang)
;	       (can be directly translated into Heliocentric-Cartesian (Thompson,2005))
;		This is the coordinate system plotted e.g. in for_drive,line='thobs'/'phobs'
;	       thuser is colatitude measured from plane-of-sky projected North (Z axis).
;	       phuser is Carrington longitude defined w.r.t. that projected Z axis
;		(if one is really looking down from the North pole, it may be tricky to define 
;		Carrington longitude i.e., it wouldn't change in time)
;	       Useful for example for structures observed at solar limb (thuser is simply determined
;		 from polar angle; phuser can be set to the day's cmer+90 or cmer-90 depending on limb).
;	       NOTE: this is the legacy choice -- old programs should still work the same with this 
;		  (default) choice of COORDUSER
;	    NOTE bang,cmer control observer's position; If coorduser='observer' is set, 
;		   they are used to translate to heliographic (model) coordinates.  
;		   (The assumption is that Solar P angle is already corrected for 
;		   since it is an easy rotation in the plane of the sky. 
;		   FORWARD keyword "thetao" should not be confused with Solar P angle - 
;		   it is meant to facilitate model-data fitting in a manner similar to that
;		   described below for phio.  Further notes on thetao can be found in FOR_LOSDEFAULTS.PRO)
;	        If FORWARD keyword "date" is set, bang and cmer will be automatically loaded
;	 	   based on Earth's position on that date; they can be overwritten by
;		   explicit assignment of bang and cmer as keywords. 
;		If coorduser='helio' is set, bang,cmer will not affect evaluation of
;	           physical model quantities (such as density, temperature, magnetic field vector)
;		   at the point(s) (ruser,thuser,phuser).
;		If coorduser='observer' is set, bang will affect physical model evaluation, but
;		   but if only cmer changed it will not (since phuser is in Carrington coordinates independent of choice of cmer)
;		HOWEVER: cmer and bang matter for both coorduser choices if doing LOS integration 
;		   since they define the line of sight.
;	           Note:  A value of phuser=cmer+20 is displaced 20 degrees west longitude 
;			from disk center; phuser=cmer+90 would be West limb, etc. 
;		         note if plane of sky radius < 1 and dodisk not set (always true for TAU and TLOS)
;				-- a value of 0 (and message "no non excluded points") will be returned 
;	     FINALLY - 
;			  "coorduser='observer',bang=0"  
;			should be exactly equivalent to 
;			  "coorduser='helio'" (bang unset or set to zero)
;			 (assuming everything else the same, including cmer)
;		See further notes in FOR_GET_GRID.PRO
;
;	    default coorduser='observer'
;
;	---> GridPramsStruct tags RPos/ThPos/PhPos
;
;       keyword PHIO (double - DEGREES) allows a rotation of model structure in phi, useful for fitting data.
;		NOTE FOR MODEL CAVMORPH, BETTER TO USE MODEL PARAMETER PHCS!
;	        - because it will be saved as part of model parameters -
;			but, if I ever add more streamers then PHIO will remain
;			the one to use to rotate the model globally, since
;			there will be more than one PHCS streamer longitude
;			center
;		By FORWARD convention for other models, model structures are centered on phi=0,theta=90
;		 in model spherical coordinates. So, if Phio = 0, and phuser=0.,thuser=90
;		 the model structure will be probed right in the middle. 
;		There is a redundancy between phuser and Phio.
;		- one can think of it as flying through a fixed model structure (changing phuser) or 
;		rotating the model structure (changing Phio).  The latter approach makes sense
;		if one is fitting data --  phuser can be set to the actual Carrington
;	 	longitude of an observation, and then Phio varied until an optimum central longitude of the 
;		model structure is found in Carrington coordinates. 
;
;	    default phio=0
;
;	---> GridPramsStruct tag Phio (identical but converted to radians)
;
;       keyword LOSOFFSET (double -- SOLAR RADII) -- Offset from center point of LOS integration
;			(X=0 plane if LOSUSE=XLOS 
;			  Thomson Sphere if LOSUSE=TAU or LOSUSE=TLOS)
;		is only used if POS=1 (POS set in FOR_OBSDEFAULT) and tested for below;
;		May be set, for example, if one wants to calculate an integrand
; 		along an LOS associated with a RUSER,THUSER,PHUSER set for a POS position for coorduser= 'observer'  
; 		This can also be done by fixing CMER and specifying spherical coords along LOS,
;		although this is is a little harder (and if requiring an integrand, will still need to set POS=1).
;		Note relation with DODISK (default=1 in FOR_LOSDEFAULTS)
;		 is same as described for GRIDTYPE=PLANEOFSKY
; 	   default losoffset=0,
;
;	---> GridPramsStruct tag LOSoffset (identical unless changed in FOR_GET_GRID)

 if strupcase(gridtype) eq 'USERINPUT' or strupcase(gridtype) eq 'USER' then begin

; check to see if coming from widget but not USER
;	(currently not possible)

     if notuser eq 1 then begin
       ruser=1.05d0
       thuser=90.d0
       phuser=0.d0
       coorduser='observer'
       phio=0.d0
     endif else begin
       default,ruser,1.05d0
       default,thuser,90.d0
       default,phuser,0.d0
       default,coorduser,'observer'
       default,phio,0.d0
     endelse

     if n_elements(losoffset) eq 1 then if strupcase(string(losoffset)) eq 'NULL' then losoffset=0.d0
     if strupcase(instrument) ne 'PHYSICAL DIAGNOSTICS' then begin
        if pos eq 0 then losoffset='NULL' else default,losoffset,0.d0
     endif else begin
; pos should not be 0 for Physical Diagnostics - or if it is temporarily
;  it will be changed. And we don't want to set losoffset to NULL for PD.
;           if pos eq 0 then default,losoffset,'NULL' else default,losoffset,0.d0
        default,losoffset,0.d0
    endelse

;
; these won't be used, but fill the input/output structures

;
;	value 'NULL' will grey them out in the widget
;
     limb='NULL'
     ngrid='NULL'
     ngy='NULL'
     rheight='NULL'
     xxmin='NULL'
     xxmax='NULL'
     yymin='NULL'
     yymax='NULL'
 endif

;
;	Defaults needed for PlaneOfSky: 
;
;       keywords  XXMIN,XXMAX,YYMIN,YYMAX  (doubles - SOLAR RADII)
;		 --  size for plot
;		default +/-2 Rsun; 
;	keyword LIMB ('Cmer'/'East'/'West') -- which limb to look at
;		default 'Cmer'
;		if limb set to 'East' or 'West', sets xxmin/xxmax to 0
;	keywords NGRID (integer) and NGY (integer)-- grid spacing and size of output plot
;		(if NGY set partly overrides NGRID so that DX does not have to equal DY)
;		ngrid default 256 for POS everything but COMP which is 64
;		or EXPFAC which is 64
;		nx=ny=ngrid unless limb is set - nx=ngrid/2; ny=ngrid
;	---> GridPramsStruct tags Limb (unchanged)
;	---> GridPramsStruct tags Dx,Dy 
;	---> GridPramsStruct tags XRange/YRange 
;	---> GridPramsStruct tags XCenter/YCenter
;	---> GridPramsStruct tags RPos/ThPos/PhPos
;
;       keyword LOSOFFSET (double -- SOLAR RADII) -- Offset from center point of LOS integration
;			(X=0 plane if LOSUSE=XLOS 
;			  Thomson Sphere if LOSUSE=TAU or LOSUSE=TLOS)
;		used in FOR_POSNOINT and FOR_FORWARDMAP
;		only used plane-of-sky slice (POS=1) or for Physical Diagnostics
;		Default 0. means plane of sky (LOSUSE=XLOS) or Thomson Sphere (LOSUSE=TS, TAU)
;		otherwise, steps towards away along observer-sun line
;		If DODISK is set (see FOR_LOSDEFAULTS), POS slices plot limb at position LOSOFFSET
;		and also surface of radius DoDisk.  If DoDisk < LOSoffset,
;		essentially this plots just the portion of the surface that lies
;		in front of LOSoffset.  If LOSoffset < Dodisk, it plots the portion
;		of the plane slice at distance DoDisk that is unobstructed by the
;		spherical surface of radius DODISK.
;		NOTE - while the above six lines are true for plotting physical
;		  parameters, they are not for POS=1. For all relative values LOSoffset/DoDisk
;		  a spherical shell of radius DoDisk is plotted (where integral is over two points,
;		  starting at x=DoDisk sphere, then x=DoDisk sphere+delta) 
;		  and then the limb slice at x=LOSoffset is plotted.
;		
;		LOSoffset button lives in FOV-POS widget.
;		used in FOR_POSNOINT and FOR_FORWARDMAP
;	---> GridPramsStruct tag LOSoffset (identical)
;
;       keyword PHIO (double -- DEGREES) -  allows a rotation of model structure in phi-- useful
;               for fitting data - thus the central meridian Cmer can be set to 
;		that appropriate for the day, and phio varied.
;		NOTE FOR MODEL CAVMORPH, BETTER TO USE MODEL PARAMETER PHCS!
;		In general, though, in the PlaneOfSky context, it is redundant with changing Cmer.
;		Should be thought of as a means to move the model structure
;		in longitude relative to a fixed Carrington grid.
;	---> GridPramsStruct tag Phio (identical but converted to radians)
;

 if strupcase(gridtype) eq 'PLANEOFSKY' then begin 
;
; check to see if coming from widget but not POS
;
	if notpos eq 1 then begin
	    if strupcase(modeluse) eq 'GIBBAGLOW' or strupcase(modeluse) eq 'NUMCUBE' then $
		maxheight = 3.d0 else maxheight=1.5d0 
	    if strupcase(modeluse) eq 'CROISSANT' or strupcase(modeluse) eq 'TOMO' then maxheight = 4.5d0
	    if strupcase(modeluse) eq 'CIRTOY' then maxheight = 100d0
	    if strupcase(modeluse) eq 'STRIA' then maxheight = 36d0
	    if strupcase(modeluse) eq 'SYNCOM' then maxheight = 10d0
	    if strupcase(modeluse) eq 'TURBHY' then maxheight = 36d0
	    if strupcase(line) eq 'EXPFAC' then maxheight = 2.5d0
;
; special case AZEQUI set
;  degrees elongation
;  CIRTOY will default set it, and so overwrite maxhiehgt now
;;  however-- not working at the moment in the widget - need to manually select
;  azequi
;
	    if strupcase(modeluse) eq 'CIRTOY' then default,azequi,1 $
              else default,azequi,0
	    if keyword_set(azequi) then maxheight=45.

	    phio=0.d0
            yymin=-maxheight
            yymax= maxheight
            xxmin=-maxheight
            xxmax= maxheight
            limb='Cmer'
	    if strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG' $
                or strpos(strupcase(instrument),'OVI') ge 0 $
                 or strpos(strupcase(instrument),'NEVIII') ge 0 $
                  or strpos(strupcase(instrument),'MGIX') ge 0 $ 
		  or strupcase(instrument) eq 'LYA' or strupcase(modeluse) eq 'TURBHY' $
	        then ngrid=64 else ngrid=256
	    if strupcase(line) eq 'EXPFAC' then ngrid=64
            if pos eq 0 then begin
              if strupcase(instrument) ne 'PHYSICAL DIAGNOSTICS' then begin
                losoffset = 'NULL' 
 	      endif else begin
		losoffset = 0.
	      endelse
	      if strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG' $
                  or strpos(strupcase(instrument),'OVI') ge 0 $
                  or strpos(strupcase(instrument),'NEVIII') ge 0 $
                  or strpos(strupcase(instrument),'MGIX') ge 0 $ 
                  or strupcase(instrument) eq 'LYA' $
		  or strupcase(modeluse) eq 'TURBHY' $
	          then ngrid=64 else ngrid=256
            endif else begin
                losoffset=0.d0
                ngrid=256
            endelse
            ngy=ngrid
        endif else begin
	  if strupcase(line) eq 'EXPFAC' then default,maxheight,2.5d0
	  if strupcase(modeluse) eq 'GIBBAGLOW' or strupcase(modeluse) eq 'NUMCUBE' then $
		maxheight = 3.d0 else maxheight=1.5d0 
	  if strupcase(modeluse) eq 'CROISSANT' or strupcase(modeluse) eq 'TOMO' then maxheight = 4.5d0
	  if strupcase(modeluse) eq 'CIRTOY' then maxheight = 100d0
	  if strupcase(modeluse) eq 'STRIA' then maxheight = 36d0
	  if strupcase(modeluse) eq 'SYNCOM' then maxheight = 10d0
	  if strupcase(modeluse) eq 'TURBHY' then maxheight = 36d0
;
; special case AZEQUI set
;
;  CIRTOY will default set it, and so overwrite maxhiehgt now
;;  however-- not working at the moment in the widget - need to manually select
;  azequi
;
          if strupcase(modeluse) eq 'CIRTOY' then default,azequi,1 $
              else default,azequi,0
	  if keyword_set(azequi) then maxheight=45.

  	  default,phio,0.d0
	  default,yymin,-maxheight
	  default,yymax, maxheight
	  if n_elements(limb) ne 0 then begin
            if strupcase(limb) eq 'WEST' then begin
		if n_elements(xxmin) ne 0  then $
	          if xxmin ge 0. then xxmin=xxmin else xxmin=0.d0
		if n_elements(xxmax) ne 0  then $
	          if xxmax gt 0. then xxmax=xxmax else xxmax=maxheight
 		default,xxmin,0.d0
	    endif
            if strupcase(limb) eq 'EAST' then begin
		if n_elements(xxmin) ne 0 then $
	          if xxmin lt 0. then xxmin=xxmin else xxmin=-maxheight
		if n_elements(xxmax) ne 0  then $
	          if xxmax le 0. then xxmax=xxmax else xxmax=0.d0
 		default,xxmax,0.d0
	    endif
	  endif
	  default,xxmax,maxheight
	  default,xxmin,-maxheight
          default,limb,'Cmer'
	  if strupcase(line) eq 'EXPFAC' then default,ngrid,64
	  if strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG' $
                  or strpos(strupcase(instrument),'OVI') ge 0 $
                  or strpos(strupcase(instrument),'NEVIII') ge 0 $
                  or strpos(strupcase(instrument),'MGIX') ge 0 $ 
                  or strupcase(instrument) eq 'LYA' $
		  or strupcase(modeluse) eq 'TURBHY' $
	          then default,ngrid,64 else default,ngrid,256
	  if ngrid lt 0 then begin
            if strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG' $
                  or strpos(strupcase(instrument),'OVI') ge 0 $
                  or strpos(strupcase(instrument),'NEVIII') ge 0 $
                  or strpos(strupcase(instrument),'MGIX') ge 0 $ 
                  or strupcase(instrument) eq 'LYA' $
		  or strupcase(modeluse) eq 'TURBHY' $
	          then ngrid=64 else ngrid=256
	    if strupcase(line) eq 'EXPFAC' then ngrid=64
          endif
	  default,ngy,ngrid
          if ngy lt 0 then ngy=ngrid
	  if n_elements(losoffset) eq 1 then if strupcase(string(losoffset)) eq 'NULL' then losoffset=0.d0
          if strupcase(instrument) ne 'PHYSICAL DIAGNOSTICS' then begin
            if pos eq 0 then losoffset='NULL' else default,losoffset,0.d0
          endif else begin
           default,losoffset,0.d0
	  endelse
	endelse
;
; these shouldn't matter, but just to set it for input/output structures
;
        rheight='NULL'
	ruser='NULL'
	thuser='NULL'
	phuser='NULL'
	coorduser='NULL'
 endif

;
;	Defaults needed for CarrMap
;
;       keywords XXMIN,XXMAX,YYMIN,YYMAX  (doubles -- DEGREES)
;		for now -- these are not changed (set in for_get_grid
;		and below to standard Carrington map, i.e.,
;		xxmin/xxmax=0/360. yymin/yymax=0/180.
;	keyword LIMB ('Cmer'/'East'/'West') -- which limb or central meridian
;	 		to extract slices from
;		Note - longitude in the maps refers to the central meridian longitude
;		but the data is "extracted" at the limb - so there is a +/- 90 degree
;		rotation in making the maps if East or West
;		limb default 'West'
;	keywords NGRID (integer)
;               ngrid default 180 (makes 2*NGrid,NGrid+fix(1./dang)
;		  except for EXPFAC then 45
;                 (phi,theta) grid -- for ngrid=180 dang will be just
;			under 1 so 181 degrees in latitude. Only will
;			be exactly ngrid degrees in latitude for lower resolution.
;	---> GridPramsStruct tags Limb (unchanged)
;	---> GridPramsStruct tags Dx,Dy 
;	---> GridPramsStruct tags XRange/YRange 
;	---> GridPramsStruct tags XCenter/YCenter
;	---> GridPramsStruct tags RPos/ThPos/PhPos
;
;       keyword RHEIGHT (double -- SOLAR RADII) - Height where data is extracted
;		used in defining r for Carrington map, in FOR_GET_GRID 
;		and FOR_MAKE_CARR_MAP and FOR_POS_MAP
;	---> GridPramsStruct tag RHeight (identical)
;
;       keyword LOSOFFSET (double -- SOLAR RADII) -- Offset from center point of LOS integration
;			(X=0 plane if LOSUSE=XLOS 
;			  Thomson Sphere if LOSUSE=TAU or LOSUSE=TLOS)
;		only useful for /POS 
;		default 'NULL' (unset)
;	---> GridPramsStruct tag LOSoffset (identical)
;
;       keyword PHIO (double -- DEGREES) -  allows a rotation of model structure in phi-- useful
;               for setting up Carrington map in fixed Carrington coords
;               and moving model relative to it. 
;		NOTE FOR MODEL CAVMORPH, BETTER TO USE MODEL PARAMETER PHCS (SAME EFFECT)!
;		So, if CMER is zero, but phio is 30, the map will plot from 0-360 (centered on 180) but the model
;		structure will be centered on 30. It is useful to set phio=180
;		because this will position the model structure right in the middle
;		of a map with default cmer=0 (set in LOS_DEFAULTS).  If fitting to data where a structure is positioned at
;		some particular place, e.g. again phio=30, one could then set 
;		cmer=210 and the map will be centered on the structure, but the labeling
;		will accurately reflect its position in Carrington longitude
;	---> GridPramsStruct tag Phio (identical but converted to radians)


 if strupcase(gridtype) eq 'CARRMAP' then begin

;
; check to see if coming from widget but not CARRMAP
;
	if notcarr eq 1 then begin
          phio=0.d0
          limb='West'
          rheight=1.05d0
          ngrid=180
	  if strupcase(line) eq 'EXPFAC' then begin
 	   rheight=2.5d0
	   ngrid=45
   	  endif
          losoffset='NULL'
	endif else begin
  	  default,phio,0.d0
          default,limb,'West'
          if strupcase(limb) ne 'WEST' and strupcase(limb) ne 'EAST' and strupcase(limb) ne 'CMER' then limb = 'West'
	  if strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG' $
;                  or strpos(strupcase(instrument),'OVI') ge 0 $
;                  or strpos(strupcase(instrument),'NEVIII') ge 0 $
;                  or strupcase(instrument) eq 'LYA' $
	          or strupcase(instrument) eq 'WL' or (strupcase(instrument) eq 'NONE' and strupcase(line) eq 'NONE') $
	            then if strupcase(limb) eq 'CMER' then limb='West'
	  if strupcase(line) eq 'EXPFAC' then begin
           default,rheight,2.5d0
	   default,ngrid,45
   	  endif
          if strupcase(limb) eq 'CMER' then begin
           default,rheight,1.0001d0
          endif
          default,rheight,1.05d0
          default,ngrid,180
	  if n_elements(losoffset) eq 1 then if strupcase(string(losoffset)) eq 'NULL' then losoffset=0.d0
          if strupcase(instrument) ne 'PHYSICAL DIAGNOSTICS' then begin
              if pos eq 0 then losoffset='NULL' else default,losoffset,0.d0
          endif else begin
             default,losoffset,0.d0
          endelse
	endelse

;
; these shouldn't matter, but just to set it for input/output structures
;
	ruser='NULL'
	thuser='NULL'
	phuser='NULL'
;        xxmin=0.d0
;        xxmax=360.d0
;        yymin=0.d0
;        yymax=180.d0
        ngy='NULL'
	xxmin='NULL'
	xxmax='NULL'
	yymin='NULL'
	yymax='NULL'
	coorduser='NULL'
 endif

;
; define structure containing "grid" inputs
;  to be used in widget interface
; 

 if strupcase(gridtype) eq 'CARRMAP' and (strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG' $
;                  or strpos(strupcase(instrument),'OVI') ge 0 $
;                  or strpos(strupcase(instrument),'NEVIII') ge 0 $
;                  or strupcase(instrument) eq 'LYA' $
		  or strupcase(instrument) eq 'WL' or $
                 (strupcase(instrument) eq 'NONE' and strupcase(line) eq 'NONE')) then LimbVal=['East','West'] else $
  LimbVal=['Cmer','East','West']
 if strupcase(gridtype) eq 'PLANEOFSKY' then LimbVal='nodisplay'

 PhioVal='double-DEGREES'

 NGyVal='integer'
 GridTypeVal=['PLANEOFSKY','CARRMAP']
 DistObsVal='double-RSUN'
 if strupcase(gridtype) eq 'PLANEOFSKY' then AzEquiVal='tog' else AzEquiVal='nodisplay'

; if pos gt 0 or strupcase(instrument) eq 'PHYSICAL DIAGNOSTICS' then begin
;  AzEquiVal='nodisplay'
;  DistObsVal='nodisplay'
; endif

endif else begin
 
;  DATA FROM HERE
;
; testing CARRMAP capability
; so far limiting widget visibility for this option to MLSO
; and actually for now just limiting to KCOR
;
;     if (strpos(strupcase(instrument),'OMP') ge 0 $
;	  or strupcase(instrument) eq 'KCOR') then mlsoyes=1 else mlsoyes=0
     if strupcase(instrument) eq 'KCOR' then mlsoyes=1 else mlsoyes=0
     if mlsoyes eq 1 then begin
      GridTypeVal=['PLANEOFSKY','CARRMAP']
     endif else GridTypeVal='nodisplay'

     if strupcase(gridtype) eq 'PLANEOFSKY' then begin
;
; check to see if coming from widget but not POS
;
      if notpos eq 1 then begin
	 ngrid=512
   	 ngy=ngrid
      endif else begin
 	default,ngrid,512
	default,ngy,ngrid
      endelse

      limb='NULL'
      rheight='NULL'
      LimbVal='nodisplay'
;
; these next will be set to image size
;  although can be overwritten by input
;
      default,xxmin,'NULL'
      default,xxmax,'NULL'
      default,yymin,'NULL'
      default,yymax,'NULL'
     endif
;
     if strupcase(gridtype) eq 'CARRMAP' then begin 
;
; check to see if coming from widget but not CARRMAP 
;
	if notcarr eq 1 then begin
          limb='West'
          rheight=1.3d0
;
; note ngrid is not used in DATA CARRMAP but we assign it here because it may in future
;
          ngrid=180
	endif else begin
          default,limb,'West'
          if strupcase(limb) ne 'WEST' and strupcase(limb) ne 'EAST' and strupcase(limb) ne 'CMER' then limb = 'West'
	  default,rheight,1.3
          default,ngrid,180
        endelse
        LimbVal=['East','West'] 
        ngy='NULL'
        xxmin='NULL'
        xxmax='NULL'
        yymin='NULL'
        yymax='NULL'
     endif

     distobs='NULL'
     azequi=0
     DistObsVal='nodisplay'
     AzEquiVal='nodisplay'
     PhioVal='nodisplay'
     NGyVal='nodisplay' 
;
; these shouldn't matter, but just to set for input/output structures
;
     losoffset='NULL'
     phio='NULL'
     ruser='NULL'
     thuser='NULL'
     phuser='NULL'
     coorduser='NULL'

endelse

if is_number(xxmin) then xxmin=double(xxmin)
if is_number(xxmax) then xxmax=double(xxmax)
if is_number(yymin) then yymin=double(yymin)
if is_number(yymax) then yymax=double(yymax)

;print,'maxheight',maxheight
;print,'xxmin,xxmax,yymin,yymax',xxmin,xxmax,yymin,yymax
;print,'model',modeluse
;print,'ngrid',ngrid
;print,'ngy',ngy
; print,'losoffset in for_griddefaults',losoffset
; print,'rheight in for_griddefaults',rheight
; print,'limb in for_griddefaults',limb

GridInputs={GridType:gridtype,GridTypeVal:GridTypeVal,RUser:ruser,RUserVal:'double-RSUN',ThUser:thuser,ThUserVal:'double-DEGREES',PhUser:phuser,PhUserVal:'double-DEGREES',CoordUser:coorduser,CoordUserVal:'string',$
	XXMin:xxmin,XXMinVal:'double-RSUN',XXMax:xxmax,XXMaxVal:'double-RSUN',YYMin:yymin,YYMinVal:'double-RSUN',YYMax:yymax,YYMaxVal:'double-RSUN',Limb:limb,LimbVal:limbval,NGrid:ngrid,NGridVal:'integer',NGy:ngy,NGyVal:NGyVal,$
        RHeight:rheight,RHeightVal:'double-RSUN',$
	AzEqui:azequi,AzEquiVal:azequival,$
	DistObs:distobs,DistObsVal:distobsval,$
	LOSOffSet:losoffset,LOSOffSetVal:'double-RSUN'}

;print,'for_griddefaults pos',pos
;print,'ngrid=',ngrid
;print,'ngy=',ngy
end
