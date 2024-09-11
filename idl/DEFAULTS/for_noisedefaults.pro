function for_noisedefaults,instrument,line,modeluse,donoise=donoise,aperture=aperture,norandom=norandom,integration=integration,modeffint=modeffint,modeffquant=modeffquant,efficiency=efficiency,background=background,errtrunc=errtrunc,tscope=tscope,resolution=resolution,gridtype=gridtype,ngrid=ngrid,ngy=ngy,xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,limb=limb,azequi=azequi,distobs=distobs,nowidgmess=nowidgmess

common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops


;
;
;  this program sets defaults for telescope-specific noise
;	So far just done for CoMP type instruments
;
;  Called by FOR_OBSDEFAULTS, FOR_DRIVE
;
;  Inputs - INSTRUMENT set in FOR_OBSDEFAULTS
;
;
;  Keywords:
;
; SEE BELOW AND OBSPRAMSSTRUCT FOR DESCRIPTION OF KEYWORDS
;
;  History:
;	Written 16-Nov-2013  Sarah Gibson
;   	Version 2.0 July 2014
;	Nov 2016 - got rid of 'dummy' line in call to for_griddefaults
;	 (appears to have reinstated later)
;	Feb 2017 - forced double in output structure to avoid conflicts in savemap
;	April 2018 -- updated keyword MINNOS - so that could choose value of
;	Signal to noise below which not to plot
;       September 2018 - updated resolution capability for Carrington maps
;       February 2021 -- added modeluse to call, and removed 'dummy' in call to for_griddefaults
;	   because some of the grid defaults depend on model type
;	July 2021 -- made changes to for_plotfits, for_drive, for_obsdefaults because
;		this function should not be called for DATA because it was breaking
;	 	when sent to griddefaults
;	Sept 2021 -- expanded conditional test for STOKESQOI etc
;		passing through azequi
;		passed through nowidgmess
;       Dec 2021 -- passed through distobs
;	    added CLARO specs
;	Jan 2022 -- changed name MINNOS to ERRTRUNC
;	May 2022 -- added default integrtion time for CLARO
;			updated efficiencey for CLARO
;	Sep 2022 -- updated modeffquant and fixed bug in efficiency for CLARO
;			(basically, was set to .1 if not newstart)
;	Oct 2022 -- added VELC (with help from Vema Panditi)
;		 cleaned up assignment of aperture, efficiency, background
;	Nov 2022 -- added ngrid,ngy,xxmin-max,yymin-max as 
;	keywords in call to for_griddefaults so that defaults 
;	would be applied appropriately and consistently
;	  also added limb in keyword to this routine and call to
;	  for_griddefaults for same reason
;       Dec 2022 - added NEVIII
;	Feb 2023 -- turned off NORANDOM toggle in widget 
;	Mar 2023 -- added check for EUVPOL to check for CLARO for modeffquant
;       May 2023 -- changed units of efficiency from percent to fraction (see for_sun_flux)
;	Sep 2023 -- commented out STOKESP because confusing
;       Feb 2024 -- added MGIX 
;               

obsstart=flag.obs

;  	DONOISE: whether or not to add noise
;

default,donoise,0

;
;  	TELESCOPE: COMP, COSMO, ATST,CLARO,EUVPOL
;		or OTHER
;	

if strupcase(instrument) eq 'LYA' then tscope='CLARO'
if strpos(strupcase(instrument),'OVI') ge 0 $
 or strpos(strupcase(instrument),'NEVIII') ge 0 or strpos(strupcase(instrument),'MGIX') ge 0 $
then tscope='EUVPOL'

;
; CLARO is just Lya, but just to have something
;


default,tscope,'COMP'

if strpos(strupcase(instrument),'OMP') lt 0 and strupcase(instrument) ne 'CORMAG' $ 
 and strpos(strupcase(instrument),'OVI') lt 0 $
  and strpos(strupcase(instrument),'NEVIII') lt 0 $
  and strpos(strupcase(instrument),'MGIX') lt 0 $
  and strupcase(instrument) ne 'LYA' $
then begin

  donoise=0
  aperture=0.d0
  efficiency=0.d0
  background=0.d0
endif else begin
  
 newstart=0
 if exist(aperture) eq 0 then newstart = 1 else begin
  if exist(efficiency) eq 0 then efuse=0 else efuse = efficiency
  if exist(background) eq 0 then bguse=0 else bguse= background
  if donoise eq 0 and aperture le 1.d0 and efuse eq 0 and bguse eq 0 then newstart=1
 endelse
;
; 	APERTURE - aperture of telescopes 
;	   units: centimeters
;
;	EFFICIENCY - expected efficiency of telescopes
;	   units: fraction
;
;	BACKGROUND - telescope location background
;	   units: ppm (parts per million)
;

 if strupcase(tscope) eq 'COMP' then begin
  apertureuse=20.d0
  efficiencyuse=0.05d0
  backgrounduse=5.d0
 endif

 if strupcase(tscope) eq 'CORMAG' then begin
  apertureuse=20.d0
  efficiencyuse=0.05d0
  backgrounduse=5.d0
 endif

 if strupcase(tscope) eq 'COSMO' then begin
; check these
  apertureuse=150.d0
  efficiencyuse=0.05d0
  backgrounduse=5.d0
 endif

 if strupcase(tscope) eq 'ATST' or strupcase(tscope) eq 'DKIST' then begin
;
; need to check these!!
;
  apertureuse=400.d0
  efficiencyuse=0.05d0
  backgrounduse=50.d0
 endif

 if strupcase(tscope) eq 'VELC' then begin
  apertureuse=19.5d0
  efficiencyuse=0.04d0
  backgrounduse=0.d0
 endif

 if strupcase(tscope) eq 'CLARO' then begin
  apertureuse=12.d0
  efficiencyuse=0.0074d0
  backgrounduse=0.d0
 endif

 if strupcase(tscope) eq 'OTHER' or strupcase(tscope) eq 'EUVPOL' then begin
  apertureuse=50.d0
  efficiencyuse=0.05d0
  backgrounduse=0.d0
 endif

 default,aperture,apertureuse
 default,efficiency,efficiencyuse
 default,background,backgrounduse
 if newstart eq 1 then begin
  aperture=apertureuse
  efficiency=efficiencyuse
  background=backgrounduse
 endif

endelse

;
; 	INTEGRATION: time generally supplied by user
;		units: seconds
;

default,integration,300.d0

;
; 	MODEFFINT, MODEFFQUANT - efficiency associated with Modulation Cycle for Stokes I, Quantity
;	  This will affect the noise calculation in FOR_PERSONALITY
;	  and the total number of photons gathered by a telescope
;	  as determined in FOR_FIXUNITS
;         See Tomczyk technical note, pages 5 and 6
;         noise is divided by this, so it represents sqrt(moddutycycle)
;         where moddutycycle is the fraction of modulation cycle time applicable
;         e.g. for CoMP, standard modulation cycle is
;         (Io+Qo, Io-Qo, Io+Uo, Io-Uo, Io+Vo, Io-Vo)
;         so moddutycycle for I is 1, moddutycycle for Q,U,V is 1/3
;         and MODEFFINT = 1, MODEFFQUANT is 1/sqrt(3)
;         but even for CoMP, one might want to choose
;               MODEFFQUANT=MODEFF=1 e.g. for [Io+Vo,Io-Vo]);
;               or MODEFF=1, MODEFFQUANT=1/sqrt(2) for (Io+Qo, Io-Qo, Io+Uo, Io-Uo)
;         unitless


if strupcase(tscope) ne 'OTHER' then begin
 default,modeffint,1. 
 if (strupcase(tscope) ne 'CLARO' and strupcase(tscope) ne 'EUVPOL') then meq=1./sqrt(3.) else meq=1./sqrt(2.)
 Case 1 of
        strupcase(line) eq 'STOKESQ' or $
         strupcase(line) eq 'STOKESU' or $
         strupcase(line) eq 'STOKESV' or $
         strupcase(line) eq 'STOKESW' or $
         strupcase(line) eq 'STOKESL' or $
;         strupcase(line) eq 'STOKESP' or $
; removing because confusing (P later means L/I) and only there for limited backward compatability
         strpos(strupcase(line),'AZ') ge 0 or $
         strpos(strupcase(line), 'LOI') ge 0 or $
         strpos(strupcase(line), 'QOI') ge 0 or $
         strpos(strupcase(line), 'UOI') ge 0 or $
;         strpos(strupcase(line), 'POI') ge 0 or $
         strpos(strupcase(line), 'VOI') ge 0 : begin
		default,modeffquant,meq
 	 	if obsstart eq 2 then begin
                 modeffquant=meq
		 widgregen=1
		 if n_elements(flag) gt 0 then flag.widgregen=widgregen
                endif
	end
	else: begin
		default,modeffquant,modeffint
 	 	if obsstart eq 2 then begin
                 modeffquant=modeffint
		 widgregen=1
		 if n_elements(flag) gt 0 then flag.widgregen=widgregen
                endif
	end
 endcase
endif else begin
 default,modeffint,1.
 default,modeffquant,modeffint
endelse

;
; 	RESOLUTION - resolution of telescopes
;	   units: arcsec
;   NOTE THIS NEEDS TO BE SET BY FORWARD MODEL PIXEL SIZE
;   SO IS FORCED HERE (CANNOT BE CHANGED WITHOUT CHANGING NX,NY)
;   EXCEPT IF CALLED FROM 
;   FOR GRIDTYPE USER, WHICH HAS NO NX, NY
;   SO IN THAT CASE IT ACTS AS A STANDARD KEYWORD
;   (OR DATA CALLED FROM FOR_PLOTFITS, WHERE RESOLUTION
;   DEPENDS ON ACTUAL OBSERVATION)
;

if exist(gridtype) then if strupcase(gridtype) eq 'USER' then begin
;
; default for user input is 2 arcsecond, but this can be overruled
; by setting keyword resolution
;
 default,resolution,2.
endif

if keyword_set(gridtype) eq 0 or keyword_set(ngrid) eq 0 or keyword_set(ngy) eq 0 or n_elements(xxmin) eq 0 or n_elements(xxmax) eq 0 or n_elements(yymin) eq 0 or n_elements(yymax) eq 0 then begin
 for_griddefaults,modeluse,0,instrument,line,ngrid=ngrid,ngy=ngy,xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,limb=limb,gridtype=gridtype,GridInputs=GridTemp,azequi=azequi,distobs=distobs,nowidgmess=nowidgmess
; these lines below no longer needed because passing
;	keywords through
;       Need to be careful that inputs are not overwritten
;
; default,xxmin,GridTemp.xxmin
; default,xxmax,GridTemp.xxmax
; default,yymin,GridTemp.yymin
; default,yymax,GridTemp.yymax
; default,ngrid,GridTemp.ngrid
; default,ngy,GridTemp.ngy
endif

if is_number(xxmin) then xxmin=double(xxmin)
if is_number(xxmax) then xxmax=double(xxmax)
if is_number(yymin) then yymin=double(yymin)
if is_number(yymax) then yymax=double(yymax)

if is_number(xxmin) and is_number(ngy) then begin

;
; note this default only makes sense 
; for plane of sky map, and not data
;

 resolutionx = 959.63d0*abs(double(xxmax)-double(xxmin))/ngrid
 resolutiony = 959.63d0*abs(double(yymax)-double(yymin))/ngy

;
; when a save set is read, for_widget case RESET=4, the save set resolution
; is passed through explicitly (and is not zero), so resolution does not change
; w.r.t. xx/yy min/max and ngrid/ngy
; NOTE it is possible that this will not be the same
; as resolutionx>resolutiony if there has been a zoom in
; but no recalculation
;
; for other RESET values in call to for_widget resolution will not be passed through,
;  so it will be changed based on grid spacing
; (default if not set)
;
; if changes made to plot widget, first time call to obs_defaults, 
;  resolution is passed through without grid info and unchanged. 
; But, if there is grid info change, obs_defaults is called again and 
;  resolution set to zero so overwritten (even if DONOISE not set)
; 
; for_obsdefaults is also called from for_widget_event under various circumstances
; (e.g., switching to/from RADIO,COMP)
; in these cases resolution is passed through without grid info and unchanged
;
;

 if keyword_set(resolution) eq 0 then resolution=resolutionx>resolutiony else begin
  if resolution eq 0 then resolution=resolutionx>resolutiony 
 endelse

endif
 
;
; now take care of Carrington maps
;
if is_number(ngrid) eq 1 and is_number(ngy) eq 0 then begin
;
; latitude pixel size in degrees
;
   dlat=180.d0/double(ngrid+fix(ngrid/180)-1) 
;
; the resolution in latitude (polar angle) is uniform in arcsec 
;  and is the appropriate value to use in the noise calculation
;  the resolution in longitude will be the same in degrees at in latitude
;  and effectively represent the cadence of observations making up the 
;  carrington map. The integration time should be <= this cadence,
;  or integration(units fraction of day)*13 <= dlat
;
    C=959.63d0*2.d0*!dpi
    resolution=dlat*C/360.d0
endif

default,resolution,0.d0

; ERRTRUNC
;               a threshold on error to plot. 
;                 If positive, it is relative error, so points of SNR<1/ERRTRUNC
;                       won't be plotted as long as NULLDATCOLOR is set
;                 If negative, it is absolute error, so points of photon noise<ERRTRUNC
;                       won't be plotted as long as NULLDATCOLOR is set
;               (default, 1)
;

default,errtrunc,1.

errtruncval='double'

; 
;       keyword NORANDOM (integer) -- whether to add a random noise IF
;               photon noise relative error has been calculated -- in DONOISE tab
;               default 0 (random noise).  Note if NULLDATACOOR is negative, even though no
;               random noise is plotted, its affect is evident in the avoidance of plotting "bad" data.

default,norandom,0
;norandomval='tog'
norandomval='nodisplay'
; turned off in widget because it was confusing -- possible to make
; plots with donoise on but no noise indicated. So in general the noise
; will always be shown as salt-and-pepper random scaled to noise level
; but from line command if for example one wants to set NULLDATACOLOR 
; negative one can set NORANDOM=1 and have the masking without the salt-and-pepper

return,{DoNoise:DoNoise,DoNoiseVal:'tog',Aperture:double(Aperture),ApertureVal:'double',Resolution:double(Resolution),ResolutionVal:'nodisplay',Integration:double(Integration),IntegrationVal:'double',ModEffInt:double(ModEffInt),ModEffIntVal:'double',ModEffQuant:double(ModEffQuant),ModEffQuantVal:'double',Efficiency:double(Efficiency),EfficiencyVal:'double',Background:double(Background),BackgroundVal:'double',ErrTrunc:double(ErrTrunc),ErrTruncVal:errtruncval,Tscope:Tscope,$
;TscopeVal:['COMP','COSMO','ATST']}
TscopeVal:'nodisplay',$
NoRandom: norandom,NoRandomVal:norandomval}

end
