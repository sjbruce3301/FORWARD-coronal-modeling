pro for_obsdefaults,magmod,modeluse,rfilter=rfilter,ngrid=ngrid,ngy=ngy,xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,limb=limb,gridtype=gridtype,$
        line=line,instrument=instrument,pos=pos,labelonly=labelonly,obsloslimit=obsloslimit,frequency_MHz=frequency_MHz,dogyro=dogyro,fcor=fcor,ulimb=ulimb,rotaz=rotaz,wavelength_Ang=wavelength_Ang,wavelength2_Ang=wavelength2_Ang,numion=numion,$
abundance=abundance,cversion=cversion,UserSpecFiles=UserSpecFiles,$
        ioneq=ioneq,UserTResp=UserTResp,SpecPrams=SpecPrams,LWidth=LWidth,LLim=LLim,InGofNT=InGofNT,OutGofNT=OutGofNT,$
	nangleint=nangleint,collkey=collkey,isotropic=isotropic,wpar_open=wpar_open,wpar_closed=wpar_closed,aniso_open=aniso_open,$
	chromorad=chromorad,blend=blend,einsteina=einsteina,einsteinb=einsteinb,gj=gj,$
	xrt=xrt,eit=eit,wl=wl,cds=cds,iris=iris,myspect=myspect,iondens=iondens,eis=eis,ovi1032=ovi1032,ovi1037=ovi1037,neviii770=neviii770,neviii780=neviii780,mgix706=mgix706,lya=lya,radio=radio,faraday=faraday,aia=aia,$
        euvia=euvia,euvib=euvib,trace=trace,swap=swap,kcor=kcor,cormag=cormag,$
        comp=comp,fe11comp=fe11comp,si9comp=si9comp,othercomp=othercomp,si10comp=si10comp,greencomp=greencomp,$
        swss=swss,losem=losem,colden=colden,benergy=benergy,b_pos_int=b_pos_int,b_pos_dens_int=b_pos_dens_int,ben_dens_int=ben_dens_int,b_int=b_int,b_dens_int=b_dens_int,pop2losem=pop2losem,pop2colden=pop2colden,$
	donoise=donoise,seespec=seespec,errtrunc=errtrunc,aperture=aperture,norandom=norandom,resolution=resolution,integration=integration,modeffint=modeffint,modeffquant=modeffquant,$
	efficiency=efficiency,background=background,tscope=tscope,NoisePrams=NoisePrams,$
	PITeamResp=PITeamResp,ObsInputs=ObsInputs,$
        FCompPrams=FCompPrams,seecomp=seecomp,tp2te=tp2te,for_wlmin=for_wlmin,for_wlmax=for_wlmax,azequi=azequi,distobs=distobs,$
	smalln=smalln,qnorm=qnorm,cecoeff=cecoeff,icoll=icoll,isum=isum,isplin=isplin,$
	iwatom=iwatom,iwline=iwline,vintchoice=vintchoice,iweqi=iweqi,idebug=idebug,fiwatmo=fiwatmo,crtn=crtn,noftran=noftran,$
	pop2abundance=pop2abundance,pop2ionfrac=pop2ionfrac,pop2tregime=pop2tregime,pop2on=pop2on,working_dir=working_dir,nowidgmess=nowidgmess

common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops

;
; Name: for_obsdefaults

;
; Purpose: This program sets defaults for the forward calculation observables
;                               although details are set in FOR_OBS_NAME
;	
;  Inputs: MAGMOD, MODELUSE defined in FOR_MODELDEFAULTS
;	 NGRID,NGY,XXMIN,XXMAX,YYMIN,YYMAX
;		used in FOR_NOISEDEFAULTS
;
;  Keywords defined below
;
;  Optional output keyword ObsInputs gathers all keyword inputs
;       useful for widget setup
;
;  Called by FOR_DRIVE, FOR_WIDGET_EVENT, FOR_WIDGET_PLOT_EVENT, FOR_WIDGET
;		FOR_WIDGET_MODEL_EVENT, FOR_WIDGET_PRINT_COMMAND
;  Calls FOR_COMPDEFAULTS, FOR_NOISEDEFAULTS, FOR_SPECDEFAULTS
;
;  History:
;       Written Sarah Gibson, Terry Kucera 2013
;       24-Sept-2013 Added instruments keyword to for_specdefault call
;       16-Nov-2013 added call to for_noisedefaults and keywords
;       31-Dec-2013 Added LWidth, LLim, InGofNT, OutGofNT keywords to for_specdefault call. 
;                           Removed ARegion. TAK
; 	Version 2.0 July 2014
;       13-Jan-2015 - added pop2colden, pop2losem, pop2abundance,pop2tregime and pop2ionfrac keywords. TAK
;       Feb6-2016 - updated pop2 SEG
;		added POP2ON -- top level widget interface to turn on and off
;		 POP2 -- not passed to FOR_DRIVE (redundant with POP2TREGIME)
;	        also put in checks for POP2MODEL (currently CAVMORPH and NUMCUBE/ADAPTCUBE and PSIMAS and AWSOM)
;		and observables that support POP2
;		Also fixed some minor bugs in RFILTERVAL, XXMIN (affecting how
;		 PrintCommand printed) 
;      June 2016 -- updated pop2tregime -- moved from SpecPrams to ObsPrams structure
;	October 2017 -- added nangleint,isotropic,wpar_open,wpar_closed,aniso_open,aniso_closed
;		hooks for UVMODEL
; Jan 2018: included check for instrument NONE as
; Mar 2018: sent 'dummy' instrument/line for DATA calls to ensure
;   FCompPrams is filled with defaults
;well as WL - SEG
;  April 2018-- updated MINNOS -- comments only in this subroutine
;  June 2018 -- added benergy
;  May 2019 -- added collkey; removed aniso_closed
;  August 2020 -- changed default filter for KCOR to gamma_filter
;  September 2020 -- added hooks for TOMO, CROISSANT
;  November 2020 -- added hook for visible spectrometer -- although be careful, not implemented yet!!
;  January 2021 -- added hook for STRIA, also added rpower_filter option
;  February 2021 -- added modeluse as part of call to for_noisedefaults
;		allowed rfilter for Physical Diagnostics, at least from line command for_drive
;  July 2021 -- made sure for_noisedefaults not called for DATA
;  Sept 2021 -- but expanded NoisePrams so that printcommand didn't list nonexistant tags
;	refined defaults set for INSTRUMENT='NONE' to be for INSTRUMENT and LINE='NONE' 
;	changed inst to instr in for_specdefaults call because of potential for confusion with system var
;	Passing through azequi
;	passing through nowidgmess
; Dec 2021 
;	passing through distobs
;	allowing noise calculation for UV
; Jan 2022
;	renamed MINNOS ERRTRUNC - because it can be either relative or absolute error, depending on sign
;	turned off POS for DATA
; Mar 2022
;	added hooks for TURBHY
; Oct-Nov 2022 
;        added some text about POS
;	also removed checks for non number POS -- should not happen
;	  and was overspecific to pos=1
;	added limb=limb to for_noisedefaults call and inputs to this routine
; Dec 2022
;	added NEVIII
; Jan 2023
;       restructured so abundance, LWidth (w_ang)
;       chromorad,blend, einsteina, einsteinb, and gj 
;       defaults are defined for UV/EUV here
;        (in SpecPrams not in FOR_UVMODEL.PRO).
;	moved lambda into wavelength_ang because that makes more sense than frequency_mhz
; Feb 2023
;	changed instr to instrument
; May 2023
;	added comment about collkey
;	changed units of efficiency from percent to fraction (see for_sun_flux)
; June 2023 -- added SYNCOM hooks
; July 2023 -- added IONDENS hooks
; Sept 2023 -- added AWSOM hooks
; Oct 2023- passed through line to for_specdefaults
;
; Feb 2024
;      added MGIX 
;      fixed problem where DATA had DoNoiseVal dummy assigned without Val causing problems
;	when selecting SAVE
;	no longer reset rfilter with readmap
;       Jun 2024 -- added BPOS column variables
; 	Jul 2024 -- added CIRTOY hooks
;			passed through ulimb
;	Aug 2024 -- added WLRAT hooks

widgregen=flag.widgregen
datastart=flag.dta
modelstart=flag.mdl
mapstart=flag.rmp
obsstart=flag.obs

;
;   inputs to FOR_OBS_NAME (called by FOR_DRIVE and FOR_GETSTRUCTS)
;
;       keyword LINE - (string or number) see FOR_OBS_NAME for options, including
;               observables and plane-of-sky model parameters and diagnostics.
;		Most observables dealt with in FOR_INTENSINT or FOR_POSNOINT;
;		special case Ninst (torus instability power law slope)
;		dealt with in FOR_FORWARDMAP.  Default PB if neither line nor 
;		instrument set; If instrument is set but not line there is a 
;		different default for each instrument. Defaults set in FOR_OBS_CHOOSE,
;		in order to indicate no choice set here as 'NONE'
;	---> ObsPramsStruct tags LineName, LineNum, and Label
;

default,line,'NONE'

;
;       keyword INSTRUMENT (string) see FOR_OBS_NAME for options
;		Also can be called via toggles -- so /eit and instrument='eit'
;		are equivalent. Default 'WL' (white light) 
;		for cases where neither line nor instrument is set;
;		Otherwise line can be set but not instrument as long as 
;		the line is a non-observable quantity (e.g., POS parameter, instrument='physical diagnostics';). 
;		Defaults set in FOR_OBS_CHOOSE, in order to indicate no choice set here as 'NONE'
;		Two things can happen -- INSTRUMENT will go to 'physical diagnostics' if there's a line assigned
;		Or INStRUMENT will go to WL if not
;	---> ObsPramsStruct tags Instrument and Label
;

default,instrument,'NONE'

if strupcase(instrument) eq 'GREENCOMP' then instrument='CORMAG'

;
; but take care of case where CAVMORPH or TOMO or CROISSANT or CIRTOY is switched to
; in case a magnetic observable is being used last
;

if flag.mdl eq 1 and (strupcase(modeluse) eq 'CAVMORPH' or strupcase(modeluse) eq 'TOMO' or strupcase(modeluse) eq 'CROISSANT' or strupcase(modeluse) eq 'CIRTOY') then begin
   line='NONE'
   instrument='NONE'
endif

;
; 	keyword POS (1 or 0)  forces a plane of sky calculation for observables
;		so integration is only over one LOS pixel (done in FOR_FORWARDMAP. 
;		Also if Physical Diagnostic will be set to -1 
;		This happens in FOR_OBS_NAME
;		Also if set, will change to +/-2 if TLOS set
;		- default for polarized quantities or if azequi set
;		This happens in FOR_LOSDEFAULTS
;		Note pos value impacts losoffset in FOR_GRIDDEFAULTS
;		Starts out with default 0
;		Should never be 'NULL' unless in old save file
;	---> ObsPramsStruct tag Pos (identical unless changed as above)
;

default,pos,0.0
PosVal='tog'
if strupcase(string(pos)) eq 'NULL' then pos=0.0
;
; no longer setting DATA to 'NULL'- using zero
;if strupcase(modeluse) eq 'DATA' then pos='NULL'
if strupcase(modeluse) eq 'DATA' then begin
  pos=0.0
  posval='nodisplay'
endif

;
;       keyword LABELONLY (0/1) 
;		if set, ObsPramsStruct will only have a plot label in it
;

default,labelonly,0

;
; 	keyword OBSLOSLIMIT (double-RSUN)
;		if set, and integrating in constant TAU along LOS
;		  then LOS integration will not go past this radial
;		  height for LOS intersecting the photosphere.  
;		Eventually can customize each type of observable.  
;		Right now, default zero
;		  will mean that LOS limits will NOT be changed,
;		  but user can overwrite to implement this.
;		will change (in for_intensint)
;                 NLOS and LOSMIN keeping resolution fixed
;

default,obsloslimit,0.d0
;
; removed setting to NULL for data because causes type error. Use 0.
;if strupcase(string(obsloslimit)) eq 'NULL' then obsloslimit=0.d0
;if strupcase(modeluse) eq 'DATA' then obsloslimit='NULL' else begin
if strupcase(modeluse) eq 'DATA' then obsloslimit=0.d0 else begin
 if obsloslimit gt 0. and obsloslimit le 1. then begin
  obsloslimit=0.d0
 endif
endelse

;
; 
; if instrument RADIO or FARADAY have choice of frequency 
;
;       keyword FREQUENCY_MHz
;
; if RADIO then also whether to include gyro
;       keyword DoGyro
;
; if EUV spectrogram instrument (EIS, CDS, IRIS, MYSPECT, IONDENS)  have choice of wavelength
;   or (when implemented) visible spectrometer (SWSS)
;       keyword WAVELENGTH_ANG (Angstroms)
;   and also ion number
;       keyword NUMION (integer)
;  (note element is assigned in Observables drop-down menu)
;

;
; just in case instrument is passed in as key word
;

if keyword_set(cds) then instrument='CDS'
if keyword_set(myspect) then instrument='MYSPECT'
if keyword_set(iondens) then instrument='IONDENS'
; special case, someone inputs line of IONDENS
if strupcase(line) eq 'IONDENS' then begin
 instrument='IONDENS'
 line='NONE'
endif
if keyword_set(iris) then instrument='IRIS'
if keyword_set(eis) then instrument='EIS'
if keyword_set(radio) then instrument='RADIO'
if keyword_set(faraday) then instrument='FARADAY'
if keyword_set(xrt) then instrument='XRT'
if keyword_set(eit) then instrument='EIT'
if keyword_set(wl) then instrument='WL'
if keyword_set(aia) then instrument='AIA'
if keyword_set(euvia) then instrument='EUVIA'
if keyword_set(euvib) then instrument='EUVIB'
if keyword_set(trace) then instrument='TRACE'
if keyword_set(swap) then instrument='SWAP'
if keyword_set(kcor) then instrument='KCOR'
if keyword_set(cormag) then instrument='CORMAG'
if keyword_set(ovi1032) then instrument='OVI1032'
if keyword_set(ovi1037) then instrument='OVI1037'
if keyword_set(neviii770) then instrument='NEVIII770'
if keyword_set(neviii780) then instrument='NEVIII780'
if keyword_set(mgix706) then instrument='MGIX706'
if keyword_set(lya) then instrument='LYA'
if keyword_set(comp) then instrument='COMP'
if keyword_set(fe11comp) then instrument='FE11COMP'
if keyword_set(si9comp) then instrument='SI9COMP'
if keyword_set(si10comp) then instrument='SI10COMP'
if keyword_set(othercomp) then instrument='OTHERCOMP'
if keyword_set(greencomp) then instrument='CORMAG'
if keyword_set(swss) then instrument='SWSS'
if keyword_set(losem) then instrument='LOSEM'
if keyword_set(colden) then instrument='COLDEN'
if keyword_set(benergy) then instrument='BENERGY'
if keyword_set(ben_dens_int) then instrument='BEN_DENS_INT'
if keyword_set(b_int) then instrument='B_INT'
if keyword_set(b_dens_int) then instrument='B_DENS_INT'
if keyword_set(b_pos_int) then instrument='B_POS_INT'
if keyword_set(b_pos_dens_int) then instrument='B_POS_DENS_INT'
if keyword_set(pop2losem) then instrument='POP2LOSEM'
if keyword_set(pop2colden) then instrument='POP2COLDEN'

if strupcase(instrument) eq 'SWSS' then begin
 print,'******************************************************'
 print,'******************************************************'
 print,'******************************************************'
 print,'******************************************************'
 print,'Spectral Lines including scattering not yet implemented'
 print,'Except via COMP, CORMAG spectropolarimetric instruments'
 print,'Which require Fortran installation.'
 print,'Please choose a different instrument'
 print,'******************************************************'
 print,'******************************************************'
 print,'******************************************************'
 print,'******************************************************'
 message,'FORWARD halted.'
endif

if strupcase(modeluse) ne 'DATA' and strupcase(instrument) eq 'KCOR' then instrument='WL'

;  keyword POP2ON - turns on and off population 2 - needs to be on
;	for POP2COLDEN or POP2LOSEM, and for POP2TREGIME (see below) to be
;	visible on widget in SPECPRAMS.  Note, if the model does not have
;	Pop2 capability (controled via variable pop2model below), POP2ON
;	will be set to zero, and with it POP2TREGIME
;

pop2model=0
if strupcase(modeluse) eq 'CAVMORPH' or strupcase(modeluse) eq 'NUMCUBE' or strupcase(modeluse) eq 'ADAPTCUBE' or strupcase(modeluse) eq 'PSIMAS' or strupcase(modeluse) eq 'AWSOM' then pop2model=1
;
; some observations dont really have meaningful pop2 calculations,
; (in future might allow some of these for pop2tregime=3
; which makes density small in the integral so should work for most of these
; although may still be a problem for radio)
;
if strupcase(instrument) eq 'RADIO' or strupcase(instrument) eq 'FARADAY' or strpos(strupcase(instrument),'OMP') gt 0 or strupcase(instrument) eq 'CORMAG' $
  or strpos(strupcase(instrument),'OVI') ge 0 $
   or strpos(strupcase(instrument),'NEVIII') ge 0 $
   or strpos(strupcase(instrument),'MGIX') ge 0 $
   or strupcase(instrument) eq 'LYA' $
  then pop2model=0

if exist(pop2on) then pop2onold=pop2on else pop2onold='notset'
if is_number(pop2onold) eq 0 and keyword_set(pop2tregime) then pop2on=1
if pop2model eq 1 then begin
 if strpos(strupcase(instrument),'POP2') ge 0 then begin
  if exist(pop2on) then begin
   if pop2on eq 0 then pop2on=1 
  endif else pop2on=1
 endif else default,pop2on,0
 pop2onval=['0','1']
endif else begin
 pop2on=0
 pop2onval='nodisplay'
endelse

if string(pop2onold) ne string(pop2on) and strupcase(string(pop2onold)) ne 'NOTSET' then widgregen=1

if n_elements(flag) gt 0 then flag.widgregen=widgregen

for_obs_setup,magmod,modeluse,$
 all_betterinst=all_betterinst,all_names=all_names,phys_params=phys_params,all_lines=all_lines,all_types=all_types,pop2on=pop2on,working_dir=working_dir

insttypename=instrument+'_type'
if strupcase(instrument) ne 'NONE' and strupcase(instrument) ne 'PHYSICAL DIAGNOSTICS' then test=execute('insttype=all_types.'+insttypename) else insttype='NONE'

;
; WL has the option of turning on/off F corona
;	using Koutchmy-Lamy formula
;	keyword FCOR
;
; also set a default for limb darkening coefficient (see Billing chapter 6)
;  

if strupcase(instrument) eq 'WL' or (strupcase(instrument) eq 'NONE' and strupcase(line) eq 'NONE') then begin
  default,fcor,0
  FCorVal='tog'
  default,ulimb,0.63d0
  ULimbVal='double'
endif else begin
  fcor = 0
  FCorVal='nodisplay'
  ulimb=0.63d0
  ULimbVal='nodisplay'
endelse

;
; if spectropolarimetric instrument (e.g. CoMP) have
;               option of changing reference frame 
;		this will affect Q, U, and Azimuth
;		choices are radial (frame w.r.t. local vertical)
;		N-S (frame w.r.t sun's rotation axis)
;		E-W (frame w.r.t. equator
;
;       keyword ROTAZ

; if (strpos(strupcase(instrument),'OMP') gt 0 or strupcase(instrument) eq 'CORMAG' $
;     or strupcase(insttype) eq 'UV SPECTROPOLARIMETERS') $
;    and (strpos(strupcase(line),'Q') gt 0 or strpos(strupcase(line),'U') gt 0 or strpos(strupcase(line),'AZ') gt 0) then begin
if (strpos(strupcase(instrument),'OMP') gt 0 or strupcase(instrument) eq 'CORMAG'  $
     or strupcase(insttype) eq 'UV SPECTROPOLARIMETERS') $
     then begin
  default,rotaz,'radial'
  if strupcase(rotaz) eq 'NULL' then rotaz='radial'
  if (strpos(strupcase(line),'Q') gt 0 or strpos(strupcase(line),'U') gt 0 or strpos(strupcase(line),'AZ') gt 0) then begin
   RotAzVal=['radial','N-S','E-W','45']
  endif else RotAzVal='nodisplay'
endif else begin
  rotaz='NULL'
  RotAzVal='nodisplay'
endelse
;
;
; RADIO and FARADAY needs an extra parameter for frequency
; RADIO also needs DOGYRO for doing gyroresonance (as well as just Bremstrahlung)
; UV Spectrometers need wavelength and ion number
; unless built into line name
;
; CODEX like WL spectrograph needs two wavelengths for ratio
; so will include dummy Wavelength2_Ang


if strupcase(insttype) eq 'RADIO' or strupcase(insttype) eq 'UV/EUV SPECTROMETERS' or strupcase(insttype) eq 'UV SPECTROPOLARIMETERS' or strupcase(insttype) eq 'VISIBLE SPECTROMETERS' then begin

 if strupcase(insttype) eq 'UV SPECTROPOLARIMETERS' then begin
   if strupcase(instrument) eq 'LYA' then lambda=1215.67
   if strupcase(instrument) eq 'OVI1032' then lambda=1031.91
   if strupcase(instrument) eq 'OVI1037' then lambda=1037.61
   if strupcase(instrument) eq 'NEVIII770' then lambda=770.42
   if strupcase(instrument) eq 'NEVIII780' then lambda=780.39 
   if strupcase(instrument) eq 'MGIX706' then lambda=706.06
   Wavelength_AngVal='nodisplay'
   wavelength_Ang=lambda
   Wavelength2_AngVal='nodisplay'
   wavelength2_Ang=lambda
   frequency_MHzVal='nodisplay'
   frequency_MHz=0
   dogyro=0
   dogyroval='nodisplay'
   NumIonVal='nodisplay'
   numion=0
 endif else begin

  if strupcase(insttype) eq 'RADIO' then begin
    frequency_MHzVal='double'
    if strupcase(instrument) eq 'RADIO' then begin
     DoGyroVal='tog'
;    DoGyroVal='nodisplay'
     default,dogyro,0
     if dogyro eq 1 then default,frequency_MHz,1.d2 else default,frequency_MHz,1.d3
     if frequency_MHz eq 0 then if dogyro eq 1 then frequency_MHz=1.d2 else frequency_MHz=1.d3
     if dogyro eq 1 then begin
      pos=0
      PosVal='nodisplay'
     endif
     if strupcase(line) eq 'NONE' then line='I'
    endif else begin
     DoGyroVal='nodisplay'
     dogyro=0
     default,frequency_MHz,1.d3
     if frequency_MHz eq 0 then frequency_MHz=1.d3
     if strupcase(line) eq 'NONE' then line='FR'
    endelse
    NumIonVal='nodisplay'
    numion=0
    Wavelength_AngVal='nodisplay'
    wavelength_Ang=0
    Wavelength2_AngVal='nodisplay'
    wavelength2_Ang=0
  endif else begin
   if strpos(strupcase(instrument),'CDS') ge 0 then default,line,'O'
   if strpos(strupcase(instrument),'IRIS') ge 0 then default,line,'SI'
   if strpos(strupcase(instrument),'IONDENS') ge 0 then default,line,'NE'
   default,line,'FE'
   if strupcase(line) eq 'NONE' then begin
    line='FE'
    if strpos(strupcase(instrument),'CDS') ge 0 then line='O'
    if strpos(strupcase(instrument),'IRIS') ge 0 then line='SI'
    if strpos(strupcase(instrument),'IONDENS') ge 0 then line='NE'
   endif
;
; note, backward compatibility maintained for Don Schmit's FE12RATIO
;
   if strupcase(line) ne 'FE12DE' and strupcase(line) ne 'FE12RATIO' then begin
;
; for UV and VISIBLE SPECTROMETERS:
; note the line can be entered with just the element (e.g. FE), and ion number and
; wavelength entered as separate keywords or if not entered then falling to defaults
; (this is how the widget does it); or as a string
; containing all the information (easier from the command line). In the latter case,
; it will need to be unpacked. Either way, FOR_UNPACKLINENAME will put the element value (e.g., 'HE')
; into the variable ELEM.  Note that if LINE is entered as the full string containing
; NUMION and WAVELENGTH_ANG, but these are also set as keywords, the keywords will overwrite
; the full string of the entered line (so only the element will be preserved).
;
    for_unpacklinename,line,elem,numion_unpack,wavelength_Ang_unpack 

    elems=all_names.eis_names
;
; this is an array of just the element names
;
    line_defaults=all_lines.eis_lines
;
; this has defaults in the form 'FE12_195.119'
;
    welem=where(strupcase(elem) eq elems,c)
    if c eq 0 then begin 
       d=dialog(/WARNING,'Line not allowed, going to Fe default')
       elem='Fe'
       welem=where(strupcase(elem) eq elems,c)
    endif
    for_unpacklinename,line_defaults[welem],elem_def,numion_def,wavelength_Ang_def
    if strupcase(elem_def) ne strupcase(elem) then begin message,'problem with elements. This should not happen' & stop,line & endif
    if exist(numion_unpack) then numion_defuse=numion_unpack else numion_defuse=(numion_def)[0]
    if exist(wavelength_Ang_unpack) then wavelength_Ang_defuse=wavelength_Ang_unpack else wavelength_ang_defuse=(wavelength_ang_def)[0]
    default,wavelength_Ang,wavelength_Ang_defuse
    if double(wavelength_Ang) eq 0 then wavelength_Ang=wavelength_Ang_defuse
    default,numion,numion_defuse
    if numion eq 0 then numion=numion_defuse
    line=strupcase(elem)+strtrim(string(numion),2)+'_'+strtrim(string(wavelength_Ang),2)
   endif else begin
    numion=0
    wavelength_ang=0.d0
   endelse
   frequency_MHzVal='nodisplay'
   frequency_MHz=0
   dogyro=0
   dogyroval='nodisplay'
   Wavelength_AngVal='double'
   NumIonVal='integer'
   if strpos(strupcase(instrument),'IONDENS') ge 0 then PosVal='nodisplay'
  endelse
 endelse
endif else begin
  Wavelength_AngVal='nodisplay'
  wavelength_ang=4000
  Wavelength2_AngVal='nodisplay'
  wavelength2_ang=4100
  frequency_MHzVal='nodisplay'
  frequency_MHz=0
  dogyro=0
  dogyroval='nodisplay'
  NumIonVal='nodisplay'
  numion=0
endelse

if strupcase(line) eq 'WLRAT' then begin
   Wavelength_AngVal='double'
   default,wavelength_Ang,4000
   Wavelength2_AngVal='double'
   default,wavelength2_Ang,4100
endif

;
; maybe not necessary to show
;

;ObsLosLimitVal='double'
ObsLosLimitVal='nodisplay'

;
; if intensity have choice of radial filter
;

;print,'1'
;if n_elements(rfilter) ne 0 then print,instrument,line,rfilter

if keyword_set(rfilter) then begin
  if is_number(rfilter) then if rfilter eq 0 then rfilter='no_filter' else rfilter='NULL'
  if (strupcase(instrument) ne 'AIA' or strupcase(modeluse) ne 'DATA') and strupcase(rfilter) eq 'AIA_RFILTER' then rfilter='NULL'
  if strupcase(rfilter) eq 'NULL' or datastart eq 1 or datastart eq 2 or modelstart eq 1 $
; no longer reset rfilter with map
;   or  mapstart eq 1 
    or obsstart eq 1 then begin
    rfilter='no_filter'
    if strupcase(instrument) eq 'AIA' and strupcase(modeluse) eq 'DATA' then rfilter='aia_rfilter'
    if strupcase(instrument) eq 'SWAP' or strpos(strupcase(instrument),'EUVI') ge 0 then rfilter='quarterpower_filter'
;
    if strupcase(instrument) eq 'KCOR' then rfilter='gamma_filter'
    if strupcase(modeluse) eq 'STRIA' then rfilter='rpower_filter'
    if strupcase(modeluse) eq 'SYNCOM' then rfilter='rpower_filter'
    if strupcase(modeluse) eq 'TURBHY' then rfilter='rpower_filter'
;
; coronagraphs
;
    if strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG' or $
     strupcase(insttype) eq 'UV SPECTROPOLARIMETERS' or $
	strupcase(instrument) eq 'WL' or (strupcase(instrument) eq 'NONE' and strupcase(line) eq 'NONE') then rfilter='no_filter'
;
  endif
endif 
;
; only filter intensity
; NOTE - if AIA_RFILTER the data is explicitly changed
; other filters are just applied a posteriori within FOR_PLOT
;
;if is_number(pos) then if pos eq 1 then rfilter='NULL'

if strupcase(modeluse) eq 'STRIA' then default,rfilter,'rpower_filter'
if strupcase(modeluse) eq 'SYNCOM' then default,rfilter,'rpower_filter'
if strupcase(modeluse) eq 'TURBHY' then default,rfilter,'rpower_filter'
if strupcase(instrument) eq 'NONE' then default,rfilter,'no_filter'
; if strupcase(instrument) eq 'PHYSICAL DIAGNOSTICS' then rfilter='NULL'
if (strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG' $
     or strupcase(insttype) eq 'UV SPECTROPOLARIMETERS') $
     and ((strupcase(line) ne 'STOKESI') and strupcase(line) ne 'STOKESL' and strupcase(line) ne 'STOKESQ' and strupcase(line) ne 'STOKESU' and strupcase(line) ne 'I_KCOR' and strupcase(line) ne 'L_KCOR') then rfilter='NULL'
; note I_KCOR and L_KCOR are CORMAG lines
if strupcase(instrument) eq 'RADIO' and strupcase(line) ne 'STOKESI' then rfilter='NULL'
if strupcase(instrument) eq 'FARADAY' then rfilter='NULL'
if (strupcase(instrument) eq 'WL' or (strupcase(instrument) eq 'NONE' and strupcase(line) eq 'NONE')) $
   and strupcase(line) eq 'P' then rfilter='NULL'
;
if strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG' or $
     strupcase(insttype) eq 'UV SPECTROPOLARIMETERS' or $
   strupcase(instrument) eq 'WL' then default,rfilter,'no_filter'
if strupcase(instrument) eq 'AIA' and strupcase(modeluse) eq 'DATA' then default,rfilter,'aia_rfilter'
if strupcase(instrument) eq 'SWAP' or strpos(strupcase(instrument),'EUVI') ge 0 then default,rfilter,'quarterpower_filter'
if strupcase(instrument) eq 'KCOR' then default,rfilter,'gamma_filter'
default,rfilter,'no_filter'

if strupcase(rfilter) eq 'NULL' then flag.rfil=0
if strupcase(rfilter) eq 'NO_FILTER' then flag.rfil=0
if strupcase(rfilter) eq 'AIA_RFILTER' then flag.rfil=1
if strupcase(rfilter) eq 'NGRF_FILTER' then flag.rfil=2
if strupcase(rfilter) eq 'QUARTERPOWER_FILTER' then flag.rfil=3
if strupcase(rfilter) eq 'GAMMA_FILTER' then flag.rfil=4
if strupcase(rfilter) eq 'RPOWER_FILTER' then flag.rfil=5
                          
;print,'2'
;print,instrument,line,rfilter

;
; define structure containing information about spectral properties
;  and temperature response
;
;  keyword SPECPRAMS -- this is the structure containing the keywords to follow
;	as tags; it is an output of this code that becomes a tag within OBSPRAMSSTRUCT,
;	and also within OBSINPUTS (used by FOR_WIDGET to populate call to FOR_DRIVE).
;	SPECPRAMS can also be passed in as a keyword itself via FOR_DRIVE (only
;	can happen in command-line interface, not widget). In this case, 
;	values of its tags are used as keywords if not otherwise set.
;
;  keyword SEESPEC - used by widget to toggle visibility of other keywords (note
;	not all are necessarily displayed)
;
;  keyword POP2TREGIME - temperature regime for a second population if defined,
;     if set to 1, treats second population as coronal, and 
;	can have different POP2ABUNDANCE
;     if set to 2, treats second population as chromospheric and applies
;	Lyman absorption (POP2IONFRAC input)
;     if set to 3, removes second population from display/integration
;     if set to 0 (default), second population treated no different than first 
;	(this will be forced for POP2ON=0, and vice verse).
;     Note pop2dens, pop2temp, pop2fillingfactor and fillingfactor are
;	defined within models, so far just in CAVMORPH and NUMCUBE/ADAPTCUBE and PSIMAS and AWSOM
;			
;  keyword ABUNDANCE - default to '' which is Chianti's 'coronal_ext'
;	unless insttype=IClass = UV SPECTROPOLARIMETERS
;	   in which case it is an ion specific number
;		(string of it to avoid type errors)
;  keyword POP2ABUNDANCE - default to ABUNDANCE
;
;  keyword CVERSION - Chianti version, default '' is most recent
;
;  keyword USERSPECFILES - array or scalar containing name(s) of genx files 
;    containing structures containing spectra. Each file corresponds to 
;	different density and contains spectra for a range of temperatures 
;	(see FOR_SPECPRAMS)
;
;  keyword IONEQ -- ion equilibrium assumptions -- default is 'Chianti'
;			NOTE -- if running for Coronal Polarimeters and
;			programs crashes -- make sure you have compiled
;			the latest forcomp fortran program
;  keyword POP2IONFRAC - ion fractions for population 2.
;    if POP2TREGIME = 2 then  a filename containing a structure., e.g. {H:0.7, He:[0.9,0.1]} will be needed.
;         Otherwise (pop2tregime ne 2) it will not be used and FORWARD will be assume POP2 will be treated with IONEQ
;  
;  keyword USERTRESP -- temperature response appropriate to the instrument
;	currently not used, default ''
;
;  keyword PITEAMRESP -- Default = 0 is AIA Team default (with ChiantiFix)
;
;  keyword LLIM - emissivity limit for which lines to include
;  keyword LWIDTH - linewidth for spectograph instruments.
;       lines +/- LWidth/2 will be included as blends
;	also used to store w_ang for UV
;  keyword InGofNT - Name of file containing Input GofNT structure
;  keyword  OutGofNT - Name of file containing Output GofNT structure
;
; (for UV)
;
;  keyword CHROMORAD -- chromospheric  radiation
;  keyword BLEND -- line blending factor due to transition
;  keyword EINSTEINA - Einstein coefficient for spontaneous emission (1/SEC)
;  keyword EINSTEINB - Einstein coefficient for absorption/stimulated emission (CM^2/SEC)
;  keyword GJ -  Lande factor
;  
;
;  keyword COLLKEY - whether to turn on collisions for UV calculation
;		(0: scattering only, 1: scat+coll, -1: coll only)
;  keyword ISOTROPIC - whether to do isotropic (1) or anisotropic (0) model (UV spectro)
;       If set, will overwrite any ModSolStruct.AnIso
;  keyword NANGLEINT - How many steps to take in solid angle in integrating over incoming radiation (UV spectro)
;  keyword WPAR_OPEN, WPAR_CLOSED -- multiplicative factors to establish W_PAR if not set in ModSolStruct (UV spectro)
;       multiply W_MIN which depends on temperature, ion maxx
;       If info on open vs closed not available, will assume all is closed
;  keyword ANISO_OPEN -- model choice for ANISO for OVI 
;	not used if set in ModSolStruct (UV spectro)
;       If info on open vs closed not available, will assume all is closed
;

;
; if model CAVMORPH, Pop2TRegime will be forced by parameter choice
; so can't be set by user 
; and should not appear in widget
; this will communicate this to FOR_SPECDEFAULTS
;

if pop2on eq 1 then begin
 default,pop2tregime,2
 if pop2tregime eq 0 then pop2tregime=2
endif 

if not keyword_set(pop2tregime) or pop2on eq 0 then begin
      pop2tregimeval='nodisplay'
      pop2tregime=0
endif else begin
;
; for CAVMORPH - don't want to display on widget
;
 if strupcase(modeluse) eq 'CAVMORPH' then begin
       pop2tregimeval='nodisplay'
 endif else pop2tregimeval=['1','2','3']
;
endelse

;
; put in a check for DATA so don't copy IONEQ unnecessarily

if strupcase(modeluse) eq 'DATA' then ioneq='data'

if pop2on eq 1 then $
      SeeSpecInputs=for_specdefaults(SpecPrams,seespec=seespec,abundance=abundance,working_dir=working_dir,$
          cversion=cversion,UserSpecFiles=UserSpecFiles,ioneq=ioneq,UserTResp=UserTResp,PITeamResp=PITeamResp,$
	  nangleint=nangleint,collkey=collkey,isotropic=isotropic,wpar_open=wpar_open,wpar_closed=wpar_closed,aniso_open=aniso_open,$
          chromorad=chromorad,blend=blend,einsteina=einsteina,einsteinb=einsteinb,gj=gj,$
          LWidth=LWidth,LLim=LLim,InGofNT=InGofNT,OutGofNT=OutGofNT,line=line,instrument=instrument,IClass=insttype,$
          pop2abundance=pop2abundance,pop2ionfrac=pop2ionfrac,pop2tregime=pop2tregime) $
      else $
      SeeSpecInputs=for_specdefaults(SpecPrams,seespec=seespec,abundance=abundance,working_dir=working_dir,$
          cversion=cversion,UserSpecFiles=UserSpecFiles,ioneq=ioneq,UserTResp=UserTResp,PITeamResp=PITeamResp,$
	  nangleint=nangleint,collkey=collkey,isotropic=isotropic,wpar_open=wpar_open,wpar_closed=wpar_closed,aniso_open=aniso_open,$
          chromorad=chromorad,blend=blend,einsteina=einsteina,einsteinb=einsteinb,gj=gj,$
          LWidth=LWidth,LLim=LLim,InGofNT=InGofNT,OutGofNT=OutGofNT,line=line,instrument=instrument,IClass=insttype)


if strupcase(modeluse) ne 'DATA' and (strupcase(insttype) eq 'UV/EUV SPECTROMETERS' or strupcase(insttype) eq 'EUV/XRAY IMAGERS' or strupcase(insttype) eq 'VISIBLE SPECTROMETERS' or strupcase(insttype) eq 'UV SPECTROPOLARIMETERS') then begin
;if strupcase(modeluse) ne 'DATA' then begin
   SeeSpecInputsVal='structure'
endif else begin
   SeeSpecInputsVal='nodisplay'
endelse

;
; define structure containing information about noise properties
;
;  keyword NOISEPRAMS -- this is the structure containing the keywords to follow
;	as tags; it is an output of this code that becomes a tag within OBSPRAMSSTRUCT,
;	and also within OBSINPUTS (used by FOR_WIDGET to populate call to FOR_DRIVE).
;
;       DONOISE: whether or not to add noise
;
;       APERTURE - aperture of telescopes
;          units: centimeters
;
;       keyword NORANDOM (integer) -- whether to add a random noise IF
;               photon noise relative error has been calculated -- in DONOISE tab
;               default 0 (random noise).  Note if NULLDATACOOR is negative, even though no
;               random noise is plotted, its affect is evident in the avoidance of plotting "bad" data.
;
;       EFFICIENCY - expected efficiency of telescopes
;          units: fraction
;
;       BACKGROUND - telescope location background
;          units: ppm (parts per million)
;
;       INTEGRATION: time observed
;               units: seconds
;
;	MODEFFINT: modulation efficiency for Stokes I
;	MODEFFQUANT: modulation efficiency for Quantity
;	  (e.g. CoMP sequence [I+Q,I-Q,I+U,I-U,I+V,I-V] has MODEFFINT=1, MODEFFQUANT=1/sqrt(3)
;
;    Note -- RESOLUTION: pixel size
;               is NOT a keyword here, but rather
;               is established by choice of field-of-view
;                       (XXMIN,XXMAX,YYMIN,YYMAX)
;               and NGRID, NGY (grid spacing)
;               Change using FOV TAB below
;               it will be explicitly listed in
;               units of arc-seconds in plot title
;
;
;	ERRTRUNC 
;       	a threshold on error to plot. 
;		  If positive, it is relative error, so points of SNR<1/ERRTRUNC
;			won't be plotted as long as NULLDATCOLOR is set
;		  If negative, it is absolute error, so points of photon noise<ERRTRUNC
;			won't be plotted as long as NULLDATCOLOR is set
;		(default, 1)


if strupcase(modeluse) ne 'DATA' then $
  DoNoiseInputs=for_noisedefaults(instrument,line,modeluse,donoise=donoise,aperture=aperture,norandom=norandom,errtrunc=errtrunc,resolution=resolution,gridtype=gridtype,ngrid=ngrid,ngy=ngy,azequi=azequi,distobs=distobs,$
	integration=integration,modeffint=modeffint,modeffquant=modeffquant,efficiency=efficiency,background=background,tscope=tscope,xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,limb=limb,nowidgmess=nowidgmess) $
 else DoNoiseInputs={DoNoise:0.d0,DoNoiseVal:'nodisplay',Aperture:0.d0,ApertureVal:'nodisplay',Resolution:0.d0,ResolutionVal:'nodisplay',Integration:0.d0,IntegrationVal:'nodisplay',ModEffInt:0.d0,ModEffIntVal:'nodisplay',ModEffQuant:0.d0,ModEffQuantVal:'nodisplay',Efficiency:0.d0,EfficiencyVal:'nodisplay',Background:0.d0,BackgroundVal:'nodisplay',ErrTrunc:0.d0,ErrTruncVal:'nodisplay',Tscope:'NULL',TscopeVal:'nodisplay',NoRandom:0,NoRandomVal:'nodisplay'}

pos =double(pos)
;if (strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG') and strupcase(modeluse) ne 'DATA' and strpos(strupcase(line),'AZ') le 0 and strpos(strupcase(line),'OI') le 0 and strpos(strupcase(line),'DOPPLER') lt 0 and strpos(strupcase(line),'WIDTH') le 0 then begin
if (strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG')  $
 or strupcase(insttype) eq 'UV SPECTROPOLARIMETERS' $
and strupcase(modeluse) ne 'DATA' then begin
  DoNoiseInputsVal='structure' 
;  if strupcase(strtrim(string(pos),2)) eq '1.0000000' then begin
  if pos eq 1 then begin
    DoNoiseInputsVal='nodisplay'
    DoNoiseInputs.donoise=0
  endif
endif else begin
  DoNoiseInputsVal='nodisplay'
  DoNoiseInputs.donoise=0
endelse

; removed this line because now FLAG.NOISE is set in FOR_WIDGET_PLOT_EVENT
;  and it is unset as soon as it is used in FOR_PLOT_DEFAULTS
;
;if keyword_set(donoise) and flag.noise eq 0 then flag.noise=1 else flag.noise=0

;
; define structure containing FORCOMP program input
;
;  keyword FCOMPPRAMS -- this is the structure containing the keywords to follow
;	as tags; it is an output of this code that becomes a tag within OBSPRAMSSTRUCT,
;	and also within OBSINPUTS (used by FOR_WIDGET to populate call to FOR_DRIVE).
; 	It is used to write the INPUT file for FORCOMP.
;

SeeCompInputs=for_compdefaults(FCompPrams,instrument,line,tp2te=tp2te,for_wlmin=for_wlmin,for_wlmax=for_wlmax,smalln=smalln,qnorm=qnorm,cecoeff=cecoeff,isum=isum,icoll=icoll,isplin=isplin,iwatom=iwatom,iwline=iwline,vintchoice=vintchoice,iweqi=iweqi,idebug=idebug,fiwatmo=fiwatmo,noftran=noftran,seecomp=seecomp,crtn=crtn)

if (strpos(strupcase(instrument),'OMP') ge 0 or strupcase(instrument) eq 'CORMAG') and strupcase(modeluse) ne 'DATA' then begin
  SeeCompInputsVal='structure'
endif else begin
;
; fill with defaults
;
  SeeCompInputs=for_compdefaults(FCompPrams,'dummy','dummy',tp2te=tp2te,for_wlmin=for_wlmin,for_wlmax=for_wlmax,smalln=smalln,qnorm=qnorm,cecoeff=cecoeff,isum=isum,icoll=icoll,isplin=isplin,iwatom=iwatom,iwline=iwline,vintchoice=vintchoice,iweqi=iweqi,idebug=idebug,fiwatmo=fiwatmo,noftran=noftran,seecomp=seecomp,crtn=crtn)
  SeeCompInputsVal='nodisplay'
  SeeCompInputs.SeeComp=0
endelse

;rfilterval='tog'
;
; to do: make all filter choices available
;

if strupcase(instrument) eq 'AIA' and strupcase(modeluse) eq 'DATA' then rfilterval=['no_filter','aia_rfilter','nrgf_filter','quarterpower_filter','gamma_filter','rpower_filter'] $
 else begin
  if exist(xxmin) then begin
   if is_number(xxmin) then begin
    if min(xxmin) lt -1. and max(xxmax) gt 1. and min(yymin) lt -1. and max(yymax) gt 1. then begin
       rfilterval=['no_filter','nrgf_filter','quarterpower_filter','gamma_filter','rpower_filter'] 
    endif else begin
      rfilterval=['no_filter','quarterpower_filter','gamma_filter','rpower_filter']
      if strupcase(rfilter) eq 'NRGF_FILTER' then rfilter='quarterpower_filter'
    endelse
   endif else rfilterval='nodisplay'
  endif else rfilterval=['no_filter','nrgf_filter','quarterpower_filter','gamma_filter','rpower_filter']
endelse

if strupcase(gridtype) eq 'CARRMAP' then begin
 rfilterval='nodisplay'
 rfilter='no_filter'
endif

;print,'rfilter=',rfilter
;print,'rfilterval=',rfilterval
;
; define structure containing "observables" inputs
;  to be used in widget interface
; 


ObsInputs={all_types:all_types,all_typesval:'struct4',all_inst:all_betterinst,all_instval:'struct1',all_names:all_names,all_namesval:'struct2',phys_params:phys_params,phys_paramsval:'struct3',$
Instrument:strupcase(instrument),InstVal:'nodisplay',$
Line:strupcase(line),LineVal:'nodisplay',$
Pos:pos,PosVal:PosVal,RFilter:rfilter,RFilterVal:rfilterval,Pop2On:pop2on,Pop2OnVal:pop2onval,$
Frequency_MHz:frequency_MHz,Frequency_MHzVal:Frequency_MHzVal,$
DoGyro:DoGyro,DoGyroVal:DoGyroVal,$
RotAz:rotaz,RotAzVal:RotAzVal,FCor:fcor,FCorVal:FCorVal,$
ULimb:ulimb,ULimbVal:ULimbVal,$
Wavelength_Ang:wavelength_Ang,Wavelength_AngVal:Wavelength_AngVal,$
Wavelength2_Ang:wavelength2_Ang,Wavelength2_AngVal:Wavelength2_AngVal,$
NumIon:numion,NumIonVal:NumIonVal,$
Pop2TRegime:pop2tregime,Pop2TRegimeVal:pop2tregimeVal,$
SeeSpecInputs:SeeSpecInputs,SeeSpecInputsVal:SeeSpecInputsVal,DoNoiseInputs:DoNoiseInputs,DoNoiseInputsVal:DoNoiseInputsVal,ObsLosLimit:obsloslimit,ObsLosLimitVal:ObsLosLimitVal,SeeCompInputs:SeeCompInputs,SeeCompInputsVal:SeeCompInputsVal}

NoisePrams=DoNoiseInputs
SpecPrams=SeeSpecInputs
FCompPrams=SeeCompInputs
SpecPrams.Cversion=str_replace(SpecPrams.Cversion,'*','.')

;print,'instrument=',instrument
;print,'line=',line
;print,'for_obsdefaults pos=',pos
end
