PRO for_widget

;+
;  Name:  FOR_WIDGET
;
;  This program reads in the parameter Input files,
;  creates the FOR_DRIVE command strings,
;  and generates the top level widgets
;
;  If it is the first time called, it generates widgets and strings
;  based on defaults.  
;
;  If FLAG.RESET = 1, it goes to default for Inputs,
;  	widgets and strings except for the model name which is stored 
;	in MODOPS.MODEL,and magnetism flag FLAG.MAGMOD which is set by
;	call to FOR_MODELDEFAULTS in this program. Also WORKING_DIR if set.
;
;  If FLAG.RESET = 2 it uses a user-inputted READPRAMS to define parameters;
;	but since model has not changed widgets and strings and inputs 
;	(other than VARIABLES and MODELSTRING) will be unchanged
;
;  If FLAG.RESET = 3 it uses a user-inputted READPRAMS to define parameters;
;	and since model has changed widgets and strings and inputs 
;	(other than VARIABLES)  will go to defaults appropriate for the 
;	model being deployed (except SETTINGS.READPRAMS and WORKING_DIR and MOREPLOTS and NOERASE)
;
;  If FLAG.RESET = 4 it uses a user-inputted READMAP to define parameters,
;	widgets and strings; setting and output will be mostly unchanged;
;	plot will be defaults for the READMAP including grid unless NOERASE set
;	in which case the plot window will be the same
;	**set when READMAP is chosen in SETTINGS tab of TOP Widget***
;
;  If FLAG.RESET = 5 it uses a user-inputted USEDFILE to define 
;	inputs, widgets and strings to be consistent with a file's
;	grid size/spacing, point of view, instrument, etc.
;
;  If FLAG.RESET = 6 it is like RESET=1 except also MOREPLOTS and NOERASE
;	and GRIDTYPE and INSTRUMENT and LINE and POS are saved and plotting defaults not reset. 
;	also RFILTER-- but not DATE unless MOREPLOTS or NOERASE are set
;	(unless FLAG.MAGMOD has changed, in which case INSTRUMENT and LINE go to defaults)
;	(note that DATA now has MAGMOD=-1 for magnetically dependent observable, and -2 for 
;	 non-magnetically dependent observable, and this will be explicitly taken into account to 
;	 establish if the change from magnetic to non-magnetic has happened).
;
;  If FLAG.RESET = 7 it is identical to RESET=4 except that PLOT options are preserved
;	from the prior widget setup
;	** set for DATA savemap, but only when calculating it either for the first time
;	** (e.g. if loading byfile fits or bydate) or recalculate/redo **
;
;  Called from IDL command line to initiate widget interface
;  	also by FOR_WIDGET_EVENT when RESET button activated,
;		or when MODEL changes (forces RESET=6)
;  	also by FOR_WIDGET_SETTINGS_EVENT if READPRAMS set (RESET=2 or 3)
;  	also by FOR_WIDGET_SETTINGS_EVENT if READMAP set (RESET=4)
;
;  EXTERNAL CALLS:  
;		in GUI directory and subdirectories
;			FOR_WIDGET_LOADING, FOR_WIDGET_BUILD, 
;			FOR_WIDGET_TOP,	FOR_WIDGET_MODEL, FOR_WIDGET_PLOT
;			FOR_WIDGET_DISPLAY
;
;		in DEFAULTS directory  -- FOR_MODDEFAULTS,FOR_OBSDEFAULTS,
;		 FOR_SETTINGDEFAULTS, FOR_OUTPUTDEFAULTS, FOR_GRIDDEFAULTS
;		 FOR_LOSDEFAULTS, FOR_PLOTDEFAULTS
;
;	History
;		Written by Blake Forland May 2013
;		Revised 2013-2014 SEG
;	Version 2.0 July 2014
;		Feb-2016 Added POP2ON,POP2TREGIME SEG
;		Feb-2017 Turned off run of FOR_MODELDEFAULTS for PFSS 
;		  because this forced unnecessary reloads of cube
;		June-2019 - used file_delete for PC compatibility
;		September-2020 -- added hooks for TOMO, CROISSANT
;			also added mmsave below to check for transition
;			between magnetic and non-magnetic 
;		January 2021 -- added hook for STRIA
;		September 2021 -- made adjustments to account for magmod=-1, -2 for DATA
;                 Also removed STRIA conditional for magmod=0 (where TOMO, CAVMORPH don)
;		  also expanded conditional on STOKESPOI
;		  also testing AZEQUI
;	 	December 2021-- passed through distobs
;			also fixed bug where AzEquiVal wasn't updated on old files
;		January 2022-- 
;			fixed bug where obsopstemp didn't completey remove 
;				all_inst,all_names,phys_params forcing default reset
;			overwrote MinNos with ErrTrunc for old files
;			passed azequi through losdefaults
;		March 2022 -- preserved RFILTER when MOREPLOTS set
;			forgot DATE when changing model if MOREPLOTS, NOERASE set
;			added TLOS option
;			changed xoffset --> losoffset
;			Also added TURBHY magmod --> 2 (density only)
;		April 2022 -- passed azequi and distobs through for_plotdefaults
;			updated backward compatibility checks
;		May 2022 -- added UV SpecPol to CoMP for RotAz setup
;		Nov 2022 -- added limb keyword in some of the obsdefaults calls (the ones with grid info for resolution)
;		Jan 2023 -- added wavelength_ang definition to data obspramsstruct
;		Feb 2023 -- changed instr to instrument in call to for_plotdefaults
;		Jul 2023 -- added working_dir to call for modeldefaults (was missing for one)
;		Sep 2023 -- added hooks for AWSOM
;		Feb 2024 -- if moreplots not set, reading in savemap 
;                          will keep its rfilter
;			and in general savemap will maintain plot settings
;		Jul 2024 -- added hooks for CIRTOY
;		Jul 2024 -- passed through ulimb
;			fixed bug in backward compatibility
;		Aug 2024 -- added WLRAT hooks (WL_ANG)

;-
  
common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops
 
;
;  COMMON BLOCK:
;
;  FLAG tracks aspects of current widget implementation
;
;    MAGMOD tracks if model has magnetic field 
;    TAB tracks whether gather windows has been called, so if first time can
;	give message about how to gather windows -- but at the moment
;	I have this commented out. So tab is not really doing anything.
;    RESET similarly checks to see if you just set it, if so, 
;	calls for_widget from the top and reverts to defaults (except model);
;	if set to 2, 3 or 5, makes use of READPRAMS, 
;	if set to 4 or 7, makes use of READMAP
;    WIDGREGEN keeps track of whether a change has been made that requires 
;	regeneration of the widget
;    NOERASESET keeps track of whether NOERASE has just been changed
;    DATE keeps track of whether date has been changed in calendar (only used in FOR_WIDGET_CALENDAR_EVENT)
;    CHANGETABS used in FOR_WIDGET_PLOT_EVENT to force regeneration of plot widget when tabs changed
;    DTA keeps track of whether data has been selected in top widget -- 2 if MOREPLOTS set, otherwise 1; used in
;	FOR_LOSDEFAULTS (turns off occulter unless CoMP), FOR_OBSDEFAULTS (resets filter), 
;	and FOR_PLOTDEFAULTS (resets plotlog, imin, imax, usecolor and docont, sunedge,whitedisk,charsize,dispexp,dispgamma,rpow,nobangletitle,addturbtitle - via FORCECOLOR)
;    RFIL keeps track of which (if any) radial filter is set.  Set within FOR_OBSDEFAULTS, and can be reset in FOR_WIDGET_PLOT_EVENT 
;	used by FOR_PLOTDEFAULTS (affects choices of plotlog, imin, imax)
;    MDL keeps track of changes to model. Set in FOR_WIDGET_EVENT to 1 or 2 (MOREPLOTS).  Used in FOR_LOSDEFAULTS to force XLOS for PSIMAS or AWSOM.
; 	Used in FOR_OBSDEFAULTS (resets filter unless MOREPLOTS). Used in FOR_PLOTDEFAULTS same as DTA (except for MOREPLOTS)
;    RMP keeps track of whether READMAP is set.  Set in FOR_WIDGET_SETTINGS_EVENT to 1 or 2 (MOREPLOTS).  Used in FOR_PLOTDEFAULTS as in DTA (except
;	(for MOREPLOTS).  Used in FOR_OBSDEFAULTS (resets filter unless MOREPLOTS).
;    OBS keeps track of changes to observable being presented.  
;       Set equal to 3 in FOR_WIDGET_PLOT_EVENT if either units or plotlog or pos change,
;	 and equal to 2 if rfilter changes 
;        but note reset to 0 after running for_plotdefault.  
;       Or can be set in FOR_WIDGET_EVENT if PHYSICAL DIAGNOSTIC or OBSERVABLE is changed:
;	  set to 1 unless CoMP and different line/same instrument, or MOREPLOTS, in which case set to 2; also if
;	  change is to instrument but not line, stays set to zero.
;	Then FOR_PLOTDEFAULT resets variables as in DTA if set to 1 or 2, and just resetting imin,imax to scaled to data if set to 3
;	Also FOR_OBSDEFAULT will reset filter if set to 1 
;	Also FOR_COMPDEFAULT will reset variables if set to 1 
;	Also FOR_LOSDEFAULT will reset radio CMER if set to 1 
;		turned this off because it was annoying
;	 and will reset LOSUSE if set to 1
;    NOISE

;
;
;  VARIABLES is this codes name for ModelInputs
;  SETTINGS is this codes name for SettingsInputs
;  PLOTOPS is this codes name for PlotInputs
;  LOSOPS is this codes name for LosInputs
;  GRIDOPS is this codes name for GridInputs
;  OBSOPS is this codes name for ObsInputs
;  OUTOPS is this codes name for OutputInputs
;
;  MODOPS is specific to GUI programs and tracks just 
;		the model name and model parameter command string
;
;  WIDGETS tracks ID for the widget windows
;
;   TOP is top window: set up in FOR_WIDGET, destroyed in 
;	FOR_WIDGET_EVENT (if "Quit" selected)
;	and here in FOR_WIDGET any time it is run
;   MODB is window showing model parameters: setup in FOR_WIDGET_MODEL
;	Destroyed at beginning of FOR_WIDGET_MODEL if already existing
;	and when TOP is destroyed
;   PLOTB is window showing plotting keywords, created in FOR_WIDGET_PLOT
;	Destroyed at beginning of FOR_WIDGET_PLOT if already existing
;	and when TOP is destroyed
;   SETSB is settings change menu: set up in FOR_WIDGET_SETTINGS, 
; 	Destroyed at beginning of FOR_WIDGET_SETTINGS if already existing
;	destroyed in FOR_WIDGET_SETTINGS_EVENT when "CLOSE" 
;	button selected
;   OUTB is output change menu: set up in FOR_WIDGET_OUTPUT
;	Destroyed at beginning of FOR_WIDGET_OUTPUT if already existing
;	destroyed in FOR_WIDGET_OUTPUT_EVENT when "CLOSE" button selected
;   DATE is date change menu - set up in FOR_WIDGET_CALENDAR;	
;	Destroyed at beginning of FOR_WIDGET_CALENDAR if already existing; 
;	destroyed in FOR_WIDGET_CALENDAR_EVENT if 
;	"CLOSE" or "CHANGEDATE" button selected 
;   DISB is display window for FORWARD call: set up in FOR_WIDGET_DISPLAY
;	Destroyed at beginning of FOR_WIDGET_DISPLAY if already existing
;	and when TOP is destroyed
;   HELP  Set up in FOR_WIDGET_HELP (not used right now)
;	destroyed in FOR_WIDGET_HELP_EVENT, FOR__WIDGET_MODEL_EVENT
;   LOAD is window warning loading is in progress: set up in FOR_WIDGET_LOADING,
;	called and destroyed in FOR_WIDGET,FOR_WIDGET_EVENT
;
;  STRINGS is call string for all *Inputs variables except ModelInputs
;	(contained as MODELSTRING in MODOPS), with explicit definitions of variables, 
;	so, STRINGS+MODOPS is how FOR_DRIVE is called 
;	(and when made explicit is the output of PRINTCOMMAND)
;	STRINGS and MODELSTRING  are generated by FOR_WIDGET_BUILD
;	(also note that now OBSSTRING includes OBSOPS.LINE, 
;		INSTRUMENT, etc - before it just had the 
;		arrays of instruments needed to build the OBSERVABLE
;		widget.  Now this is contained in OBSSTRINGSTOP,
;		used in FOR_WIDGET_TOP,
;		also generated by FOR_WIDGET_BUILD.)
;

mdtor=!dpi/180d0
;
; test for PC
;

;
; first check to see if first time running or reset called
;

resetuse=0
firsttime=0
if n_elements(flag) gt 0 then begin
	if flag.reset eq -1 then firsttime = 1
	if flag.reset eq 1 then resetuse = 1
	if flag.reset eq 2 then resetuse = 2
	if flag.reset eq 3 then resetuse = 3
	if flag.reset eq 4 then resetuse = 4
	if flag.reset eq 5 then resetuse = 5
	if flag.reset eq 6 then resetuse = 6
	if flag.reset eq 7 then resetuse = 7
endif else firsttime = 1

;print,'reset=',resetuse

;
; if first time set up widgets structure
; if reset destroy any open widgets and reset their index value
;

disbsave=''
loadsave=''

if firsttime ne 1 then begin
  disbsave=widgets.disb
  widget_control,widgets.top,/DESTROY
  loadsave=widgets.load
endif else begin
 file_delete, 'widget_temp.sav', /quiet
endelse

widgets={top:'',modb:'',plotb:'',setsb:'',disb:'',outb:'',date:'',help:'',load:loadsave}

if exist(outops) then begin
 outops.mapname='defaults'
 if resetuse eq 1 then begin
   outops.moreplots=0
   outops.noerase=0
 endif
 if resetuse eq 6 then begin
   if outops.moreplots eq 0 and outops.noerase eq 0 then settings.date=''
 endif

 if outops.moreplots eq 1 or outops.noerase eq 1 then widgets.disb=disbsave else if disbsave ne '' then widget_control,disbsave,/DESTROY
endif

;
; delete any open windows for resetuse = 1
;

if resetuse eq 1 then begin
 while !d.window ne -1 do wdelete, !d.window
 widget_control,/reset
endif

;
; empty the strings
; (they will be rebuilt for all cases below)
; except resetuse = 6 where we need to save plotstring
;

if exist(strings) then plotstringsave=strings.plotstring
strings={plotstring:',',losstring:',',gridstring:',',outstring:',',setstring:'',obsstring:'',command:''}
     
; 
; load widget to warn people that widget reboot is in process
;
     
; for_widget_loading,0

;
;   assigns MODEL default if first time,
;	empties MODOPS.MODELSTRING
;

testredata=0
if exist(modops) then if strupcase(modops.model) eq 'DATA' and resetuse ne 4 and resetuse ne 7 then testredata=1
if firsttime or testredata then modops={model:'',modelstring:''} else $
	modops.modelstring = ''

;
; set up the default flags 
;

mmsave=''
rmpsave=0
dtasave=0
rfilsave=0
mdlsave=0
obssave=0
noisesave=0
noerasesave=0
if resetuse gt 1 then begin
 if keyword_set(flag.magmod) then mmsave=flag.magmod
 if keyword_set(flag.rmp) then rmpsave=flag.rmp
 if keyword_set(flag.dta) then dtasave=flag.dta
 if keyword_set(flag.rfil) then rfilsave=flag.rfil
 if keyword_set(flag.mdl) then mdlsave=flag.mdl
 if keyword_set(flag.obs) then obssave=flag.obs
 if keyword_set(flag.noise) then noisesave=flag.noise
 if keyword_set(flag.noerase) then noerasesave=flag.noerase
endif
flag={magmod:'',tab:0,reset:0,widgregen:0,noerase:noerasesave,date:'',changetabs:0,dta:dtasave,rfil:rfilsave,mdl:mdlsave,rmp:rmpsave,obs:obssave,noise:noisesave}

;
; establish Input files for common blocks 
;

;
;   read in READMAP file if RESET = 4 or 7
;

if resetuse eq 4 or resetuse eq 7 then begin

	restore,settings.readmap

     	modeluse=strupcase(ModPramsStruct.name)
   	if not tag_exist(ModPramsStruct,'MagMod')  then begin
        	if modeluse ne 'CAVMORPH' and modeluse ne 'TOMO' $
                        and modeluse ne 'CROISSANT' and modeluse ne 'CIRTOY' and modeluse ne 'TURBHY' $
                        then ModPramsStruct=add_tag(ModPramsStruct,1,'MagMod')
        	if modeluse eq 'CAVMORPH' or modeluse eq 'TOMO' $
                        or modeluse eq 'CROISSANT' or modeluse eq 'CIRTOY' $
                        then ModPramsStruct=add_tag(ModPramsStruct,0,'MagMod')
		if modeluse eq 'TURBHY' then $
			ModPramsStruct=add_tag(ModPramsStruct,2,'MagMod')
		if modeluse eq 'DATA' and (strupcase(ObsPramsStruct.Instrument) eq 'COMP' $
		        or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG') $
                        then ModPramsStruct=add_tag(ModPramsStruct,-1,'MagMod')
		if modeluse eq 'DATA' and (strupcase(ObsPramsStruct.Instrument) ne 'COMP' $
		        and strupcase(ObsPramsStruct.Instrument) ne 'CORMAG') $
                        then ModPramsStruct=add_tag(ModPramsStruct,-2,'MagMod')
   	endif
	magmoduse=ModPramsStruct.magmod

	gridopssave = gridops
	obsopssave = obsops

;
; **if moreplots set**
; special case AIA DATA, check to see if radial gradient filter
; has been applied already, use information overwrite any inputted rfilter keyword
;

        if outops.moreplots eq 1 then begin
         rfilter=obsopssave.rfilter
         if strupcase(rfilter) eq 'AIA_RFILTER' then rfilter='NULL'
         if strupcase(ObsPramsStruct.Instrument) eq 'AIA' and strupcase(modeluse) eq 'DATA' then begin
             if tag_exist(QuantMap,'BUnit') then if strpos(strupcase(QuantMap.Bunit),'FILTER') ge 0 then rfilter='aia_rfilter'
             if strpos(strupcase(settings.readmap),'RFILTER') ge 0 then rfilter='aia_rfilter'
         endif 
         obsopssave.rfilter=rfilter
        endif

        oldgrid=0
	if exist(gridinputs) then begin
         if datatype(gridinputs) eq 'STC' then begin
                oldgrid=1
;
; this will be true for any recent save file
;  gridinputs is save version, gridopssave is most recent setup
;  gridops will be appropriate mixing of the two
; (same for obsops,obsinputs, etc)
;
; WARNING -- DATA wont have modelinputs
	 	if datatype(modelinputs) eq 'STC' then variables=modelinputs
		obsops=obsinputs
		gridops=gridinputs
		losops=losinputs
;
; change grid to last used if noerase or moreplots set and not null
; (which will happen if coming from carrington map)
; otherwise use what is in GridPramsStruct
;
	        if (outops.noerase eq 1 or outops.moreplots eq 1) and exist(gridopssave) and strupcase(gridopssave.xxmin) ne 'NULL' then begin
	              gridops.xxmin=double(gridopssave.xxmin)
	              gridops.xxmax=double(gridopssave.xxmax)
	              gridops.yymin=double(gridopssave.yymin)
	              gridops.yymax=double(gridopssave.yymax)
	        endif
; 
; do some backward compatibility
;
		if tag_exist(gridops,'XOffset') then begin
	 	    losoffsetuse=gridops.XOffset
                    gridops=rem_tag(gridops,'XOffSet')
                    gridops=rem_tag(gridops,'XOffSetVal')
		endif else losoffsetuse=0.
		if not tag_exist(gridops,'AzEqui') then gridops=add_tag(gridops,0.,'AzEqui')
                if strupcase(gridops.gridtype) eq 'PLANEOFSKY' then AzEquiValUse='tog' else AzEquiValUse='nodisplay'
		if not tag_exist(gridops,'AzEquiVal') then gridops=add_tag(gridops,AzEquiValUse,'AzEquiVal')
		if not tag_exist(gridops,'DistObs') then gridops=add_tag(gridops,215.,'DistObs')
		if not tag_exist(gridops,'DistObsVal') then gridops=add_tag(gridops,'double-RSUN','DistObsVal')
		if not tag_exist(gridops,'LosOffset') then gridops=add_tag(gridops,losoffsetuse,'LosOffSet')
		if not tag_exist(gridops,'LosOffsetVal') then gridops=add_tag(gridops,'double-RSUN','LosOffsetVal')

 		if tag_exist(gridops,'Phi0') then losops=add_tag(losops,gridops.Phi0,'phio')
 		if tag_exist(gridops,'Phi0Val') then losops=add_tag(losops,'double-DEGREES','phioVal')

		if not tag_exist(losops,'NoStretch') then losops=add_tag(losops,0.,'NoStretch')
		if not tag_exist(losops,'NoStretchVal') then losops=add_tag(losops,'tog','NoStretchVal')
;
; adjust for TLOS
;
   	        if not tag_exist(losops,'NLos')  then begin
 	             if strupcase(losops.LosUse) eq 'TAU' then losops=add_tag(losops,losops.NTau,'NLos')
 	             if strupcase(losops.LosUse) eq 'XLOS' then losops=add_tag(losops,losops.NXLos,'NLos')
 	             if strupcase(losops.LosUse) eq 'TLOS' then losops=add_tag(losops,losops.NXLos,'NLos')
 	        endif
  	        if not tag_exist(losops,'LosInt')  then begin
	              if strupcase(losops.LosUse) eq 'TAU' then losops=add_tag(losops,losops.TauInt,'LosInt')
	              if strupcase(losops.LosUse) eq 'XLOS' then losops=add_tag(losops,losops.XLosInt,'LosInt')
	              if strupcase(losops.LosUse) eq 'TLOS' then losops=add_tag(losops,losops.XLosInt,'LosInt')
	        endif
	        if not tag_exist(losops,'LosMin')  then begin
	              if strupcase(losops.LosUse) eq 'TAU' then losops=add_tag(losops,losops.TauMin,'LosMin')
	              if strupcase(losops.LosUse) eq 'XLOS' then losops=add_tag(losops,losops.XLosMin,'LosMin')
	              if strupcase(losops.LosUse) eq 'TLOS' then losops=add_tag(losops,losops.XLosMin,'LosMin')
	        endif
 
;
; update instruments and other observation flags if necessaray
;
  		obsstring=1
;
; resolution should be forced to be same as save set 
; so zooming in/out wont affect it without recalculate being set
; 
   	        if not tag_exist(obsops,'DoNoiseInputs')  then begin
  	         obsops=add_tag(obsops,obsopssave.DoNoiseInputs,'DoNoiseInputs') 
		 resolutionx = 959.63*abs(gridops.xxmax-gridops.xxmin)/gridops.ngrid
		 resolutiony = 959.63*abs(gridops.yymax-gridops.yymin)/gridops.ngy
	         obsops.DoNoiseInputs.resolution=resolutionx>resolutiony
	        endif else begin
 		 if tag_exist(obsops.donoiseinputs,'MinNos') then begin
		  errtrunc=obsops.donoiseinputs.minnos
		  noisestruc=obsops.donoiseinputs
		  noisestruc=add_tag(noisestruc,errtrunc,'errtrunc')
		  noisestruc=rem_tag(noisestruc,'minnos')
	          obsops=rem_tag(obsops,'donoiseinputs')
		  obsops=add_tag(obsops,noisestruc,'donoiseinputs')
	         endif
	 	endelse

		if not tag_exist(obsops,'Pop2On') then obsops=add_tag(obsops,0,'Pop2On')
		if not tag_exist(obsops,'Pop2TRegime') then obsops=add_tag(obsops,0,'Pop2TRegime')

;		if (strpos(strupcase(obsops.instrument),'OMP') gt 0 or strupcase(obsops.instrument) eq 'CORMAG') and (strpos(strupcase(obsops.line),'Q') gt 0 or strpos(strupcase(obsops.line),'U') gt 0 or strpos(strupcase(obsops.line),'AZ') gt 0) then begin
		if (strpos(strupcase(obsops.instrument),'OMP') gt 0 or strupcase(obsops.instrument) eq 'CORMAG') $
                      or strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' then begin
		    if not tag_exist(obsops,'RotAz')  then obsops=add_tag(obsops,'','RotAz') 
		endif else begin
		    if not tag_exist(obsops,'RotAz')  then obsops=add_tag(obsops,'NULL','RotAz') else obsops.RotAz='NULL'
   		endelse

		if not tag_exist(obsops,'DoGyro') then obsops=add_tag(obsops,obsopssave.DoGyro,'DoGyro')
		if not tag_exist(obsops,'FCor') then obsops=add_tag(obsops,obsopssave.FCor,'FCor')
		if not tag_exist(obsops,'ULimb') then obsops=add_tag(obsops,obsopssave.ULimb,'ULimb')
		if not tag_exist(obsops,'Wavelength_Ang') then obsops=add_tag(obsops,obsopssave.Wavelength_Ang,'Wavelength_Ang')
		if not tag_exist(obsops,'Wavelength2_Ang') then obsops=add_tag(obsops,obsopssave.Wavelength2_Ang,'Wavelength2_Ang')
;
; check this
;
   	        if not tag_exist(obsops,'SeeSpecInputs')  then obsops=add_tag(obsops,obsopssave.SeeSpecInputs,'SeeSpecInputs')
   	        if not tag_exist(obsops,'SeeCompInputs')  then obsops=add_tag(obsops,obsopssave.SeeCompInputs,'SeeCompInputs')

                if not tag_exist(obsops,'rfilter') then obsops=add_tag(obsops,obsopssave.rfilter,'RFilter')
;                if exist(rfilter) then obsops.rfilter=rfilter else obsops=add_tag(obsops,rfilter,'RFilter')

;
; should be ok because for_obsdefaults will rebuild the structures (Noise, Spec, Comp) and
; so if there are new tags they will just go to default
;
		obsopstemp=rem_tag(obsops,'all_inst')
		obsopstemp=rem_tag(obsopstemp,'all_names')
		obsopstemp=rem_tag(obsopstemp,'phys_params')
  		for_widget_build,obsopstemp,inputsname='obsops',stringout=obsstring
		x=execute("for_obsdefaults,magmoduse,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,modeluse"+obsstring+",obsinputs=obsops,working_dir=settings.working_dir")

;
; reset common block datacube if necessary
;
		if strupcase(modeluse) eq 'PSIMAS' or strupcase(modeluse) eq 'AWSOM' or strupcase(modeluse) eq 'NUMCUBE' or strupcase(modeluse) eq 'ADAPTCUBE' then settings.nreinit=1
	 endif
	endif

;
; more structure updates to check for all files
;
; LOS
   	if not tag_exist(LosPramsStruct,'DoDisk')  then LosPramsStruct=add_tag(LosPramsStruct,0,'DoDisk')
   	if not tag_exist(LosPramsStruct,'UpOccult')  then LosPramsStruct=add_tag(LosPramsStruct,1000.,'UpOccult')
   	if not tag_exist(LosPramsStruct,'Occult')  then LosPramsStruct=add_tag(LosPramsStruct,1.,'Occult')
 	if tag_exist(GridPramsStruct,'Phi0') then LosPramsStruct=add_tag(LosPramsStruct,GridPramsStruct.Phi0,'phio')
   	if not tag_exist(LosPramsStruct,'NoStretch')  then LosPramsStruct=add_tag(LosPramsStruct,0,'NoStretch')
   	if not tag_exist(LosPramsStruct,'NLos')  then begin
 	       if strupcase(LosPramsStruct.LosUse) eq 'TAU' then LosPramsStruct=add_tag(LosPramsStruct,LosPramsStruct.NTau,'NLos')
 	       if strupcase(LosPramsStruct.LosUse) eq 'XLOS' then LosPramsStruct=add_tag(LosPramsStruct,LosPramsStruct.NXLos,'NLos')
 	       if strupcase(LosPramsStruct.LosUse) eq 'TLOS' then LosPramsStruct=add_tag(LosPramsStruct,LosPramsStruct.NXLos,'NLos')
 	endif
  	if not tag_exist(LosPramsStruct,'LosInt')  then begin
	        if strupcase(LosPramsStruct.LosUse) eq 'TAU' then LosPramsStruct=add_tag(LosPramsStruct,LosPramsStruct.TauInt,'LosInt')
	        if strupcase(LosPramsStruct.LosUse) eq 'XLOS' then LosPramsStruct=add_tag(LosPramsStruct,LosPramsStruct.XLosInt,'LosInt')
	        if strupcase(LosPramsStruct.LosUse) eq 'TLOS' then LosPramsStruct=add_tag(LosPramsStruct,LosPramsStruct.XLosInt,'LosInt')
	endif
	if not tag_exist(LosPramsStruct,'LosMin')  then begin
	        if strupcase(LosPramsStruct.LosUse) eq 'TAU' then LosPramsStruct=add_tag(LosPramsStruct,LosPramsStruct.TauMin,'LosMin')
	        if strupcase(LosPramsStruct.LosUse) eq 'XLOS' then LosPramsStruct=add_tag(LosPramsStruct,LosPramsStruct.XLosMin,'LosMin')
	        if strupcase(LosPramsStruct.LosUse) eq 'TLOS' then LosPramsStruct=add_tag(LosPramsStruct,LosPramsStruct.XLosMin,'LosMin')
	endif
; GRID
	if tag_exist(GridPramsStruct,'XOffset') then begin
	 	    losoffsetuse=GridPramsStruct.XOffset
                    GridPramsStruct=rem_tag(GridPramsStruct,'XOffSet')
	endif else losoffsetuse=0.
 	if not tag_exist(GridPramsStruct,'AzEqui') then GridPramsStruct=add_tag(GridPramsStruct,0.,'AzEqui')
	if not tag_exist(GridPramsStruct,'DistObs') then GridPramsStruct=add_tag(GridPramsStruct,215.,'DistObs')
	if not tag_exist(GridPramsStruct,'LosOffset') then GridPramsStruct=add_tag(GridPramsStruct,losoffsetuse,'LosOffSet')

;
; these next are for older files
;
   	if tag_exist(GridPramsStruct,'Coord')  then begin
        	if strupcase(GridPramsStruct.Coord) eq 'CART' then begin
                  if is_number(LosPramsStruct.Occult) then begin
                    LosPramsStruct.Occult=-1.*LosPramsStruct.Occult
                  endif
        	endif
   	endif

   	dims=size(GridPramsStruct.RPos)
   	dimx=dims[1]
   	dimy=dims[2]
   	if not tag_exist(GridPramsStruct,'NGrid')  then GridPramsStruct=add_tag(GridPramsStruct,dimx,'NGrid')
   	if not tag_exist(GridPramsStruct,'NGy')  then GridPramsStruct=add_tag(GridPramsStruct,dimy,'NGy')
; OBS
   	if not tag_exist(ObsPramsStruct,'ObsLosLimit')  then ObsPramsStruct=add_tag(ObsPramsStruct,0.d0,'ObsLosLimit')
   	if not tag_exist(ObsPramsStruct,'Date')  then ObsPramsStruct=add_tag(ObsPramsStruct,'','Date')
  	if strpos(strupcase(settings.readmap),'POS') ge 0 then Pos=1.0 else Pos=0.0
   	if not tag_exist(ObsPramsStruct,'Pos')  then ObsPramsStruct=add_tag(ObsPramsStruct,Pos,'Pos')
   	if not tag_exist(ObsPramsStruct,'Frequency_MHz')  then ObsPramsStruct=add_tag(ObsPramsStruct,0,'Frequency_MHz')
   	if not tag_exist(ObsPramsStruct,'DoGyro')  then ObsPramsStruct=add_tag(ObsPramsStruct,0,'DoGyro')
; these may change without forcing recalculation
;	if (strpos(strupcase(ObsPramsStruct.instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.instrument) eq 'CORMAG') and (strpos(strupcase(ObsPramsStruct.linename),'Q') gt 0 or strpos(strupcase(ObsPramsStruct.linename),'U') gt 0 or strpos(strupcase(ObsPramsStruct.linename),'AZ') gt 0) then begin
	if (strpos(strupcase(ObsPramsStruct.instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.instrument) eq 'CORMAG') $
                      or strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' then begin
		    if not tag_exist(ObsPramsStruct,'RotAz')  then ObsPramsStruct=add_tag(ObsPramsStruct,'','RotAz') 
	endif else begin
		    if not tag_exist(ObsPramsStruct,'RotAz')  then ObsPramsStruct=add_tag(ObsPramsStruct,'NULL','RotAz') else ObsPramsStruct.RotAz='NULL'
   	endelse
   	if not tag_exist(ObsPramsStruct,'FCor')  then ObsPramsStruct=add_tag(ObsPramsStruct,0,'FCor')
	if not tag_exist(ObsPramsStruct,'ULimb') then ObsPramsStruct=add_tag(ObsPramsStruct,0.63,'ULimb')
   	if not tag_exist(ObsPramsStruct,'Wavelength_Ang')  then ObsPramsStruct=add_tag(ObsPramsStruct,0,'Wavelength_Ang')
   	if not tag_exist(ObsPramsStruct,'Wavelength2_Ang')  then ObsPramsStruct=add_tag(ObsPramsStruct,0,'Wavelength2_Ang')
   	if not tag_exist(ObsPramsStruct,'NumIon')  then ObsPramsStruct=add_tag(ObsPramsStruct,0,'NumIon')
        if not tag_exist(ObsPramsStruct,'Pop2TRegime') then ObsPramsStruct=add_tag(ObsPramsStruct,0,'Pop2TRegime')
        if not tag_exist(ObsPramsStruct,'IClass') then begin
                 if ObsPramsStruct.Pop2TRegime ne 0 then pop2on=1 else pop2on=0
                 for_obs_choose,magmoduse,modeluse,ObsPramsStruct.LineName,ObsPramsStruct.Instrument,ObsPramsStruct.Label,ObsPramsStruct.LineNum,type,pop2on=pop2on,working_dir=settings.working_dir
                 ObsPramsStruct=add_tag(ObsPramsStruct,type,'IClass')
        endif
   	if not tag_exist(ObsPramsStruct,'SpecPrams')  then ObsPramsStruct=add_tag(ObsPramsStruct,obsopssave.SeeSpecInputs,'SpecPrams')
   	if not tag_exist(ObsPramsStruct,'FCompPrams')  then ObsPramsStruct=add_tag(ObsPramsStruct,obsopssave.SeeCompInputs,'FCompPrams')

;
; older files
;
	if oldgrid eq 0 then begin
	  if strupcase(modeluse) ne 'DATA' then begin
	   d=DIALOG(/WARNING,'OLD SAVEMAP FILE; Parameter values shown in model widget wont necessarily reflect those used to make SAVEMAP') 
;
; set some new parameters if old file
;
   	   if strupcase(ObsPramsStruct.LineName) eq 'STOKESP' then ObsPramsStruct.LineName='StokesL'
   	   if strupcase(ObsPramsStruct.Label) eq 'STOKESP' then ObsPramsStruct.Label='StokesL'
   	   if strupcase(ObsPramsStruct.Label) eq 'STOKESP_POS' then ObsPramsStruct.Label='StokesL_POS'
   	   if strpos(strupcase(ObsPramsStruct.LineName), 'POI') ge 0 then ObsPramsStruct.LineName='StokesLoI'
   	   if strpos(strupcase(ObsPramsStruct.Label), 'POI') ge 0 then ObsPramsStruct.Label='StokesLoI'
   	   if strpos(strupcase(ObsPramsStruct.Label), 'POI_POS') ge 0 then ObsPramsStruct.Label='StokesLoI_POS'
   	   if strupcase(ModPramsStruct.Name) eq 'DATACUBE' then begin
		ModPramsStruct.Name='numcube'
		modeluse='numcube'
	   endif
     	   variables=1
     	   usedate=ObsPramsStruct.date
 	   nreinit=1
     	   for_modeldefaults,modeluse,date=usedate,nreinit=nreinit,modelinputs=variables,working_dir=settings.working_dir
	   settings.nreinit=nreinit
   	   if not tag_exist(ObsPramsStruct,'NoisePrams')  then ObsPramsStruct=add_tag(ObsPramsStruct,obsopssave.DoNoiseInputs,'NoisePrams')
	  endif else begin
;
;  DATA files need the following
;
   	    if not tag_exist(ObsPramsStruct.NoisePrams,'ModEffInt')  then begin
              ObsPramsStruct={date:ObsPramsStruct.date,instrument:ObsPramsStruct.instrument,linename:ObsPramsStruct.linename,pos:'NULL',label:ObsPramsStruct.linename,linenum:ObsPramsStruct.linenum,frequency_MHz:0,Wavelength_Ang:0,Wavelength2_Ang:0,dogyro:0,fcor:0,ulimb:0.63,rotaz:ObsPramsStruct.rotaz,numion:0,NoisePrams:obsopssave.DoNoiseInputs,IClass:ObsPramsStruct.IClass}
	    endif
	  endelse

          if strupcase(GridPramsStruct.GridType) eq 'PLANEOFSKY' then begin
               resolution=959.63d0*(GridPramsStruct.Dx>GridPramsStruct.Dy) 
          endif else begin
            dlat=GridPramsStruct.Dy ; pixel size in degrees, latitude
            C=959.63d0*2.d0*!dpi
            resolution=dlat*C/360.d0
          endelse

          for_obsdefaults,magmoduse,modeluse,azequi=GridPramsStruct.AzEqui,distobs=GridPramsStruct.DistObs,gridtype=GridPramsStruct.gridtype,rfilter=rfilter,obsinputs=obsops,line=ObsPramsStruct.linename,instrument=ObsPramsStruct.instrument,Pos=ObsPramsStruct.Pos,label=ObsPramsStruct.label,numion=ObsPramsStruct.numion,frequency_MHz=ObsPramsStruct.frequency_MHz,dogyro=ObsPramsStruct.dogyro,fcor=ObsPramsStruct.fcor,ulimb=ObsPramsStruct.ulimb,rotaz=ObsPramsStruct.rotaz,wavelength_Ang=ObsPramsStruct.wavelength_Ang,wavelength2_Ang=ObsPramsStruct.wavelength2_Ang,resolution=resolution,NoisePrams=ObsPramsStruct.NoisePrams,SpecPrams=ObsPramsStruct.SpecPrams,FCompPrams=ObsPramsStruct.FCompPrams,working_dir=settings.working_dir 
;
; change grid to last used if noerase or moreplots set and not null
; (which will happen if coming from carrington map)
; otherwise use what is in GridPramsStruct
;
	  if (outops.noerase eq 1 or outops.moreplots eq 1) and exist(gridopssave) and strupcase(gridopssave.xxmin) ne 'NULL' then begin
	        gridops.xxmin=double(gridopssave.xxmin)
	        gridops.xxmax=double(gridopssave.xxmax)
	        gridops.yymin=double(gridopssave.yymin)
	        gridops.yymax=double(gridopssave.yymax)
	  endif else begin
	        gridops.xxmin=double(GridPramsStruct.Xrange[0])
	        gridops.xxmax=double(GridPramsStruct.Xrange[1])
	        gridops.yymin=double(GridPramsStruct.Yrange[0])
	        gridops.yymax=double(GridPramsStruct.Yrange[1])
	  endelse
	  for_griddefaults,modeluse,ObsPramsStruct.Pos,ObsPramsStruct.Instrument,ObsPramsStruct.LineName,gridinputs=gridops,gridtype=GridPramsStruct.gridtype,phio=GridPramsStruct.Phio,$
	        azequi=gridops.azequi,$
	        distobs=gridops.distobs,$
		xxmin=gridops.xxmin,xxmax=gridops.xxmax,yymin=gridops.yymin,yymax=gridops.yymax,$
		ngrid=GridPramsStruct.NGrid,ngy=GridPramsStruct.NGy,limb=GridPramsStruct.limb,$
		losoffset=GridPramsStruct.LosOffset,rheight=GridPramsStruct.rheight
	  if is_number(LosPramsStruct.axisymmult) then begin
		  if LosPramsStruct.axisymmult eq 1. then begin
			axisym=0
			nlosuse=LosPramsStruct.NLos
		  endif else begin
			asixym=1
			nlosuse=fix(LosPramsStruct.nlos/2.d0)
	 	  endelse
	  endif else begin
			axisym='NULL'
			nlosuse='NULL'
	  endelse
	  if is_number(LosPramsStruct.thetao) then thetaouse=LosPramsStruct.thetao/mdtor else thetaouse=LosPramsStruct.thetao
          if is_number(GridPramsStruct.phio) then phiouse=GridPramsstruct.phio/mdtor else phiouse=0.d0
          for_losdefaults,modeluse,gridops.gridtype,gridops.rheight,gridops.limb,obsops.instrument,obsops.line,obsops.Pos,gridops.azequi,losinputs=losops,losuse=LosPramsStruct.losuse,dodisk=LosPramsStruct.dodisk,$
			axisym=axisym,$
			cmer=GridPramsStruct.cmer/mdtor,bang=LosPramsStruct.bang/mdtor,thetao=thetaouse,phio=phiouse,$
			losmin=LosPramsStruct.losmin,losint=LosPramsStruct.losint,nlos=nlosuse,$
			occult=LosPramsStruct.occult,upoccult=LosPramsStruct.upoccult

	endif

	settings.date=ObsPramsStruct.date

        modops.model=modeluse

; first time loading a save file (resetuse =4) want to use its plot settings 
;  so assign them to plotops (and then execute via plotstringsave which mirrors plotops)
;  on the other hand, if resetuse eq 7, so a DATA file which has been regenerated,
;   want to keep whatever the recent plot settings are - so what is in common block plotops
; some files may have floats instead of doubles which will mess this up, so putting in a fix

        if resetuse eq 4 then begin
	 if datatype(PlotSave) eq 'STC' then begin
          t=tag_names(PlotSave)
          for i=0,n_elements(t)-1 do begin
           if strpos(t[i],'VAL') lt 0 and typename(PlotSave.(i)) ne 'ANONYMOUS' then begin
            if strupcase(string(PlotOps.(i)[0])) ne 'NULL'  $
             and strupcase(string(PlotSave.(i)[0])) ne 'NULL' then begin
             if typename(PlotOps.(i)) ne typename(PlotSave.(i)) then begin
              case 1 of 
                typename(PlotOps.(i)) eq 'FLOAT': $
                 void=execute('PlotOps.(i)=float(PlotSave.(i))')
                typename(PlotOps.(i)) eq 'DOUBLE': $
                 void=execute('PlotOps.(i)=double(PlotSave.(i))')
                typename(PlotOps.(i)) eq 'INT': $
                 void=execute('PlotOps.(i)=fix(PlotSave.(i))')
                typename(PlotOps.(i)) eq 'LONG': $
                 void=execute('PlotOps.(i)=long(PlotSave.(i))')
                typename(PlotOps.(i)) eq 'STRING': $
                 void=execute('PlotOps.(i)=string(PlotSave.(i))')
	        else: stop
              endcase
             endif else $
               void=execute('PlotOps.(i)=PlotSave.(i)')
           endif
          endif
          endfor
          t=tag_names(PlotSave.DoContInputs)
          for i=0,n_elements(t)-1 do begin
           if strpos(t[i],'VAL') lt 0 and strupcase(string(PlotSave.DoContInputs.(i)[0])) ne 'NULL' $
              and strupcase(string(PlotOps.DoContInputs.(i)[0])) ne 'NULL' then begin
             if typename(PlotOps.DoContInputs.(i)) ne typename(PlotSave.DoContInputs.(i)) then begin
              case 1 of 
                typename(PlotOps.DoContInputs.(i)) eq 'FLOAT': $
                 void=execute('PlotOps.DoContInputs.(i)=float(PlotSave.DoContInputs.(i))')
                typename(PlotOps.DoContInputs.(i)) eq 'DOUBLE': $
                 void=execute('PlotOps.DoContInputs.(i)=double(PlotSave.DoContInputs.(i))')
                typename(PlotOps.DoContInputs.(i)) eq 'INT': $
                 void=execute('PlotOps.DoContInputs.(i)=fix(PlotSave.DoContInputs.(i))')
                typename(PlotOps.DoContInputs.(i)) eq 'LONG': $
                 void=execute('PlotOps.DoContInputs.(i)=long(PlotSave.DoContInputs.(i))')
                typename(PlotOps.DoContInputs.(i)) eq 'STRING': $
                 void=execute('PlotOps.DoContInputs.(i)=string(PlotSave.DoContInputs.(i))')
	        else: stop
              endcase
             endif else $
               void=execute('PlotOps.DoContInputs.(i)=PlotSave.DoContInputs.(i)')
           endif
          endfor
          t=tag_names(PlotSave.FieldLinesInputs)
          for i=0,n_elements(t)-1 do begin
           if strpos(t[i],'VAL') lt 0 and strupcase(string(PlotSave.FieldLinesInputs.(i)[0])) ne 'NULL' $
              and strupcase(string(PlotOps.FieldLinesInputs.(i)[0])) ne 'NULL' then begin
             if typename(PlotOps.FieldLinesInputs.(i)) ne typename(PlotSave.FieldLinesInputs.(i)) then begin
              case 1 of 
                typename(PlotOps.FieldLinesInputs.(i)) eq 'FLOAT': $
                 void=execute('PlotOps.FieldLinesInputs.(i)=float(PlotSave.FieldLinesInputs.(i))')
                typename(PlotOps.FieldLinesInputs.(i)) eq 'DOUBLE': $
                 void=execute('PlotOps.FieldLinesInputs.(i)=double(PlotSave.FieldLinesInputs.(i))')
                typename(PlotOps.FieldLinesInputs.(i)) eq 'INT': $
                 void=execute('PlotOps.FieldLinesInputs.(i)=fix(PlotSave.FieldLinesInputs.(i))')
                typename(PlotOps.FieldLinesInputs.(i)) eq 'LONG': $
                 void=execute('PlotOps.FieldLinesInputs.(i)=long(PlotSave.FieldLinesInputs.(i))')
                typename(PlotOps.FieldLinesInputs.(i)) eq 'STRING': $
                 void=execute('PlotOps.FieldLinesInputs.(i)=string(PlotSave.FieldLinesInputs.(i))')
	        else: stop
              endcase
             endif else $
               void=execute('PlotOps.FieldLinesInputs.(i)=PlotSave.FieldLinesInputs.(i)')
           endif
	  endfor
          t=tag_names(PlotSave.StkLinesInputs)
          for i=0,n_elements(t)-1 do begin
           if strpos(t[i],'VAL') lt 0 and strupcase(string(PlotSave.StkLinesInputs.(i)[0])) ne 'NULL' then begin
             if typename(PlotOps.StkLinesInputs.(i)) ne typename(PlotSave.StkLinesInputs.(i)) $
              and strupcase(string(PlotOps.StkLinesInputs.(i)[0])) ne 'NULL' then begin
              case 1 of 
                typename(PlotOps.StkLinesInputs.(i)) eq 'FLOAT': $
                 void=execute('PlotOps.StkLinesInputs.(i)=float(PlotSave.StkLinesInputs.(i))')
                typename(PlotOps.StkLinesInputs.(i)) eq 'DOUBLE': $
                 void=execute('PlotOps.StkLinesInputs.(i)=double(PlotSave.StkLinesInputs.(i))')
                typename(PlotOps.StkLinesInputs.(i)) eq 'INT': $
                 void=execute('PlotOps.StkLinesInputs.(i)=fix(PlotSave.StkLinesInputs.(i))')
                typename(PlotOps.StkLinesInputs.(i)) eq 'LONG': $
                 void=execute('PlotOps.StkLinesInputs.(i)=long(PlotSave.StkLinesInputs.(i))')
                typename(PlotOps.StkLinesInputs.(i)) eq 'STRING': $
                 void=execute('PlotOps.StkLinesInputs.(i)=string(PlotSave.StkLinesInputs.(i))')
	        else: stop
              endcase
             endif else $
               void=execute('PlotOps.StkLinesInputs.(i)=PlotSave.StkLinesInputs.(i)')
           endif
	  endfor
         endif
        endif

        if resetuse eq 4 and datatype(PlotSave) ne 'STC' then for_plotdefaults,magmoduse,modops.model,plotinputs=plotops,gridtype=GridPramsStruct.gridtype,azequi=GridPramsStruct.AzEqui,distobs=GridPramsStruct.DistObs,dodisk=LosPramsStruct.dodisk,noerase=outops.noerase,instrument=ObsPramsStruct.instrument,line=ObsPramsStruct.LineName,rotaz=ObsPramsStruct.RotAz,donoise=ObsPramsStruct.NoisePrams.DoNoise,pos=ObsPramsStruct.Pos else $
        c = execute("for_plotdefaults,magmoduse,modops.model,plotinputs=plotops,gridtype=GridPramsStruct.gridtype,azequi=GridPramsStruct.AzEqui,distobs=GridPramsStruct.DistObs,dodisk=LosPramsStruct.dodisk,noerase=outops.noerase,instrument=ObsPramsStruct.instrument,line=ObsPramsStruct.LineName,rotaz=ObsPramsStruct.RotAz,donoise=ObsPramsStruct.NoisePrams.DoNoise,pos=ObsPramsStruct.Pos"+plotstringsave)

;
;  I included this next bit for backward compatibility
; because I used to carry along all the NUMCUBE parameters in PSIMAS although
; many were not needed e.g. those defining background.  Thus, I would load an
; old save file but the widget would not reflect the reduced set of variables.
; So, I would rerun for_modeldefaults to update modelinputs/variables structure.
;
; It quickly caused problems, however,
; for example forcing unnecessary PFSS and DATA reloads, and so I put in the conditional.
; But then I realized it also forces NUMCUBE/PSIMAS reloads and if for example the
; save set is in the WORKING_DIR but the original datacube is not, it causes problems.
;  so original intention of not showing reduced set of variables in PSIMAS is problematic.
;
;  I may still need it if I ever model parameters. For example, NUMCUBE, PFSSMOD now have
;   topology, odensprof, oT0, cdensprof, cT0. But these require linking to cube or file
;   with topology, so, should not cause problems for old save files. But, something to keep 
;   an eye on.
;
;	if strupcase(modeluse) ne 'DATA' and strupcase(modeluse) ne 'PFSSMOD' then begin
; 	  modstring=1
; 	  for_widget_build,variables,inputsname='variables',stringout=modstring
; 	  modeluse=modops.model
;      	  usedate=ObsPramsStruct.date
;  	  nreinit=settings.nreinit
;           c=execute("for_modeldefaults,modeluse,date=usedate,nreinit=nreinit,modelinputs=variables"+modstring)
; 	  settings.nreinit=nreinit
;  	endif


endif else begin

;
; set everything if FIRSTTIME (RESET=0)
; reset everything except WORKING_DIR and MODEL name/type if RESET = 1
; only change model prams and turn off SETTINGS.READMAP if RESET = 2
; make sure settings are ok for model and turn off SETTINGS.READMAP if RESET = 3
; change specific settings, obs, grid and los stuff if RESET = 5 
; reset everything except WORKING_DIR and MODEL name/type, also DATE, MEMORY
;	GRIDTYPE, LINE & INSTRUMENT (unless magmod changed), and PLOT defaults if RESET = 6
;
     if resetuse eq 2 or resetuse eq 3 then settings.readmap=''

     if resetuse ne 2 and resetuse ne 3 and resetuse ne 5 then begin
      if resetuse ne 0 then begin
	readpramsuse=settings.readprams
        workdiruse=settings.working_dir
        dateuse=settings.date
        memoryuse=settings.memory
      endif
      for_settingdefaults,SettingInputs=settings
      if resetuse ne 0 then begin
        settings.working_dir=workdiruse
        if resetuse eq 6 then begin
          settings.date=dateuse
          settings.memory=memoryuse
	endif
      endif
     endif

;
; set/reset outputs if model or observable might have changed (all but 2)
; preserve MOREPLOTS and NOERASE for all except RESET = 0 and 1
;
     if resetuse gt 3 then begin
	noeraseuse=outops.noerase
        moreplotuse=outops.moreplots
     endif

     if resetuse ne 2 and resetuse ne 3 then for_outputdefaults,OutputInputs=outops

     if resetuse gt 3 then begin
        outops.moreplots=moreplotuse
        outops.noerase=noeraseuse
     endif

;
; get most of RESET=5 from USEDFILE data file
;

     if resetuse eq 5 then begin

        for_viewfromdata,usedfile=settings.usedfile,$
           date=date,$
	   gridtype=gridtype,xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,$
           ngrid=ngrid,ngy=ngy,cmer=cmer,bang=bang,$
           line=line,instrument=instrument

	   settings.date=date
	   settings.readmap=''
	   settings.usedfile=''

     endif

;
; READPRAMS for PFSS model defines PFSSFILE/TOMOFILE/STRIAFILE/SYNCOMFILE, 
; and there could
; be an inconsistency with DATE.  
; The one in PFSSFILE/TOMOFILE/STRIAFILE should be used.
; This only applies to RESET=2,3 because they are the only ones
; with READPRAMS set, and will be clear because the date
; will be changed within PFSSMODPRAMS/TOMOPRAMS/STRIAPRAMS
;
; same applies for TOMO
;
     usedate=settings.date

;
; now reset model parameters for all
;

     modeluse=modops.model
     variables=1
     usenreinit=settings.nreinit
     for_modeldefaults,modeluse,readprams=settings.readprams,date=usedate,working_dir=settings.working_dir,nreinit=usenreinit,modelinputs=variables,ModPramsStruct=magtest
     settings.nreinit=usenreinit

     if settings.date ne usedate then settings.date=usedate

     magmoduse=magtest.magmod	
     modops.model=modeluse

;
; for_viewfromdata now needs to be run for cases other than
; RESETUSE = 5, since dates may have changed and with them viewing
; angles
;

;
; if model hasnt changed nor has magmod then keep instrument and line
;	and pos
;
; WARNING-- this did not seem to check for magmod/model change!
;  maybe it was considered implicit in choice of resetuse 2 or 6
;  but it would be good to be able to access overplot capability of resetuse=6
;  going between magnetic and nonmagnetic without running into danger
;  of magnetic observable retained in between.
;  So added mmsave test  below
;   note the only issue is going from magnetic to non-magnetic model
;   and cannot force DATA to be anything other than instrument/line it is
;

     if (resetuse eq 2 or resetuse eq 6) then begin
        if (mmsave eq 0 or mmsave eq 2 or mmsave eq -2) or $
	   ((mmsave eq 1 or mmsave eq -1) and magmoduse eq 1) then begin
	 instrument = obsops.instrument 
	 line=obsops.line
         pos=obsops.pos
	 rfilter=obsops.rfilter
        endif
     endif

     if resetuse ne 5 and resetuse ne 0 then for_viewfromdata,$
           date=settings.date,cmer=cmer,bang=bang,instrument=instrument

;
; now reset OBS for all but RESET=2 
; and reset GRID for all but RESET=2 and RESET=3
;
; RESET=5 will use GRID information from USEDFILE
; RESET=6 will use GRID information from DATA 
; (but not ngrid,ngy, just FOV)
;

     if resetuse ne 2 then begin

	if resetuse ne 6 then begin
          for_obsdefaults,magmoduse,modeluse,obsinputs=obsops,azequi=azequi,distobs=distobs,gridtype=gridtype,line=line,instrument=instrument,xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,limb=limb,ngrid=ngrid,ngy=ngy,working_dir=settings.working_dir
	  if resetuse ne 3 then for_griddefaults,modeluse,obsops.Pos,obsops.instrument,obsops.line,gridinputs=gridops,gridtype=gridtype,azequi=azequi,distobs=distobs,xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,ngrid=ngrid,ngy=ngy 
	endif else begin
          for_obsdefaults,magmoduse,modeluse,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,obsinputs=obsops,pos=pos,line=line,instrument=instrument,rfilter=rfilter,xxmin=gridops.xxmin,xxmax=gridops.xxmax,yymin=gridops.yymin,yymax=gridops.yymax,limb=gridops.limb,ngrid=ngrid,ngy=ngy,working_dir=settings.working_dir
	  for_griddefaults,modeluse,obsops.Pos,obsops.instrument,obsops.line,gridinputs=gridops,gridtype=gridops.gridtype,azequi=gridops.azequi,distobs=gridops.distobs,xxmin=gridops.xxmin,xxmax=gridops.xxmax,yymin=gridops.yymin,yymax=gridops.yymax,ngrid=ngrid,ngy=ngy,limb=gridops.limb,rheight=gridops.rheight
  
	endelse
        if obsops.line eq 'NONE' and obsops.instrument eq 'NONE' then begin
	   obsops.line='PB'
	   obsops.instrument = 'WL'
        endif
     endif

;
; reset LOS for all 
; (RESET=2 needs it because PFSSFILE may have changed and with it viewing angle)
; RESET=6 will use information from DATA 
;

     if resetuse ne 6 then for_losdefaults,modeluse,gridops.gridtype,gridops.rheight,gridops.limb,obsops.instrument,obsops.line,obsops.Pos,gridops.azequi,losinputs=losops,cmer=cmer,bang=bang,phio=0.d0 $
     else for_losdefaults,modeluse,gridops.gridtype,gridops.rheight,gridops.limb,obsops.instrument,obsops.line,obsops.Pos,gridops.azequi,losinputs=losops,cmer=cmer,bang=bang,phio=0.d0,occult=losops.occult,upoccult=losops.upoccult

;
; finally, plot doesnt need to be changed if model and observable havent 
; special case resetuse = 3 NOERASE, only change imin, imax
;

  keepplot=0
  if resetuse eq 3 and outops.noerase eq 1 then keepplot=1
  if resetuse ne 2 and resetuse ne 6 and keepplot ne 1 then for_plotdefaults,magmoduse,modops.model,plotinputs=plotops,gridtype=gridops.gridtype,azequi=gridops.azequi,distobs=gridops.distobs,dodisk=losops.dodisk,noerase=outops.noerase,line=obsops.line,instrument=obsops.instrument,rotaz=obsops.rotaz,donoise=obsops.DoNoiseInputs.DoNoise,pos=obsops.Pos
  if resetuse eq 6 then c = execute("for_plotdefaults,magmoduse,modops.model,plotinputs=plotops,gridtype=gridops.gridtype,azequi=gridops.azequi,distobs=gridops.distobs,dodisk=losops.dodisk,noerase=outops.noerase,instrument=obsops.instrument,line=obsops.line,donoise=obsops.DoNoiseInputs.DoNoise,rotaz=obsops.rotaz,pos=obsops.Pos"+plotstringsave)
  if keepplot eq 1 then begin
         plotstringsave=1
         plotops.imin='scaled to data'
         plotops.imax='scaled to data'
         for_widget_build,plotops,inputsname='plotops',stringout=plotstringsave
         c = execute("for_plotdefaults,magmoduse,modops.model,plotinputs=plotops,gridtype=gridops.gridtype,azequi=gridops.azequi,distobs=gridops.distobs,dodisk=losops.dodisk,noerase=outops.noerase,instrument=obsops.instrument,line=obsops.line,rotaz=obsops.rotaz,donoise=obsops.DoNoiseInputs.DoNoise,pos=obsops.Pos"+plotstringsave)
  endif

endelse

;
;   build strings for all
;

modstring=1
for_widget_build,variables,inputsname='variables',stringout=modstring
modops.modelstring=modstring

setstring=1
for_widget_build,settings,inputsname='settings',stringout=setstring
strings.setstring = setstring

outstring=1
for_widget_build,outops,inputsname='outops',stringout=outstring
strings.outstring = outstring

obsstring=1
for_widget_build,obsops,inputsname='obsops',stringout=obsstring
strings.obsstring = obsstring

gridstring=1
for_widget_build,gridops,inputsname='gridops',stringout=gridstring
strings.gridstring = gridstring

losstring=1
for_widget_build,losops,inputsname='losops',stringout=losstring
strings.losstring = losstring

plotstring=1
for_widget_build,plotops,inputsname='plotops',stringout=plotstring
strings.plotstring=plotstring

flag.magmod=magmoduse
;
; remove any leftover temporary save files
;

if file_exist('widget_temp.sav') then file_delete,"widget_temp.sav", /quiet
  
;
; Now build widgets!!
;
  
for_widget_top

if strupcase(modeluse) ne 'DATA' then for_widget_model

for_widget_plot
     
if outops.moreplots eq 0 then for_widget_display

;
; turns off the "loading" window
;

; widget_control,widgets.load, /DESTROY

; widgets.load=''
     
END
