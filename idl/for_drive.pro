pro for_drive,model,$
	abundance=abundance,aperture=aperture,aia=aia,$
	arrowave=arrowave,axisym=axisym,$
	axiscolor=axiscolor,azequi=azequi,distobs=distobs,bgcolor=bgcolor,background=background,$
	bang=bang,bg_input=bg_input,bg_output=bg_output,bcolor=bcolor,bminuse=bminuse,$
	bscale=bscale,bthick=bthick,c_charsize=c_charsize,cds=cds,cecoeff=cecoeff,$
	charsize=charsize,charthick=charthick,colden=colden,b_int=b_int,b_dens_int=b_dens_int,b_pos_int=b_pos_int,b_pos_dens_int=b_pos_dens_int,ben_dens_int=ben_dens_int,benergy=benergy,colortable=colortable,$
	comp=comp,coorduser=coorduser,cmer=cmer,cormag=cormag,cpos=cpos,crtn=crtn,cversion=cversion,$
        datadump=datadump,date=date,defineprams=defineprams,$
 	dispexp=dispexp,dispgamma=dispgamma,$
	docont=docont,dodisk=dodisk,dogyro=dogyro,donoise=donoise,efficiency=efficiency,eis=eis,eit=eit,$
	eps=eps,euvia=euvia,errtrunc=errtrunc,$
	euviab=euvib,extratitle=extratitle,faraday=faraday,fieldlines=fieldlines,fiwatmo=fiwatmo,$
	FCompPrams=FCompPrams,fcor=fcor,ulimb=ulimb,for_wlmin=for_wlmin,for_wlmax=for_wlmax,$
	frequency_MHz=frequency_MHz,gif=gif,greencomp=greencomp,gridtype=gridtype,$
	icoll=icoll,idebug=idebug,incres=incres,imax=imax,imin=imin,instrument=instrument,integration=integration,$
	InGofNT=InGofNT,ioneq=ioneq,iris=iris,myspect=myspect,iondens=iondens,isum=isum,isplin=isplin,iwatom=iwatom,iweqi=iweqi,iwline=iwline,vintchoice=vintchoice,jpeg=jpeg,kcor=kcor,labelonly=labelonly,limb=limb,$
	line=line,LLim=LLim,losem=losem,losint=losint,losmin=losmin,$
	nangleint=nangleint,collkey=collkey,isotropic=isotropic,wpar_open=wpar_open,wpar_closed=wpar_closed,aniso_open=aniso_open,$
        chromorad=chromorad,blend=blend,einsteina=einsteina,einsteinb=einsteinb,gj=gj,$
	losuse=losuse,LWidth=LWidth,lya=lya,mapname=mapname,$
	memory=memory,moreplots=moreplots,$
	modeffint=modeffint,modeffquant=modeffquant,$
	narrow=narrow,neviii770=neviii770,neviii780=neviii780,ngrid=ngrid,ngy=ngy,nlev=nlev,nlos=nlos,nocontcolor=nocontcolor,nostretch=nostretch,$
	noclabel=noclabel,noerase=noerase,noforward=noforward,noftran=noftran,NoisePrams=NoisePrams,noplots=noplots,$
        nowidgmess=nowidgmess,$
	nreinit=nreinit,nstarrow=nstarrow,nwinx=nwinx,nwiny=nwiny,$
	norandom=norandom,norunmodel=norunmodel,nulldatacolor=nulldatacolor,numion=numion,$
	obsloslimit=obsloslimit,occult=occult,othercomp=othercomp,$
	OutGofNT=OutGofNT,ovi1032=ovi1032,ovi1037=ovi1037,mgix706=mgix706,parallel=parallel,phio=phio,phuser=phuser,$
	PITeamResp=PITeamResp,pop2abundance=pop2abundance,pop2colden=pop2colden,pop2ionfrac=pop2ionfrac,$
	pop2tregime=pop2tregime,pop2losem=pop2losem,pos=pos,plotlog=plotlog,pscale=pscale,$
	psplot=psplot,qnorm=qnorm,radio=radio,readmap=readmap,readprams=readprams,$
	reinit=reinit,resolution=resolution,rfilter=rfilter,rheight=rheight,ruser=ruser,rotaz=rotaz,$
	rpow=rpow,nobangletitle=nobangletitle,addturbtitle=addturbtitle,$
	savemap=savemap,saveprams=saveprams,seecomp=seecomp,seespec=seespec,$
	fe11comp=fe11comp,si9comp=si9comp,si10comp=si10comp,SpecPrams=SpecPrams,$
	smalln=smalln,sminuse=sminuse,$
	stkcolor=stkcolor,stklines=stklines,stkthick=stkthick,sunedge=sunedge,$
	swap=swap,swss=swss,tscope=tscope,tiff=tiff,title=title,$
	thetao=thetao,thuser=thuser,tp2te=tp2te,$
	trace=trace,units=units,upoccult=upoccult,usecolor=usecolor,usedfile=usedfile,$
	UserSpecFiles=UserSpecFiles,UserTResp=UserTResp,verbose=verbose,$
	wavelength_Ang=wavelength_Ang,wavelength2_Ang=wavelength2_Ang,whitedisk=whitedisk,winnumber=winnumber,wl=wl,$
	working_dir=working_dir,losoffset=losoffset,xtitle=xtitle,xrt=xrt,$
	xxmin=xxmin,xxmax=xxmax,ytitle=ytitle,yymin=yymin,yymax=yymax,$
        ModPramsStruct=ModPramsStruct,GridPramsStruct=GridPramsStruct,$
        ObsPramsStruct=ObsPramsStruct,LosPramsStruct=LosPramsStruct,$
	QuantMap=QuantMap,ModSolStruct=ModSolStruct,StokesStruct=StokesStruct,WLStruct=WLStruct,$
	_extra=extra
;+
;  Name:  FOR_DRIVE
;
;  This program does a forward calculation using a specified model
;  to define plasma properties, e.g. N, T, B, V; needed to
;  reproduce a specified observable
;
;
;  MODEL - Must be a string.  Choices now are 'giblow', 'liteslow','lowhund','mydipole',
;	'pfssmod','numcube', 'croissant', 'tomo', 'psimas', and 'cavmorph' and 'cirtoy' and 'awsom'
;       (being tested: 'adaptcube' (which allows adaptive mesh)) and 'stria'  and 'syncom' and 'turbhy'
;	If not specified, default is 'giblow'
;
;  INPUT keywords are defined and defaults are set in DEFAULTS/FOR_*DEFAULTS.PRO 
;		except for model inputs -- these are covered by _EXTRA
;			and described and provided with defaults in the various "MODNAME/modname"prams.pro 
;		also note that data-plotting codes FOR_PLOTFITS and PLOT_COMP_QUICKINV use some 
;			of the codes in the tree but are independent of FOR_DRIVE. Thus they
;			set their own defaults.
;
;  OUTPUTS
;
;		parameters based on inputs are grouped in structure format
;		  (but not identical to inputs necessarily, e.g., changes from degrees to radians for angles)
;		  these are grouped into those pertaining to particular models (MODPRAMSSTRUCT)
;		  those defining the output grid (GRIDPRAMSSTRUCT)
;		  those defining the line of sight integration (LOSPRAMSSTRUCT)
;		  those defining the observable being forward-modeled (OBSPRAMSSTRUCT)
;	    
;		also, the results of the observable calculations (QUANTMAP)
;		and the model physical quantities (e.g., density, temperature, magnetic field)
;			used in the LOS integration, for the plane-of-sky slice (MODSOLSTRUCT)
;

;
;	History
;	Initial version (2010-2014) 
;		Sarah Gibson, Terry Kucera, Jim Dove, Laurel Rachmeler
;	Widget interface (FOR_WIDGET) written by Blake Forland, Sarah Gibson
;	Version 2.0 July 2014
;
; 	9-Jan-2014 added pop2colden, pop2losem, pop2abundance and pop2tregime,pop2ionfrac keywords for  for_obsdefaults TAK
;
; 	Feb-2016 fixes for dodisk, pop2tregime, bang SEG
;	Mar-2016 - made USER gridtype use date-defined CMER, BANG; also added COORDUSER SEG
;	Feb-2017 updated definition of QuantMap.Name check to be consistent with FOR_POS_MAP
;		also forced overwrite of ObsInputs.Line and Instrument with what is in ObsPramsStruct
;		also of obsinputs.donoiseinputs with obspramsstructs.noiseprams
;		and obsinputs.seecompinputs with obspramsstruct.fcompprams
;		because save sets saved from line command, then change line, then resave, caused problems
;		 also forced overwrite of ObsInputs.RotAz because it can be changed for save sets
;		also moved call to for_plotdefaults to after dodisk/occult inputs
;		  dealt with
;	Mar-2018 - fixed bug where it was replacing AIA data line with e.g. 193 A instead of 193 
;		also called for_compdefaults with dummy instrument/line
;			for CoMP DATA
;		also fixed problem where DATA Recalculate save 
;			was crashing because ObsInputs not set
;	June 2018 - added BENERGY,etc (magnetic energy)
;	July 2018 - added BGCOLOR
;	Sept 2018 -- added resolution capability for Carrington map
;	May 2019 -- removed aniso_closed and added collkey
;	June 2019 used slash for PC compatibility
;	August 2020 - fixed typo CARMAP
;	Nov 2020 - Jan 2021 -- added hooks STRIA TOMO
;		also new keywords datadump and rpow 
;       February 2021 -- added ModPramsStruct.Name to call to for_noisedefaults
;	July 2021 -- made sure only call for_noisedefaults for non DATA
;	2020/2021 -- added hooks so that datadump dumped data, not verbose
;		 fixed bug where nulldata checked against 9999 not -9999
;	Sept 2021 -- expanded dummy set of NoisePrams for DATA
;		and put in pass through of ioneq='data' so it didn't make the IONEQ file
;	        and put in hooks for magmod DATA -1 -2
;		Also removed STRIA conditional for magmod=0 (where TOMO, CAVMORPH don)
;		Added file_delete of 'IONEQ'
;		put in conditional not DATA before updating QuantMap.ID for Carrington Map
;               changed inst to instr in for_specdefaults call because of potential for confusion with system var
;		expanded conditional test for STOKESQOI/UOI etc
;		testing azequi 
;		passed nowidgmess through to for_plot and modelprams (so PFSS wouldn't plot them)
;	Nov 2021 --- passed mapname through to for_forwardmap
;			print statement for saved savemap
;	Dec 2021 - passing through distobs
;	Jan 2022 - changed name of MINNOS to ERRTRUNC
;			passed azequi through for_losdefaults
;	Feb 2022 - passed through bg_input,bg_output
;	Mar 2022 - made test for plotting stklines abs(magmod) allowing 
;		CoMP overplot
;			also allowed TLOS
;		changed xoffset-->losoffset
;		added magmod=2 for TURBHY
;	Apr 2022- added nostretch keyword
;		added annotations to title and id for TURBHY
;		passed azequi and distobs through to for_plotdefaults
;		and working_dir through to for_posnoint and for_plotfieldlines
;	Jun 2022 -- added nobangletitle
;	Sep 2022 -- updated noise calculation to allow modeffquant to change
;		with change of line (newline) for specpol
;	Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;		added addturbtitle
;	Nov 2022 
;		added annotations about pos
;			and POS to title for plots
;			and checked for POS instrument/line for_obs_name compatibility
;		added /silent to cleanplot
;		also removed keyword maxheight because not used
;		and added limb=limb in call to for_obsdefaults
;	Jan 2023
;		allowed "units" keyword for gridtype=user
;	Feb 2023
;		overwrote gridinputs xxmin etc with gridpramsstruct values
;		to avoid recalculations when reloading save set
;		changed instr to instrument in call to for_specdefaults
;	March 2023 
;		put in check for q,u = 0 in atan
;		also put in check for Izero and not null so that Azimuth not set to zero
;	April 2023
;		removed default occult disk for NEVIII
;	May 2023
;		let default be stored occult for all UV specpol
;	July 2023
;		added IONDENS
;	August 2023
;		added string around xxmin/max for comparison
;       Oct 2023- passed through line to for_specdefaults

;       Feb 2024 -- added mgix706
;		updated use of PlotSave so if run with moreplots = 0 
;		will default to plot choices for readmap save file
;		but checked first for newline and went to defaults if so
;		also only did check against ObsInputs and GridInputs if 
;		they exist (if coming from widget via 
;		widget_temp.sav, or using model save set -- not data)
;		also let loaded save map default to obsinputs.rfilter
;		   note rfilter is not in any of the *structs structures
;	Mar 2024 -- fixed bugs where line command changes to plotting for other line 
;		save set were overwritten
;		*Note -- saving on top of a save set will still not
;		 save line command changes to structures other than PlotSave 
;		 this means when updating an image using widget, and then saving
;		 wont include update unless recalculation done (so will miss
;		 things like cropping an image, changing rfilter)
;		 And this is particularly annoying for DATA because
;		  FORWARD will always be operating on top of a save set.
;		 A workaround from the widget is to click on Recalculate
;		 instead of FORWARD if saving a file -- but note that can
;		 be annoying if running a slow model. I'll fix this when I can.
;       Jun 2024 -- added BPOS column variables
;	Jul 2024 - added hooks for CIRTOY
;	Aug 2024 -- added hooks for WLRAT
;-


common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops

if n_elements(flag) eq 0 then flag={magmod:'',tab:0,reset:-1,widgregen:0,noerase:0,dta:0,mdl:0,rmp:0,obs:0,noise:0,rfil:0}
if n_elements(widgets) eq 0 then widgets={top:'',modb:'',plotb:'',setsb:'',disb:'',outb:'',date:'',help:'',load:''}

slash=path_sep()

for_settingdefaults,date=date,readprams=readprams,$
        readmap=readmap,usedfile=usedfile,noplots=noplots,$
	bg_input=bg_input,$
	bg_output=bg_output,$
        norunmodel=norunmodel,working_dir=working_dir,memory=memory,parallel=parallel,$
        datadump=datadump,verbose=verbose,reinit=reinit,nreinit=nreinit,noforward=noforward

for_outputdefaults,$
        saveprams=saveprams,savemap=savemap,mapname=mapname,$
        extratitle=extratitle,$
        noerase=noerase,moreplots=moreplots,$
        gif=gif,tiff=tiff,jpeg=jpeg,$
        psplot=psplot,eps=eps

ModelInputs=1
GridInputs=1
ObsInputs=1
LosInputs=1

;
; run forward calculation
;

if readmap eq '' then begin 
  
  if keyword_set(instrument) then if strupcase(instrument) eq 'GREENCOMP' then instrument='CORMAG'

  if keyword_set(radio) then instrument='RADIO'
  if keyword_set(faraday) then instrument='FARADAY'
  if keyword_set(ovi1032) then instrument='OVI1032'
  if keyword_set(ovi1037) then instrument='OVI1037'
  if keyword_set(neviii770) then insrument='NEVIII770'
  if keyword_set(neviii780) then insrument='NEVIII780'
  if keyword_set(lya) then instrument='LYA'
  if keyword_set(mgix706) then instrument='MGIX706'

;
; uses date,readprams,working_dir from settings; saveprams from output
;	(note date ultimately saved in ObsPramsStruct)
;

  for_modeldefaults,model,ModPramsStruct=ModPramsStruct,defineprams=defineprams,date=date,working_dir=working_dir,readprams=readprams,saveprams=saveprams,nreinit=nreinit,nowidgmess=nowidgmess,_extra=extra,ModelInputs=ModelInputs

;
; default BANG and CMER should be appropriate for the date 
; This default can always be overwritten
; by explicit definition of keyword BANG=BANG or CMER=CMER in call to for_drive.
; 

  for_viewfromdata,usedfile=usedfile,$
		date=date,gridtype=gridtype,xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,$
		ngrid=ngrid,ngy=ngy,cmer=cmer,bang=bang,aia=aia,euvia=euvia,euvib=euvib,$
	        line=line,instrument=instrument

;
; uses magmod
; also ngrid and ngy and gridtype
; xxmin,xxmax,yymin,yymax
;
  for_obsdefaults,ModPramsStruct.MagMod,ModPramsStruct.Name,ngrid=ngrid,ngy=ngy,xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,limb=limb,gridtype=gridtype,$
        line=line,instrument=instrument,pos=pos,frequency_MHz=frequency_MHz,dogyro=dogyro,fcor=fcor,ulimb=ulimb,rotaz=rotaz,wavelength_Ang=wavelength_Ang,wavelength2_Ang=wavelength2_Ang,numion=numion,labelonly=labelonly,obsloslimit=obsloslimit,ObsInputs=ObsInputs,$
	donoise=donoise,seespec=seespec,aperture=aperture,norandom=norandom,resolution=resolution,integration=integration,modeffint=modeffint,modeffquant=modeffquant,efficiency=efficiency,background=background,tscope=tscope,errtrunc=errtrunc,NoisePrams=NoisePrams,$
	xrt=xrt,eit=eit,wl=wl,iris=iris,myspect=myspect,iondens=iondens,cds=cds,eis=eis,neviii770=neviii770,neviii780=neviii780,ovi1032=ovi1032,ovi1037=ovi1037,mgix706=mgix706,lya=lya,radio=radio,faraday=faraday,aia=aia,$
	euvia=euvia,euvib=euvib,trace=trace,swap=swap,kcor=kcor,cormag=cormag,rfilter=rfilter,$
	comp=comp,fe11comp=fe11comp,si9comp=si9comp,othercomp=othercomp,si10comp=si10comp,greencomp=greencomp,swss=swss,losem=losem,colden=colden,benergy=benergy,ben_dens_int=ben_dens_int,b_int=b_int,b_dens_int=b_dens_int,b_pos_int=b_pos_int,b_pos_dens_int=b_pos_dens_int,$
	pop2losem=pop2losem,pop2colden=pop2colden,pop2abundance=pop2abundance,pop2ionfrac=pop2ionfrac,$
	pop2tregime=pop2tregime,abundance=abundance,cversion=cversion,UserSpecFiles=UserSpecFiles,$
        ioneq=ioneq,UserTResp=UserTResp,PITeamResp=PITeamResp,LWidth=LWidth,LLim=LLim,InGofNT=InGofNT,OutGofNT=OutGofNT,$
	nangleint=nangleint,collkey=collkey,isotropic=isotropic,wpar_open=wpar_open,wpar_closed=wpar_closed,aniso_open=aniso_open,$
        chromorad=chromorad,blend=blend,einsteina=einsteina,einsteinb=einsteinb,gj=gj,$
	SpecPrams=SpecPrams,FCompPrams=FCompPrams,tp2te=tp2te,for_wlmin=for_wlmin,for_wlmax=for_wlmax,$
        smalln=smalln,qnorm=qnorm,cecoeff=cecoeff,icoll=icoll,isum=isum,isplin=isplin,$
        iwatom=iwatom,iwline=iwline,vintchoice=vintchoice,iweqi=iweqi,idebug=idebug,fiwatmo=fiwatmo,seecomp=seecomp,noftran=noftran,crtn=crtn,working_dir=working_dir,azequi=azequi,distobs=distobs,nowidgmess=nowidgmess

;
; uses magmod, pos
;
; note pos may be changed to -1 for physical diagnostic
; also instrument changed from none

  linenew=line
  instrumentnew=instrument
  magmodnew=ModPramsStruct.MagMod
  modnamenew=ModPramsStruct.Name
  posnew=pos
  frequency_MHznew=frequency_Mhz
  dogyronew=dogyro
  fcornew=fcor
  ulimbnew=ulimb
  rotaznew=rotaz
  Wavelength_Angnew=wavelength_ang
  Wavelength2_Angnew=wavelength2_ang
  NumIonnew=numion
  ObsLosLimitnew=obsloslimit
  Datenew=date
  NoisePramsnew=noiseprams
  Pop2Tregimenew=pop2tregime
  SpecPramsnew=SpecPrams
  FCompPramsnew=FCompPrams

  obstemp= for_obs_name(magmodnew,modnamenew,linenew,instrumentnew,posnew,frequency_MHznew,dogyronew,fcornew,ulimbnew,$
   rotaznew,Wavelength_Angnew,Wavelength2_Angnew,NumIonnew,0,ObsLosLimitnew,Datenew,NoisePramsnew,Pop2Tregimenew,SpecPramsnew,$
   FCompPramsnew,working_dir=working_dir)

  pos=posnew
  instrument=instrumentnew
  line=linenew
;
; note pos affects losoffset

  for_griddefaults,ModPramsStruct.Name,pos,instrument,line,$
        gridtype=gridtype,ruser=ruser,thuser=thuser,phuser=phuser,coorduser=coorduser,phio=phio,$
        xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,limb=limb,ngrid=ngrid,ngy=ngy,$
        losoffset=losoffset,rheight=rheight,GridInputs=GridInputs,azequi=azequi,distobs=distobs,nowidgmess=nowidgmess

;
; uses model name, gridtype, instrument, pos
;   - note pos may change from +/-1 to +/-2 if TLOS
;

  for_losdefaults,ModPramsStruct.Name,gridtype,rheight,limb,instrument,line,pos,azequi,$
        losuse=losuse,dodisk=dodisk,axisym=axisym,bang=bang,incres=incres,thetao=thetao,phio=phio,$
	losmin=losmin,losint=losint,nlos=nlos,nostretch=nostretch,$
        occult=occult,upoccult=upoccult,cmer=cmer,LosInputs=LosInputs,nowidgmess=nowidgmess

;
; Build the structures
;

  for_getstructs,$
                 GridPramsStruct,ObsPramsStruct,LosPramsStruct,ModPramsStruct,$
                 gridtype,ngrid,ngy,rheight,$
                 xxmin,xxmax,yymin,yymax,losoffset,$
                 limb,cmer,phio,losmin,losint,nlos,axisym,nostretch,$
		 azequi,distobs,$
		 dodisk,losuse,occult,upoccult,$
		 ruser,thuser,phuser,coorduser,bang,thetao,$
		 line,incres,instrument,pos,frequency_MHz,dogyro,fcor,ulimb,rotaz,wavelength_Ang,wavelength2_Ang,numion,labelonly,obsloslimit,date,working_dir,$
		  pop2tregime,SpecPrams,NoisePrams,FCompPrams,$
		  xrt=xrt,eit=eit,wl=wl,iris=iris,myspect=myspect,iondens=iondens,cds=cds,eis=eis,neviii770=neviii770,neviii780=neviii780,ovi1032=ovi1032,ovi1037=ovi1037,mgix706=mgix706,lya=lya,radio=radio,faraday=faraday,aia=aia,$
		  euvia=euvia,euvib=euvib,trace=trace,swap=swap,kcor=kcor,cormag=cormag,$
	          pop2colden=pop2colden,pop2losem=pop2losem,$
		  comp=comp,fe11comp=fe11comp,si9comp=si9comp,othercomp=othercomp,si10comp=si10comp,greencomp=greencomp,swss=swss,losem=losem,colden=colden,ben_dens_int=ben_dens_int,b_int=b_int,b_dens_int=b_dens_int,b_pos_int=b_pos_int,b_pos_dens_int=b_pos_dens_int,benergy=benergy

;
; Run the forward model 
;
  
;
; put in a placeholder for Quantmap and StokesStruct
;

   QuantMap={Name:'empty',Ctype:'empty'}
   StokesStruct={Name:'empty',Ctype:'empty'}
   if noforward eq 0 then $
     for_forwardmap,Quantmap,ModPramsStruct,GridPramsStruct,ObsPramsStruct,LosPramsStruct,StokesStruct,WLStruct,ModSolStruct,$
                  bg_input=bg_input,bg_output=bg_output,$
                  nreinit,reinit,memory,datadump,extratitle,working_dir,parallel=parallel,verbose=verbose,nowidgmess=nowidgmess,mapname=mapname
                                    
;
; end no readmap.
; if readmap, then use 
; already calculated map
;

endif else begin
  
;
; put in a placeholder for StokesStruct and WLStruct
;

   StokesStruct={Name:'empty',Ctype:'empty'}
   WLStruct={Name:'empty',Ctype:'empty'}
;
;  set a variable to check for change in line (e.g. Stokes)

   newline=0
;
;  Get QuantMap and perhaps StokesStruct structures from save file:
;  Also input and output parameter files
;

   if strpos(readmap,'.sav') lt 0 then readmapuse=readmap+'.sav' else readmapuse=readmap
   if file_exist(readmapuse) eq 0 then begin
    if n_elements(working_dir) eq 1 then begin
       if working_dir ne '' then readmapuse = working_dir+slash+readmapuse
       if file_exist(readmapuse) eq 0 then message,'no file :'+readmapuse
    endif else message,'no file :'+readmapuse
   endif 
   restore,readmapuse

;
; save parameters if requested
;
  if keyword_set(saveprams) then $
    for_modeldefaults,ModPramsStruct.Name,working_dir=working_dir,saveprams=saveprams,nowidgmess=nowidgmess,_extra=extra

;
; rfilter is saved in obsinputs not obspramsstruct
; so if obsinputs exists, default to that
;
        if var_type(ObsInputs) eq 8 then begin
         default,rfilter,ObsInputs.rfilter
	endif
;
; special case AIA DATA, check to see if radial gradient filter
; has been applied already, if so overwrite any inputted rfilter keyword
;

  if strupcase(ObsPramsStruct.Instrument) eq 'AIA' and strupcase(ModPramsStruct.Name) eq 'DATA' then begin 
   default,rfilter,'no_filter'
   if strupcase(rfilter) eq 'AIA_RFILTER' then rfilter='no_filter'
   if tag_exist(QuantMap,'BUnit') then if strpos(strupcase(QuantMap.Bunit),'FILTER') ge 0 then rfilter='aia_rfilter'
   if strpos(strupcase(readmap),'RFILTER') ge 0 then rfilter='aia_rfilter'
  endif 

;
; set flag.rfil for rfilter

  for_obsdefaults,ModPramsStruct.MagMod,ModPramsStruct.Name,gridtype=GridPramsStruct.GridType,rfilter=rfilter

;
; set some new parameters if old file
;

  if strupcase(ObsPramsStruct.Instrument) eq 'GREENCOMP' then ObsPramsStruct.Instrument='CORMAG'

;
; if CoMP type StokesStruct made before central wavelength and  intensity
; added, put in something sensible
; NOTE there will be an error here in model, in size of eqwidth
; which will effectively have been set to one (which isnt bad for 10747)
;  error only shows up in non-original-model unit translation
;

   if (strpos(strupcase(ObsPramsStruct.Instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' $
        or strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS') $
	and not tag_exist(StokesStruct,'CentWave') then begin
     if strupcase(ObsPramsStruct.Instrument) eq 'COMP' or strupcase(ObsPramsStruct.Instrument) eq 'WAVECOMP' then lambda=10747.
     if strupcase(ObsPramsStruct.Instrument) eq 'OTHERCOMP' then lambda=10798.
     if strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' then lambda=5303.
     if strupcase(ObsPramsStruct.Instrument) eq 'FE11COMP' then lambda=7892.
     if strupcase(ObsPramsStruct.Instrument) eq 'SI9COMP' then lambda=39267.
     if strupcase(ObsPramsStruct.Instrument) eq 'SI10COMP' then lambda=14302.
     if strupcase(ObsPramsStruct.Instrument) eq 'LYA' then lambda=1215.67
     if strupcase(ObsPramsStruct.Instrument) eq 'OVI1032' then lambda=1031.91
     if strupcase(ObsPramsStruct.Instrument) eq 'OVI1037' then lambda=1037.61
     if strupcase(ObsPramsStruct.Instrument) eq 'NEVIII770' then lambda=770.42
     if strupcase(ObsPramsStruct.Instrument) eq 'NEVIII780' then lambda=780.39
     if strupcase(ObsPramsStruct.Instrument) eq 'MGIX706' then lambda=706.06
     StokesStruct=add_tag(StokesStruct,lambda,'CentWave')
   endif

   if (strpos(strupcase(ObsPramsStruct.Instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' $
        or strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS') $
        and not tag_exist(StokesStruct,'CentI') then begin
     StokesStruct=add_tag(StokesStruct,StokesStruct.I,'CentI')
   endif

   if strupcase(ModPramsStruct.Name) eq 'DATACUBE' then ModPramsStruct.Name='numcube'
   if not tag_exist(ModPramsStruct,'MagMod')  then begin
        if strupcase(ModPramsStruct.Name) ne 'CAVMORPH' and strupcase(ModPramsStruct.Name) ne 'TOMO' $
                        and strupcase(ModPramsStruct.Name) ne 'CROISSANT' $
                        and strupcase(ModPramsStruct.Name) ne 'CIRTOY' $
                        and strupcase(ModPramsStruct.Name) ne 'TURBHY' $
                        then ModPramsStruct=add_tag(ModPramsStruct,1,'MagMod')
        if strupcase(ModPramsStruct.Name) eq 'CAVMORPH' or strupcase(ModPramsStruct.Name) eq 'TOMO' $
                        or strupcase(ModPramsStruct.Name) eq 'CROISSANT' $
                        or strupcase(ModPramsStruct.Name) eq 'CIRTOY' $
                        then ModPramsStruct=add_tag(ModPramsStruct,0,'MagMod')
        if strupcase(ModPramsStruct.Name) eq 'TURBHY' $
                        then ModPramsStruct=add_tag(ModPramsStruct,2,'MagMod')
        if strupcase(ModPramsStruct.Name) eq 'DATA' and (strupcase(ObsPramsStruct.Instrument) eq 'COMP' $
                        or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG') $
                        then ModPramsStruct=add_tag(ModPramsStruct,-1,'MagMod')
        if strupcase(ModPramsStruct.Name) eq 'DATA' and (strupcase(ObsPramsStruct.Instrument) ne 'COMP' $
                        and strupcase(ObsPramsStruct.Instrument) ne 'CORMAG') $
                        then ModPramsStruct=add_tag(ModPramsStruct,-2,'MagMod')
   endif
   if not tag_exist(ObsPramsStruct,'ObsLosLimit')  then ObsPramsStruct=add_tag(ObsPramsStruct,0.d0,'ObsLosLimit')
   if not tag_exist(ObsPramsStruct,'Frequency_MHz')  then ObsPramsStruct=add_tag(ObsPramsStruct,0.d0,'Frequency_MHz')
   if not tag_exist(ObsPramsStruct,'DoGyro')  then ObsPramsStruct=add_tag(ObsPramsStruct,0.d0,'DoGyro')
   if not tag_exist(ObsPramsStruct,'FCor')  then ObsPramsStruct=add_tag(ObsPramsStruct,0.d0,'FCor')
   if not tag_exist(ObsPramsStruct,'ULimb')  then ObsPramsStruct=add_tag(ObsPramsStruct,0.63d0,'ULimb')
;   if (strpos(strupcase(ObsPramsStruct.Instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' $
;        or strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS') $
;	and (strpos(strupcase(ObsPramsStruct.LineName),'Q') gt 0 or strpos(strupcase(ObsPramsStruct.linename),'U') gt 0 or strpos(strupcase(ObsPramsStruct.LineName),'AZ') gt 0) then begin
   if strpos(strupcase(ObsPramsStruct.Instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' or $
        strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' $
       then begin
    if exist(rotaz) eq 0 then rotaz=''
    rotazset=''
    if not tag_exist(ObsPramsStruct,'RotAz')  then ObsPramsStruct=add_tag(ObsPramsStruct,rotaz,'RotAz') else begin
      rotazset=ObsPramsStruct.RotAz
      if rotaz ne '' then ObsPramsStruct.RotAz=rotaz
    endelse
   endif else begin
    if not tag_exist(ObsPramsStruct,'RotAz')  then ObsPramsStruct=add_tag(ObsPramsStruct,'NULL','RotAz') else ObsPramsStruct.RotAz='NULL'
   endelse
   if not tag_exist(ObsPramsStruct,'Wavelength_Ang')  then ObsPramsStruct=add_tag(ObsPramsStruct,0,'Wavelength_Ang')
   if not tag_exist(ObsPramsStruct,'Wavelength2_Ang')  then ObsPramsStruct=add_tag(ObsPramsStruct,0,'Wavelength2_Ang')
   if not tag_exist(ObsPramsStruct,'NumIon')  then ObsPramsStruct=add_tag(ObsPramsStruct,0,'NumIon')
   if not tag_exist(ObsPramsStruct,'Date')  then ObsPramsStruct=add_tag(ObsPramsStruct,'','Date')
   if exist(pos) eq 0 then begin
     if strpos(strupcase(readmap),'POS') ge 0 then pos=1.0 else pos=0.0
   endif
   if not tag_exist(ObsPramsStruct,'POS')  then ObsPramsStruct=add_tag(ObsPramsStruct,pos,'POS')
   if not tag_exist(ObsPramsStruct,'Pop2TRegime')  then begin
    if keyword_set(pop2tregime) then ObsPramsStruct=add_tag(ObsPramsStruct,pop2tregime,'Pop2TRegime') else $
      ObsPramsStruct=add_tag(ObsPramsStruct,0,'Pop2TRegime')
   endif
   if not tag_exist(ObsPramsStruct,'IClass') then begin
     if ObsPramsStruct.Pop2TRegime ne 0 then pop2on=1 else pop2on=0
     for_obs_choose,ModPramsStruct.MagMod,ModPramsStruct.Name,ObsPramsStruct.LineName,ObsPramsStruct.Instrument,ObsPramsStruct.Label,ObsPramsStruct.LineNum,type,pop2on=pop2on,working_dir=working_dir
     ObsPramsStruct=add_tag(ObsPramsStruct,type,'IClass')   
   endif


   if not tag_exist(LosPramsStruct,'DoDisk')  then LosPramsStruct=add_tag(LosPramsStruct,0,'DoDisk')
   if not tag_exist(LosPramsStruct,'Occult')  then LosPramsStruct=add_tag(LosPramsStruct,1.,'Occult')
   if not tag_exist(LosPramsStruct,'NoStretch')  then LosPramsStruct=add_tag(LosPramsStruct,0.,'NoStretch')
;
; I think below was to fix a 0 to O problem
;
   if tag_exist(GridPramsStruct,'Phi0') then GridPramsStruct=add_tag(GridPramsStruct,GridPramsStruct.Phi0,'phio')
   if not tag_exist(GridPramsStruct,'AzEqui') then GridPramsStruct=add_tag(GridPramsStruct,0.,'AzEqui')
   if not tag_exist(GridPramsStruct,'DistObs') then GridPramsStruct=add_tag(GridPramsStruct,215.,'DistObs')
   if tag_exist(GridPramsStruct,'Coord')  then begin
	if strupcase(GridPramsStruct.Coord) eq 'CART' then begin
		if is_number(LosPramsStruct.Occult) then begin
		  LosPramsStruct.Occult=-1.*LosPramsStruct.Occult
		endif
	endif
   endif
   if not tag_exist(GridPramsStruct,'LosOffset') then $
     GridPramsStruct=add_tag(GridPramsStruct,GridPramsStruct.XOffset,'LosOffset')
   if not tag_exist(LosPramsStruct,'UpOccult')  then LosPramsStruct=add_tag(LosPramsStruct,1000.,'UpOccult')
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

;
; Make sure that the Noise, Spec and CoMP  structures are set up right
;

   if strupcase(GridPramsStruct.GridType) eq 'PLANEOFSKY' then begin
     resolution=959.63d0*(GridPramsStruct.Dx>GridPramsStruct.Dy) 
   endif else begin
     dlat=GridPramsStruct.Dy ; pixel size in degrees, latitude
     C=959.63d0*2.d0*!dpi
     resolution=dlat*C/360.d0
   endelse

;
; save modeffquant in case line is different (see below)
;
    if exist(modeffquant) then modeffquantuse=modeffquant
;
; CompPrams0 was empty, so resetting in saved file -- fixed below
   if tag_exist(ObsPramsStruct,'FCompPrams') then CompPrams0=ObsPramsStruct.FCompPrams

   if strupcase(ModPramsStruct.Name) ne 'DATA' then begin
    FCompPrams=for_compdefaults(CompPrams0,ObsPramsStruct.Instrument,ObsPramsStruct.LineName)
    NoisePrams=for_noisedefaults(ObsPramsStruct.Instrument,ObsPramsStruct.LineName,ModPramsStruct.Name,donoise=donoise,aperture=aperture,norandom=norandom,resolution=resolution,integration=integration,modeffint=modeffint,modeffquant=modeffquant,efficiency=efficiency,background=background,tscope=tscope,errtrunc=errtrunc,azequi=azequi,distobs=distobs,nowidgmess=nowidgmess)
    SpecPrams=for_specdefaults(instrument=ObsPramsStruct.Instrument,line=ObsPramsStruct.LineName,IClass=ObsPramsStruct.IClass,working_dir=working_dir)
   endif else begin
    FCompPrams=for_compdefaults(CompPrams0,'dummy','dummy')
    NoisePrams={DoNoise:0.d0,DoNoiseVal:'tog',Aperture:0.d0,ApertureVal:'double',Resolution:0.d0,ResolutionVal:'nodisplay',Integration:0.d0,IntegrationVal:'double',ModEffInt:0.d0,ModEffIntVal:'double',ModEffQuant:0.d0,ModEffQuantVal:'double',Efficiency:0.d0,EfficiencyVal:'double',Background:0.d0,BackgroundVal:'double',ErrTrunc:0.d0,ErrTruncVal:'double',Tscope:'NULL',TScopeVal:'nodisplay',NoRandom:0,NoRandomVal:'tog'}
    SpecPrams=for_specdefaults(ioneq='data',instrument=ObsPramsStruct.Instrument,line=ObsPramsStruct.LineName,IClass=ObsPramsStruct.IClass,working_dir=working_dir)
   endelse

   ObsPramsStruct = for_obs_name(ModPramsStruct.MagMod,ModPramsStruct.Name,ObsPramsStruct.LineName[0],ObsPramsStruct.Instrument,ObsPramsStruct.Pos,ObsPramsStruct.Frequency_MHz,ObsPramsStruct.DoGyro,ObsPramsStruct.FCor,ObsPramsStruct.ULimb,ObsPramsStruct.RotAz,ObsPramsStruct.Wavelength_Ang,ObsPramsStruct.Wavelength2_Ang,ObsPramsStruct.NumIon,0,ObsPramsStruct.ObsLosLimit,ObsPramsStruct.Date,NoisePrams,ObsPramsStruct.Pop2Tregime,SpecPrams,FCompPrams,working_dir=working_dir) 


; make sure correct line is used

   if keyword_set(line) ne 0 then begin

;  for CoMP and RADIO and UV, have the option of using a readmap file generated for one Stokes parameter
;  and making plot for one of the others, since all are saved
;  same for FARADAY, although there StokesI and StokesV are placeholder
;  for Faraday RM and FR
   
    if strpos(strupcase(ObsPramsStruct.Instrument),'OMP') gt 0 or $
	strupcase(ObsPramsStruct.Instrument) eq 'RADIO' or $
	strupcase(ObsPramsStruct.Instrument) eq 'FARADAY' or $
        strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' or $
	strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' then begin

      if strupcase(ObsPramsStruct.LineName) ne 'STOKES'+strupcase(line) and strupcase(ObsPramsStruct.LineName) ne strupcase(line) then begin
          newline=1
          if exist(modeffquantuse) then modeffquant=modeffquantuse else undefine,modeffquant
          NoisePrams=for_noisedefaults(ObsPramsStruct.Instrument,line,ModPramsStruct.Name,donoise=donoise,aperture=aperture,norandom=norandom,resolution=resolution,integration=integration,modeffint=modeffint,modeffquant=modeffquant,efficiency=efficiency,background=background,tscope=tscope,errtrunc=errtrunc,azequi=azequi,distobs=distobs,nowidgmess=nowidgmess)
	  ObsPramsStruct = for_obs_name(ModPramsStruct.MagMod,ModPramsStruct.Name,line,ObsPramsStruct.Instrument,ObsPramsStruct.Pos,ObsPramsStruct.Frequency_MHz,ObsPramsStruct.DoGyro,ObsPramsStruct.FCor,ObsPramsStruct.ULimb,ObsPramsStruct.RotAz,ObsPramsStruct.Wavelength_Ang,ObsPramsStruct.Wavelength2_Ang,ObsPramsStruct.NumIon,0,ObsPramsStruct.ObsLosLimit,ObsPramsStruct.Date,NoisePrams,ObsPramsStruct.Pop2Tregime,ObsPramsStruct.SpecPrams,ObsPramsStruct.FCompPrams,working_dir=working_dir) 
;      assign appropriate StokesStruct parameter to QuantMap:

        Iuse = StokesStruct.I
        Icentuse = StokesStruct.CentI
        centwave=StokesStruct.CentWave
        Izero=where(Iuse eq 0.)
        if min(Izero) ge 0. then begin
         Icentuse[Izero] = 1.
         Iuse[Izero] = 1.
        endif

        case 1 of
         (strupcase(ObsPramsStruct.LineName) eq 'RM'): intensall = StokesStruct.I
         (strupcase(ObsPramsStruct.LineName) eq 'FR'): intensall = StokesStruct.V
         (strupcase(ObsPramsStruct.LineName) eq 'STOKESI'): intensall = StokesStruct.I
         (strupcase(ObsPramsStruct.LineName) eq 'STOKESV'): intensall = StokesStruct.V
         (strupcase(ObsPramsStruct.LineName) eq 'STOKESW'): intensall = StokesStruct.W
         (strupcase(ObsPramsStruct.LineName) eq 'DOPPLERVLOS'): intensall = StokesStruct.velocities
         (strpos(strupcase(ObsPramsStruct.LineName),'VOI') ge 0 ) : intensall = StokesStruct.V/Iuse
         ((strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS') and $
          (strupcase(ObsPramsStruct.LineName) eq 'STOKESQ')):  intensall = StokesStruct.Q
         ((strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS') and $
          (strupcase(ObsPramsStruct.LineName) eq 'STOKESU')): intensall = StokesStruct.U
         ((strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS') and $
          (strpos(strupcase(ObsPramsStruct.LineName),'QOI') ge 0 )) : intensall = StokesStruct.Q/Iuse
         ((strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS') and $
          (strpos(strupcase(ObsPramsStruct.LineName),'UOI') ge 0 )) : intensall = StokesStruct.U/Iuse
         ((strupcase(ObsPramsStruct.Instrument) eq 'CORMAG') or $
          (strpos(strupcase(ObsPramsStruct.Instrument),'OMP') ge 0) or strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS') and $
          (strpos(strupcase(ObsPramsStruct.LineName),'LOI') ge 0  or $
	  strupcase(ObsPramsStruct.LineName) eq 'STOKESL' or $
          strpos(strupcase(ObsPramsStruct.LineName),'POI') ge 0  or $
	  strupcase(ObsPramsStruct.LineName) eq 'STOKESP' or $
; this is confusing because P later means L/I in for_personality
;	but it is only here for limited backward compatability with old files
;       and immediately changed	to LOI
          strpos(strupcase(ObsPramsStruct.LineName),'AZ') ge 0  or $
          strupcase(ObsPramsStruct.LineName) eq 'STOKESQ' or $
          strupcase(ObsPramsStruct.LineName) eq 'STOKESU' or $
          strpos(strupcase(ObsPramsStruct.LineName),'QOI') ge 0  or $
          strpos(strupcase(ObsPramsStruct.LineName),'UOI') ge 0  or $
          strupcase(ObsPramsStruct.LineName) eq 'LINEWIDTH'): begin

;
;  Determine L/I (L = linear polarization vector magnitude)
;
	    if strpos(strupcase(ObsPramsStruct.LineName),'POI') ge 0 then ObsPramsStruct.LineName = 'StokesLoI'
	    if (strupcase(ObsPramsStruct.LineName) eq 'STOKESP') then ObsPramsStruct.LineName = 'StokesL'

;
; convert to radial reference frame if old CoMP file
;
            Qprime = StokesStruct.Q
            Uprime = StokesStruct.U

	    if strupcase(rotazset) eq '' then for_changeref,GridPramsStruct.ThPos,StokesStruct.Q,StokesStruct.U,Qprime,Uprime,type=0
 
            q = Qprime
            u = Uprime

	    StokesStruct.Q=Q
	    StokesStruct.U=U

            p = sqrt(q*q+u*u)

            case 1 of
             (strupcase(ObsPramsStruct.LineName) eq 'STOKESQ'): intensall = Q
             (strupcase(ObsPramsStruct.LineName) eq 'STOKESU'): intensall = U
             (strpos(strupcase(ObsPramsStruct.LineName),'QOI') ge 0 ) : intensall = Q/Iuse
             (strpos(strupcase(ObsPramsStruct.LineName),'UOI') ge 0 ) : intensall = U/Iuse
             (strupcase(ObsPramsStruct.LineName) eq 'STOKESL'): intensall = P
             (strpos(strupcase(ObsPramsStruct.LineName),'LOI') ge 0 ) : intensall = P/Iuse
             (strupcase(ObsPramsStruct.LineName) eq 'LINEWIDTH'): intensall=3.d5*Iuse/Icentuse/double(centwave)
;
; note this is an equivalent width while the data will be a FWHM
;
             strpos(strupcase(ObsPramsStruct.LineName),'AZ') ge 0: begin
                  alpha = 0.5*atan(u,q)
		  test = where(u eq 0. and q eq 0.)
                  if min(test) ne -1 then alpha[test] = sqrt(-1.)
 	          mdtor=!dpi/180d0
                  intensall = alpha/mdtor
		  intensall = intensall mod 180.d0
		  test=where(intensall lt 0.)
		  if min(test) ne -1 then intensall[test] = intensall[test]+180.d0
             end
            endcase
;
; make sure underdisk and null data points are identified
;
	    test=where(Iuse eq -8888.)
 	    if min(test) ne -1 then intensall[test]=-8888.
	    test=where(intensall*0. ne 0.)
 	    if min(test) ne -1 then intensall[test]=-8888.
	    test=where(Iuse eq -9999.)
 	    if min(test) ne -1 then intensall[test]=-9999.

         end
        endcase

        Izero_notnull=where(Iuse eq 0. and intensall ne -8888)
        if min(Izero_notnull) ge 0. then intensall[Izero_notnull] = 0.

;
; not sure why I do this --  maybe there is a situation where intensall
;  is a scalar multiple?
;  runs into problems if last Quantmap.data is array of zeros
;  revising to be careful of this
;
;        QuantMap.data = intensall*QuantMap.data/QuantMap.data
	datarrayones=Quantmap.data*0. + 1.d0
	badones=where(Quantmap.data*0. ne 0.)
	if min(badones) ne -1 then datarrayones[badones] = 1.d0
        QuantMap.data = intensall*datarrayones
;
; make sure units are updated
;
	    QuantMap.BUnit='NULL'
	    modelon=1
	    if strpos(QuantMap.ID,'DATA') ge 0 then modelon=0
  	    for_fixunits,'NULL',QuantMap,ObsPramsStruct,StokesStruct=StokesStruct,model=modelon

      endif

    endif else begin

      if strtrim(strupcase(ObsPramsStruct.LineName),2) ne strtrim(strupcase(line),2) $
	and strtrim(strupcase(ObsPramsStruct.LineNum),2) ne strtrim(strupcase(line),2) then begin

;  same is true for POS slice of model parameters
; (although not for POS slice of observable, case LineNum ne -99)

        if keyword_set(pop2tregime) then pop2on=1 else pop2on=0
        for_obs_setup,ModPramsStruct.MagMod,ModPramsStruct.Name,phys_params=phys_params,pop2on=pop2on,working_dir=working_dir

        test=where(strupcase(phys_params) eq strupcase(line))
        if min(test) ne -1 then begin
	   ObsPramsStruct = for_obs_name(ModPramsStruct.MagMod,ModPramsStruct.Name,line,ObsPramsStruct.Instrument,ObsPramsStruct.Pos,ObsPramsStruct.Frequency_MHz,ObsPramsStruct.DoGyro,ObsPramsStruct.FCor,ObsPramsStruct.ULimb,ObsPramsStruct.RotAz,ObsPramsStruct.Wavelength_Ang,ObsPramsStruct.Wavelength2_Ang,ObsPramsStruct.NumIon,0,ObsPramsStruct.ObsLosLimit,ObsPramsStruct.Date,ObsPramsStruct.NoisePrams,ObsPramsStruct.Pop2Tregime,ObsPramsStruct.SpecPrams,ObsPramsStruct.FCompPrams,working_dir=working_dir) 

          for_posnoint,quantity,GridPramsStruct.Rpos,GridPramsSTruct.THpos,GridPramsStruct.PHpos,$
                LosPramsStruct,ObsPramsStruct,ModPramsStruct,GridPramsStruct,ModSolStruct,$
                norunmodel=norunmodel,nreinit=nreinit,nowidgmess=nowidgmess,working_dir=working_dir
;          norunmodel=1
; commented this out -- should not be needed because for_Settingdefaults will set
; norunmodel and so default,0 in for_posnoint should not have an effect.
; on the other hand, forcing norunmodel = 1 does not allow user to force a rerun,
; which could be necessary for old save sets.

;
          QuantMap=for_pos_map(quantity,ModPramsStruct,ObsPramsStruct,LosPramsStruct,GridPramsStruct,StokesStruct=StokesStruct,$
                      extratitle)
        endif else begin
            print,'Sorry, using default line='+ObsPramsStruct.LineName
        endelse

      endif

    endelse

; make sure labeling is right

    if strupcase(ModPramsStruct.Name) ne 'DATA' then begin 
     labelbit=ObsPramsStruct.Label
     left=strpos(ObsPramsStruct.Label,'(')
     right=strpos(ObsPramsStruct.Label,')')
     if left ne -1 and right ne -1 then begin
      labelbit=strmid(ObsPramsStruct.Label,0,left-1)
     endif
     QuantMap.ID=ModPramsStruct.Label + '!c' + labelbit + ' ' + extratitle
     QuantMap.Name=ModPramsStruct.Name+'_'+ObsPramsStruct.Instrument+'_'+ObsPramsStruct.LineName+'_'+extratitle
     if strupcase(ModPramsStruct.Name) eq 'TURBHY' then begin
         QuantMap.ID=QuantMap.ID + '!c DX=' + strtrim(string(GridPramsStruct.Dx),2)$
           + ' DY=' + strtrim(string(GridPramsStruct.Dy),2)+' (Rsun)'
         QuantMap.Name=QuantMap.Name+'_'+strtrim(string(GridPramsStruct.Dx),2)+'_'+strtrim(string(GridPramsStruct.Dy),2)
     endif
     if ObsPramsStruct.Pos ge 0 and strupcase(ObsPramsStruct.Instrument) ne 'PHYSICAL DIAGNOSTICS' and strupcase(ObsPramsStruct.Instrument) ne 'IONDENS' then begin
        if ObsPramsStruct.Pos gt 0 then QuantMap.ID=QuantMap.ID + '!c POS Integrated Over ' + strupcase(LosPramsStruct.LosUse) $
        else QuantMap.ID=QuantMap.ID + '!c Integrated Over ' + strupcase(LosPramsStruct.LosUse) 

        QuantMap.Name=QuantMap.Name+'_'+strupcase(LosPramsStruct.LosUse)
	if LosPramsStruct.NoStretch ne 0 then $
          QuantMap.ID=QuantMap.ID + ' (no stretch)'
	if strupcase(ModPramsStruct.Name) eq 'TURBHY' then begin
         if GridPramsStruct.AzEqui eq 0 then useun=' (Rsun)' $
           else useun=' (Degrees Elongation)'
         QuantMap.ID=QuantMap.ID + '; LOS step size=' + strtrim(string(LosPramsStruct.LosInt),2)+useun
         QuantMap.Name=QuantMap.Name+'_'+strtrim(string(LosPramsStruct.LosInt),2)
	endif
     endif
     if strpos(strupcase(ObsPramsStruct.Instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' or $
        strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' $
	then QuantMap.Name=ModPramsStruct.Name+'_'+ObsPramsStruct.Label+'_'+extratitle
    endif else begin
     if strpos(strupcase(ObsPramsStruct.Instrument),'COMP') ge 0  and anytim(date,/ccsds) gt anytim('2021-01-21',/ccsds) then begin
       QuantMap.Name='DATA_UCOMP_'+ObsPramsStruct.LineName+'_'+extratitle
     endif else begin
       QuantMap.Name='DATA_'+ObsPramsStruct.Instrument+'_'+ObsPramsStruct.LineName+'_'+extratitle
     endelse
    endelse

; 
; now do necessary shifts if Carrington map
; (keeping in old misspelling in case heritage files)
;

    if strupcase(QuantMap.CType) eq 'CARMAP' or strupcase(QuantMap.CType) eq 'CARRMAP' then begin
      if newline ne 0 then begin
	  DSize=Size(QuantMap.Data)
	  nph=DSize[1]
	  nth=DSize[2]
	  if strupcase(QuantMap.CarLimb) eq 'WEST' then nshift=nph/4. + nph/2.
	  if strupcase(QuantMap.CarLimb) eq 'EAST' then nshift=-nph/4. + nph/2.
	  if strupcase(QuantMap.CarLimb) eq 'CMER' then begin
	    nshift= nph/2.
	    QuantMap.Th0=180.d0
	  endif
	  for i = 0,nth-1 do begin
	    QuantMap.Data[*,i]=shift(QuantMap.Data[*,i],nshift)
	  endfor
      endif
      if strupcase(ModPramsStruct.Name) ne 'DATA' then begin 
       QuantMap.ID=QuantMap.ID+' r= '+$
                                  strtrim(string(QuantMap.Radii),1)+$
                                  ' '+QuantMap.CarLimb+' limb'
       QuantMap.Name=QuantMap.Name+'_'$
                                  +strtrim(string(QuantMap.Radii),1)+'_'+QuantMap.CarLimb
      endif
    endif

;
    if strupcase(ObsPramsStruct.instrument) eq 'RADIO' then if $
        strpos(strupcase(ObsPramsStruct.LineName),'VOI') lt 0 then $
	  QuantMap.BUnit='degrees Kelvin' else QuantMap.BUnit='fraction circular polarization'
    if strupcase(ObsPramsStruct.instrument) eq 'FARADAY' then $
	if strupcase(ObsPramsStruct.linename) eq 'RM' then QuantMap.BUnit='radians/m^2' else QuantMap.BUnit='radians'

   endif

; 
; in case file was made before changes to definition of underdisk and nulldata
; overwrite; also needed for Stokes parameters when variable different from the variable
; originally called 
;
   underdisk = where(GridPramsStruct.RPos lt 1.)
   if is_number(LosPramsStruct.DoDisk) then if min(underdisk) ne -1 and LosPramsStruct.DoDisk eq 0. then QuantMap.data[underdisk] = -9999.
;   nulldata = where(Quantmap.data eq 9999. or Quantmap.data eq 0. or Quantmap.data*0. ne 0.)
; I think it is dangerous to get rid of zero data. Hopefully this wont
; cause backwards incompatibility.
;
   nulldata = where(abs(Quantmap.data) eq 9999. or Quantmap.data*0. ne 0.)
   if min(nulldata) ne -1 then QuantMap.data[nulldata] = -8888.

;
; make sure something is available as noise
;
   Noise=QuantMap.data*0.d0
   if ObsPramsStruct.NoisePrams.DoNoise eq 1 then Noise=for_personality(QuantMap,ObsPramsStruct,StokesStruct)
   if tag_exist(QuantMap,'Noise') then QuantMap.Noise=Noise else QuantMap=add_tag(QuantMap,Noise,'Noise')
endelse
if mapname eq 'defaults' then mapname=QuantMap.name+strtrim(string(QuantMap.CType),2)

mapname=str_replace(mapname,'/','_')
mapname=str_replace(mapname,' ','_')
savename=mapname
if n_elements(working_dir) eq 1 then if working_dir ne '' then savename = working_dir+slash+mapname 
   
;
; use occult and upoccult and dodisk if inputted
; and possibly different that previous read-in LosPramsStruct value
; (this should only happen if the change obscures rather than displays data)
; (note there is a chance these may have 'NULL' values, so fix)
;
default,occult,LosPramsStruct.Occult
default,upoccult,LosPramsStruct.UpOccult
default,dodisk,LosPramsStruct.DoDisk
if strupcase(string(occult)) eq 'NULL' then occult=0.
if strupcase(string(upoccult)) eq 'NULL' then upoccult=1000.
if strupcase(string(dodisk)) eq 'NULL' then dodisk=0.d0
dodisk=double(dodisk)

; be careful -- if readmap was set then PlotSave might exist already
; and if so, we want to activate those plot settings as defaults
; unless they have been explicitly overridden by input keywords

if exist(PlotSave) eq 0 then begin
  PlotSave=1
endif else begin
;
; be careful of line change -- better to go to defaults for some things
  if newline ne 1 then begin
   t=tag_names(PlotSave)
   for i=0,n_elements(t)-1 do void=execute('default,'+t[i]+',PlotSave.(i)')
   t=tag_names(DoContInputs)
   for i=0,n_elements(t)-1 do void=execute('default,'+t[i]+',DoContInputs.(i)')
   t=tag_names(FieldLinesInputs)
   for i=0,n_elements(t)-1 do void=execute('default,'+t[i]+',FieldLinesInputs.(i)')
   t=tag_names(StkLinesInputs)
   for i=0,n_elements(t)-1 do void=execute('default,'+t[i]+',StkLinesInputs.(i)')
;
; check if rfilter is different than in save file (e.g., overridden by input)
; if so,unset imin, imax, plotlog, units
   if rfilter ne ObsInputs.rfilter then begin
     undefine,imin
     undefine,imax
     undefine,plotlog
     undefine,units
   endif
  endif

endelse

for_plotdefaults,ModPramsStruct.MagMod,ModPramsStruct.Name,gridtype=GridPramsStruct.GridType,dodisk=dodisk,noerase=noerase,line=ObsPramsStruct.LineName,instrument=ObsPramsStruct.Instrument,donoise=ObsPramsStruct.NoisePrams.DoNoise,rotaz=ObsPramsStruct.RotAz,pos=ObsPramsStruct.Pos,$
        colortable=colortable,plotlog=plotlog,imax=imax,imin=imin,usecolor=usecolor,axiscolor=axiscolor,bgcolor=bgcolor,$
        docont=docont,nocontcolor=nocontcolor,noclabel=noclabel,nlev=nlev,c_charsize=c_charsize,$
        winnumber=winnumber,nwinx=nwinx,nwiny=nwiny,$
	dispexp=dispexp,dispgamma=dispgamma,rpow=rpow,nobangletitle=nobangletitle,addturbtitle=addturbtitle,$
	nulldatacolor=nulldatacolor,sunedge=sunedge,whitedisk=whitedisk,$
        charsize=charsize,charthick=charthick,title=title,xtitle=xtitle,ytitle=ytitle,$
        fieldlines=fieldlines,bscale=bscale,units=units,bcolor=bcolor,bthick=bthick,bminuse=bminuse,narrow=narrow,nstarrow=nstarrow,$
        pscale=pscale,stklines=stklines,stkcolor=stkcolor,stkthick=stkthick,sminuse=sminuse,$
        arrowave=arrowave,azequi=azequi,distobs=distobs,PlotInputs=PlotSave

; Make plot(s) unless noplots flag is set or USER 

if noplots eq 0 and strupcase(GridPramsStruct.GridType) ne 'USERINPUT' and strupcase(GridPramsStruct.GridType) ne 'USER' then begin

; make sure plot range is correct

   xplotrange=GridPramsStruct.xrange
   yplotrange=GridPramsStruct.yrange
   if n_elements(xxmin) ne 0 or n_elements(xxmax) ne 0 or n_elements(yymin) ne 0 or n_elements(yymax) ne 0 then begin
    if n_elements(xxmin) eq 0 then xxmin=GridPramsStruct.xrange[0]
    if n_elements(xxmax) eq 0 then xxmax=GridPramsStruct.xrange[1]
    if n_elements(yymin) eq 0 then yymin=GridPramsStruct.yrange[0]
    if n_elements(yymax) eq 0 then yymax=GridPramsStruct.yrange[1]
    if is_number(xxmin) then xxmin=double(xxmin)
    if is_number(xxmax) then xxmax=double(xxmax)
    if is_number(yymin) then yymin=double(yymin)
    if is_number(yymax) then yymax=double(yymax)
    xplotrange=[xxmin,xxmax]
    yplotrange=[yymin,yymax]
   endif

   if keyword_set(addturbtitle) then addturbtitleuse=ModPramsStruct.Turb_Resolution else addturbtitleuse=0

   for_plot,QuantMap,GridPramsStruct,ObsPramsStruct=ObsPramsStruct,StokesStruct=StokesStruct,xplotrange=xplotrange,yplotrange=yplotrange,$
	dodisk=dodisk,psplot=psplot,eps=eps,usecolor=usecolor,docont=docont,$
        colortable=colortable,cpos=cpos,plotlog=plotlog,imax=imax,imin=imin,charsize=charsize,occult=occult,upoccult=upoccult,rfilter=rfilter,$
        charthick=charthick,mapname=mapname,winnumber=winnumber,noclabel=noclabel,whitedisk=whitedisk,c_charsize=c_charsize,working_dir=working_dir,$
	dispexp=dispexp,dispgamma=dispgamma,rpow=rpow,nobangletitle=nobangletitle,addturbtitle=addturbtitleuse,$
        sunedge=sunedge,nwinx=nwinx,nwiny=nwiny,noerase=noerase,axiscolor=axiscolor,bgcolor=bgcolor,nulldatacolor=nulldatacolor,nlev=nlev,units=units,$
        title=title,xtitle=xtitle,ytitle=ytitle,nocontcolor=nocontcolor,nowidgmess=nowidgmess,nodata
     
     if nodata ne 1 then begin

; If keyword fieldlines set, overplot fieldlines
     
      if abs(ModPramsStruct.MagMod) eq 1 then begin
       if strupcase(string(fieldlines)) eq 'NULL' then fieldlines=0
       if fieldlines eq 1 and strupcase(QuantMap.CType) eq 'DISK' and strupcase(ModPramsStruct.Name) ne 'DATA' then begin
	default,upoccult,LosPramsStruct.UpOccult
	default,occult,LosPramsStruct.Occult
        for_plotfieldlines,GridPramsStruct,LosPramsStruct,ObsPramsStruct,ModPramsStruct,norunmodel=norunmodel,occult=occult,upoccult=upoccult,$
          ModSolStruct,bscale=bscale,narrow=narrow,nreinit=nreinit,bcolor=bcolor,bthick=bthick,bminuse=bminuse,xplotrange=xplotrange,yplotrange=yplotrange,nowidgmess=nowidgmess,working_dir=working_dir
       endif
       if strupcase(string(stklines)) eq 'NULL' then stklines=0
       if stklines eq 1 and strupcase(QuantMap.CType) eq 'DISK' then begin
         if (strpos(strupcase(ObsPramsStruct.Instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' or $
           strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS') $
;	    strupcase(ObsPramsStruct.Instrument) eq 'LYA' or $
;	    strpos(strupcase(ObsPramsStruct.Instrument),'NEVIII') ge 0 or $
;	    strpos(strupcase(ObsPramsStruct.Instrument),'MGIX') ge 0 or $
;	    strpos(strupcase(ObsPramsStruct.Instrument),'OVI') ge 0) $
	   then begin
	   default,upoccult,LosPramsStruct.UpOccult
	   default,occult,LosPramsStruct.Occult
           for_plotstokeslines,GridPramsStruct,LosPramsStruct,ObsPramsStruct,ModPramsStruct,StokesStruct,$
		occult=occult,upoccult=upoccult,arrowave=arrowave,pscale=pscale,nstarrow=nstarrow,$
		stkcolor=stkcolor,stkthick=stkthick,sminuse=sminuse,xplotrange=xplotrange,$
		yplotrange=yplotrange
         endif
       endif
      endif
     
;    Save plots to file
     
      if !d.name ne 'PS' and (keyword_set(gif) ne 0 or keyword_set(tiff) ne 0 or keyword_set(jpeg) ne 0) then $
        for_save_plot,gif=gif,tiff=tiff,jpeg=jpeg,mapname=mapname,working_dir=working_dir
      if !d.name eq 'PS' and moreplots eq 0 then begin 
;
;	need to get this to work
;
;     		if keyword_set(eps) then pse else device,/close
		device,/close
      		set_plot,'X'
		cleanplot,/silent
      endif
     endif
endif else begin
; 
; change units if keyword set
;

 modelon=1
 if strpos(QuantMap.ID,'DATA') ge 0 then modelon=0
 unitsuse='NULL'
 if keyword_set(units) then unitsuse = units
 for_fixunits,unitsuse,QuantMap,ObsPramsStruct,StokesStruct=StokesStruct,model=modelon
endelse

if savemap ne 0 then begin

;
; make sure ObsInputs Line and Instrument are updated if making a save file
; same for noise, comp structures since they may change with change in line
;  be careful though because for old DATA save files,
;  will be no ObsInputs structure. In that case 
;  don't bother with the overwrite (only becomes an issue when changing line
;  if creating a new save set from the line command based on an old one, anyway).

        if var_type(ObsInputs) eq 8 then begin
	 ObsInputs.Line=ObsPramsStruct.LineName
 	 ObsInputs.Instrument=ObsPramsStruct.Instrument
 	 ObsInputs.DoNoiseInputs=ObsPramsStruct.NoisePrams
 	 ObsInputs.SeeCompInputs=ObsPramsStruct.FCompPrams
	 ObsInputs.RotAz=ObsPramsStruct.RotAz
	endif

;
; similarly,
; make sure GridInputs xxmin,xxmax,yymin,yymax,ngrid,ngy are also updated to match
; GridPramsStruct since these may change in for_getgrid
; UNLESS overwritten in command line and a savemap used

        if var_type(GridInputs) eq 8 then begin
	 if keyword_set(xxmin) eq 0 then xxminuse=GridPramsStruct.xrange[0] else xxminuse=xxmin         
	 if keyword_set(xxmax) eq 0 then xxmaxuse=GridPramsStruct.xrange[1] else xxmaxuse=xxmax        
	 if keyword_set(yymin) eq 0 then yyminuse=GridPramsStruct.yrange[0] else yyminuse=yymin         
	 if keyword_set(yymax) eq 0 then yymaxuse=GridPramsStruct.yrange[1] else yymaxuse=yymax        
    	 if string(GridInputs.xxmin) ne string(xxminuse) $
          then begin
           print,'xxmin changed to ',xxminuse
           GridInputs.xxmin=xxminuse
         endif
  	 if string(GridInputs.xxmax) ne string(xxmaxuse)  $
          then begin
           print,'xxmax changed to ',xxmaxuse
           GridInputs.xxmax=xxmaxuse
         endif
  	 if string(GridInputs.yymin) ne string(yyminuse) $
          then begin
           print,'yymin changed to ',yyminuse
           GridInputs.yymin=yyminuse
         endif
  	 if string(GridInputs.yymax) ne string(yymaxuse) $
          then begin
           print,'yymax changed to ',yymaxuse
           GridInputs.yymax=yymaxuse
         endif
        endif

        xxr=xxmaxuse-xxminuse
        yyr=yymaxuse-yymaxuse
        if keyword_set(ngrid) eq 0 then nxuse=fix(round(xxr/GridPramsStruct.dx)) else nxuse=ngrid
        if keyword_set(ngy) eq 0 then nyuse=fix(round(yyr/GridPramsStruct.dy)) else nyuse=ngy
  	if string(GridInputs.ngrid) ne string(nxuse) $
         then begin
           print,'ngrid changed to ',nxuse
           GridInputs.ngrid=nxuse
        endif
  	if string(GridInputs.ngy) ne string(nyuse) $
         then begin
           print,'ngy changed to ',nyuse
           GridInputs.ngy=nyuse
        endif
;
;  Save QuantMap and StokesStruct and WLSTruct structure: (empty if not comp/wrat)

         save,QuantMap,StokesStruct,WLStruct,ModPramsStruct,GridPramsStruct,ObsPramsStruct,$
		LosPramsStruct,ModSolStruct,GridInputs,LosInputs,ModelInputs,ObsInputs,PlotSave,filename = savename+'.sav'
         cd,current=thedirectory
         print,'saved file '+savename+'.sav from directory '+thedirectory
endif

;
; remove file created by for_specdefaults
;

file_delete,'IONEQ',/quiet
  
end
