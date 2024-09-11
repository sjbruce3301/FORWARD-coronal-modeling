PRO for_widget_plot_event,ev3

;+
;  Name:  FOR_WIDGET_EVENT
;
;  This program controls the buttons and input fields the plot widget 
;
;  INPUTS
;
;  EV3 - This structure contains the values that are input into the
;       for_widget_plot.
;
;  External calls: FOR_WIDGET_BUILD, FOR_WIDGET_PLOT, FOR_WIDGET_MODEL, FOR_WIDGET_DISPLAY
;	FOR_GRIDDEFAULTS, FOR_LOSDEFAULTS, FOR_PLOTDEFULTS, FOR_OBSDEFAULTS;
;
; Written by Sarah Gibson, Blake Forland 2013-2014
; Version 2.0 July 2014
;	Feb 2017 -- added check for POP2ON, replot of widget top down
;	July 2018 -- updated UV defaults for usecolor, imin,imax
;	June 2019 -- used slash for PC compatibility
;       April 2020 -- checked for change in losuse -- if so set losmin='NULL'
;               in order to force a reset of losmin, losint, nlos in for_losinputs
;       August 2020 -- changed default filter for KCOR to gamma_filter
;	August 2020 -- uncommented "for_widget_display" when toggling between PLANEOFSKY and CARRMAP
;	January 2021 -- added filter option rpower_filter for STRIA
;	2020/2021-- for CARRMAP toggle in DATA, zeroed out settings.date
;	 (to allow access of default which may be differ than for POS)
;	September 2021 -- added obsrerun check -- forces another obsdefaults
;	    run (after the first one needed to set up some variables for other defaults runs)
;	    either because gridtype changed or because resolution
;		put in check for change in azequi
;	October 2021 --
;		changed xx/yy min/max if azequi turned on
;		changed coronagraph default filter to no_filter
;	December 2021 -- passing through distobs
;	January 2022 -- passing azequi through for_losdefaults
;	February 2022 -- changed reset xxmin etc to 4 for TOMO
;	March 2022 -- added filter option rpower_filter for TURBHY
;			added strupcase in conditional for los
;			allowed azequi pos
;	April 2022 -- added check for LOSUSE change before first run of for_losdefaults
;		added pass through of azequi and distobs to for_plotdefaults
;	October 2022-- added another check for POS/LOSUSE changes to avoid
;		recalculation of CoMP variables
;	November 2022 -- forced defaults in plotdefault after azequi change
;	December 2022 -- added NEVIII
;	Feb 2022 -- changed instr to instrument in call to for_plotdefaults
;       June 2023 -- added SYNCOM hooks
;       Feb 2024 -- added MGIX
;	    commented out date reset for carrmap DATA 
;

  
  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops
  
 flag.widgregen = 0

 if strupcase(tag_names(ev3,/structure_name)) eq 'WIDGET_KILL_REQUEST' then begin

  d=DIALOG(/WARNING,'Widgets should be closed using top widget Quit button')

 endif else begin

  slash=path_sep()

  if tag_exist(ev3,'VALUE') eq 0 then begin
        widget_control,ev3.id,get_uvalue=uvalue
  endif else begin
    result = strsplit(ev3.value,'.',/EXTRACT)
    uvalue=result[0]
    value=result[1]
  endelse

  if exist(uvalue) then if strupcase(uvalue) eq 'HELP' then begin        
         filename=file_dirname(GET_ENVIRON('FORWARD'))+slash+file_basename(GET_ENVIRON('FORWARD'))+slash+'GUI'+slash+'PLOT'+slash+'optionshelp.txt'
        xdisplayfile,filename
  endif


;
; don't do anything if just changing tab or clicking on widget box
;  or starting a change by inputting a '-'
;

  if tag_exist(ev3,'TAB') eq 0 then begin

   dostuff=0
   if tag_exist(ev3,'type') then begin
     if ev3.type eq 2 then dostuff=1
   endif
   if tag_exist(ev3,'LENGTH') eq 0 then dostuff=1
   if tag_exist(ev3,'VALUE') eq 0 then widget_control,ev3.id,get_uvalue=uvalue,get_value=value
   if value eq '-' or value eq '-.' or value eq '.' then dostuff = 0

   if dostuff eq 1 then begin
    magmoduse=flag.magmod
    modeluse=modops.model

    noiseold=obsops.donoiseinputs.donoise
    rotazold=obsops.rotaz
    usecolorold=plotops.usecolor
    rfilterold=obsops.rfilter
    pop2onold=obsops.pop2on
    posold=obsops.pos
    for_widget_build,obsops,changevalue=ev3

    if obsops.donoiseinputs.donoise ne noiseold then begin
     if obsops.donoiseinputs.donoise eq 1 then flag.noise=1 
    endif 
;
; it is now possible to have POS and AZEQUI
;	 (POS=ThomsonSphere)
;
;    if obsops.pos ne posold then begin
;     if obsops.pos eq 1 and gridops.azequi eq 1 then begin
;           gridops.azequi = 0
;           if strupcase(modeluse) ne 'TOMO' then begin
;            gridops.xxmin=-1.5 
;            gridops.xxmax=1.5 
;            gridops.yymin=-1.5 
;            gridops.yymax=1.5 
;	   endif else begin
;            gridops.xxmin=-4.0 
;            gridops.xxmax=4.0 
;            gridops.yymin=-4.0 
;            gridops.yymax=4.0 
;	   endelse
;     endif
;    endif
;
; note this will not change resolution
; and note it will not affect any changes dependent on change in gridtype -- but
;  for_obsdefaults is rerun later if needed
;
    c = execute("for_obsdefaults,magmoduse,modeluse,working_dir=settings.working_dir,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,obsinputs=obsops"+strings.obsstring)

    gridold = gridops.gridtype
    azold = gridops.azequi
    ngridold=gridops.ngrid
    ngyold=gridops.ngy
    xxminold=gridops.xxmin
    xxmaxold=gridops.xxmax
    yyminold=gridops.yymin
    yymaxold=gridops.yymax
    if strupcase(gridops.gridtype) eq 'CARRMAP' and strupcase(modeluse) eq 'DATA' then begin
         gridops.ngridval='nodisplay'
    endif else begin
         gridops.ngridval='integer'
    endelse
    for_widget_build,gridops,changevalue=ev3
    c = execute("for_griddefaults,modeluse,obsops.pos,obsops.instrument,obsops.line,gridinputs=gridops"+strings.gridstring)
    if strupcase(gridops.azequi) ne strupcase(azold) then begin
     if gridops.azequi eq 1 then begin
           gridops.xxmin=-45. 
           gridops.xxmax=45. 
           gridops.yymin=-45. 
           gridops.yymax=45. 
     endif else begin
           if strupcase(modeluse) ne 'TOMO' then begin
            gridops.xxmin=-1.5 
            gridops.xxmax=1.5 
            gridops.yymin=-1.5 
            gridops.yymax=1.5 
	   endif else begin
            gridops.xxmin=-4.0 
            gridops.xxmax=4.0 
            gridops.yymin=-4.0 
            gridops.yymax=4.0 
	   endelse
     endelse
    endif

    posnewold=obsops.pos
    losuseold=losops.losuse
    for_widget_build,losops,changevalue=ev3
;
;    look for initial change in losuse
;
    if strpos(strupcase(string(uvalue)),'LOSUSE') ge 0 then losops.losmin='NULL'

    if obsops.pos ne posold then flag.obs=3 

    c = execute("for_losdefaults,modeluse,gridops.gridtype,gridops.rheight,gridops.limb,obsops.instrument,obsops.line,obsops.pos,gridops.azequi,losinputs=losops"+strings.losstring)
;
; check for additional change in losuse or azequi 
;
    if strupcase(string(losuseold)) ne strupcase(string(losops.losuse)) or strupcase(gridops.azequi) ne strupcase(azold) then begin
      losops.losmin = 'NULL'
      c = execute("for_losdefaults,modeluse,gridops.gridtype,gridops.rheight,gridops.limb,obsops.instrument,obsops.line,obsops.pos,gridops.azequi,losinputs=losops"+strings.losstring)
    endif

;
; check for change in pos (for example, 1-->2 if azequi set)
;
; (these next two lines weren't doing anything -- changed posold to posnewold below
;  and also did a check earlier for change and flag.obs=3 allowing reset of losusee
;  in for_losdefaults
;    if strupcase(string(posnewold)) ne strupcase(string(obsops.pos)) then begin
;    endif

    unitsold=plotops.units
    plotlogold=plotops.plotlog
    for_widget_build,plotops,changevalue=ev3
    if plotops.units ne unitsold or plotops.plotlog ne plotlogold or obsops.pos ne posnewold then flag.obs=3 

    if obsops.rotaz ne rotazold and strpos(strupcase(obsops.line),'AZ') ge 0 then begin
       if strupcase(obsops.rotaz) eq 'RADIAL' then begin 
	 if (strpos(strupcase(obsops.instrument),'OVI') ge 0 $
	  or strpos(strupcase(obsops.instrument),'NEVIII') ge 0 $
	  or strpos(strupcase(obsops.instrument),'MGIX') ge 0 $
          or strupcase(obsops.instrument) eq 'LYA') then begin
          plotops.usecolor=6
          plotops.imin=0.
          plotops.imax=180.
	 endif else begin
          plotops.usecolor=66 
          plotops.imin=-90.
          plotops.imax=90.
  	 endelse
       endif else begin
         plotops.usecolor=4
         plotops.imin=0.
         plotops.imax=180.
       endelse
    endif

    if obsops.pop2on ne pop2onold then begin
;
; get rid of old widgets
;
      disbsave=widgets.disb
      widget_control,widgets.top,/DESTROY
      loadsave=widgets.load
      widgets={top:'',modb:'',plotb:'',setsb:'',disb:'',outb:'',date:'',help:'',load:loadsave}
      if outops.moreplots eq 1 or outops.noerase eq 1 then widgets.disb=disbsave else if disbsave ne '' then widget_control,disbsave,/DESTROY
      for_widget_top
      if strupcase(modeluse) ne 'DATA' then for_widget_model
      for_widget_plot
      if outops.moreplots eq 0 then for_widget_display
    endif
    if plotops.usecolor ne usecolorold then begin
     if strupcase(obsops.rotaz) eq 'RADIAL' then begin
;
; this allows switching back and forth between old
;	radial AZ table 6  and the now standard table 66 representation
;
       if plotops.usecolor eq 6 then begin
         plotops.imin=0
         plotops.imax=180
       endif
       if usecolorold eq 6 then begin
         plotops.imin=-90
         plotops.imax=90
       endif
     endif 
    endif

    obsrerun=0
    if strupcase(gridops.gridtype) ne strupcase(gridold) then begin 
        if strupcase(gridops.gridtype) eq 'PLANEOFSKY' then begin
 	 if strupcase(obsops.instrument) eq 'AIA' and strupcase(modeluse) eq 'DATA' then obsops.rfilter='aia_rfilter'
 	 if strupcase(obsops.instrument) eq 'SWAP' or strpos(strupcase(obsops.instrument),'EUVI') ge 0 then obsops.rfilter='quarterpower_rfilter'
 	 if strupcase(obsops.instrument) eq 'KCOR' then obsops.rfilter='gamma_filter'
 	 if strupcase(modeluse) eq 'STRIA' then obsops.rfilter='rpower_filter'
 	 if strupcase(modeluse) eq 'SYNCOM' then obsops.rfilter='rpower_filter'
 	 if strupcase(modeluse) eq 'TURBHY' then obsops.rfilter='rpower_filter'
 	 if strpos(strupcase(obsops.instrument),'OMP') ge 0 or strupcase(obsops.instrument) eq 'CORMAG' or strpos(strupcase(obsops.instrument),'OMP') ge 0 or strpos(strupcase(obsops.instrument),'OVI') ge 0 $
	   or strpos(strupcase(obsops.instrument),'NEVIII') ge 0 or strpos(strupcase(obsops.instrument),'MGIX') ge 0 $
           or strupcase(obsops.instrument) eq 'LYA' or strupcase(obsops.instrument) eq 'WL' then obsops.rfilter='no_filter'
;
	endif
        if strupcase(gridops.gridtype) eq 'CARRMAP' then begin
         obsops.rfilter='no_filter'
	endif
        obsrerun=1
    endif
    if obsops.rfilter ne rfilterold then begin
      flag.obs=2
      flag.rfil=0
      if strupcase(obsops.rfilter) eq 'AIA_RFILTER' then flag.rfil=1
      if strupcase(obsops.rfilter) eq 'NRGF_FILTER' then flag.rfil=2
      if strupcase(obsops.rfilter) eq 'QUARTERPOWER_FILTER' then flag.rfil=3
      if strupcase(obsops.rfilter) eq 'GAMMA_FILTER' then flag.rfil=4
      if strupcase(obsops.rfilter) eq 'RPOWER_FILTER' then flag.rfil=5
    endif
    if strupcase(gridops.azequi) ne strupcase(azold) then flag.obs=2
    c = execute("for_plotdefaults,magmoduse,modops.model,plotinputs=plotops,gridtype=gridops.gridtype,azequi=gridops.azequi,distobs=gridops.distobs,dodisk=losops.dodisk,noerase=outops.noerase,instrument=obsops.instrument,line=obsops.line,donoise=obsops.DoNoiseInputs.DoNoise,pos=obsops.Pos"+strings.plotstring)
    flag.obs=0

;
; if ngrid/ngy change, recalculate noise resolution
; (unless its a change of GRIDTYPE -- which we test by change to 'NULL' for xxmin) 
;
    if is_number(xxminold) and is_number(gridops.xxmin) then begin
     if ngridold ne gridops.ngrid or ngyold ne gridops.ngy or xxminold ne gridops.xxmin or xxmaxold ne gridops.xxmax or yyminold ne gridops.yymin or yymaxold ne gridops.yymax then begin
;     print,ngridold,gridops.ngrid
;     print,ngyold,gridops.ngy
;     print,xxminold,gridops.xxmin
;     print,xxmaxold,gridops.xxmax
;     print,yyminold,gridops.yymin
;     print,yymaxold,gridops.yymax
;
; force overwrite of resolution
;
       obsops.donoiseinputs.resolution=0
       obsrerun=1
     endif
    endif

    if obsrerun eq 1 then c = execute("for_obsdefaults,magmoduse,modeluse,working_dir=settings.working_dir,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,ngrid=gridops.ngrid,ngy=gridops.ngy,xxmin=gridops.xxmin,xxmax=gridops.xxmax,yymin=gridops.yymin,yymax=gridops.yymax,limb=gridops.limb,obsinputs=obsops"+strings.obsstring)

;
; if toggle or array or occult/dodisk or ngrid/ngy or rotaz change, regenerate plot widget
;

    if flag.widgregen eq 1 then for_widget_plot

;
; if change gridtype regenerate display and top (e.g. refresh outputs)
;
    if strupcase(gridops.gridtype) ne strupcase(gridold) $
	or strupcase(gridops.azequi) ne strupcase(azold) $
       then begin 
        outops.noerase=0
        if strupcase(gridops.gridtype) eq 'PLANEOFSKY' then begin
	 plotops.nwinx=750
	 plotops.nwiny=750
         plotops.charsize=1.2
        endif
        if strupcase(gridops.gridtype) eq 'CARRMAP' then begin
	 plotops.nwinx=720
	 plotops.nwiny=360
         plotops.charsize=1.2
;         if strupcase(modeluse) eq 'DATA' then settings.date=''
        endif
;
; this next line was commented out, not sure why. I have uncommented.
	for_widget_display
    endif

    flag.widgregen = 0
            
   endif
  endif else begin
;
; note at the moment changetabs does nothing
; I created it when resolution was displayed as a parameter
; in the noise widget, but it isnt anymore.
; In general, flag.changetabs will force a regeneration of 
; a plot tab - otherwise, changing tabs does not force
;  this. It may come in handy, so I will keep it, but
; as of now, the conditional that follows should never be true.
;
   if flag.changetabs eq 1 then begin
    for_widget_plot
    flag.changetabs=0
   endif
  endelse
 endelse 

END
