PRO for_widget_event,ev
  
;+
;  Name:  FOR_WIDGET_EVENT
;
;  Runs whenever something is changed in the Top MENU Bar widget
;
;  This program controls the buttons and input fields of the main menu bar of the FORWARD code widget. 
;
;  INPUTS
;
;  EV - This structure contains the values that are input into the
;       for_widget menu bar (i.e., button pushed by user)
;
;  OUTPUTS
;
;  	Save set with last FORWARD call
;
;  EXTERNAL CALLS
;
;    In GUI directory
;  	FOR_WIDGET_LOADING, FOR_WIDGET_PLOT, FOR_WIDGET_DISPLAY, 
;	FOR_WIDGET
;	FOR_WIDGET_OUTPUT, FOR_WIDGET_SETTINGS, FOR_WIDGET_CALENDAR
;	FOR_WIDGET_PRINT_COMMAND
;  
;    In other FORWARD directories
;  	FOR_DRIVE, FOR_PLOTDEFAULTS, FOR_LOSDEFAULTS
;  
; Written by Blake Forland, Sarah Gibson 2013-2014
; Version 2.0 July 2014
;	Bug fixes -- unnecessary reruns avoided
;-
;	March 2016 -- expanded FITS list to allow FITS.GZ etc.
;	March 2018 -- saved mapname for DATA Recalculate
;	June 2018 -- changed naming convention for some CoMP lines
;		also benergy, etc
; 	June 2019 -- used slash and file_delete for PC compatibility
;		(some heritage usewindows for safety)
;	Sept 2020 -- added hooks for CROISSANT and TOMO  and STRIA model
;	Feb 2021 -- changed so if moreplots sticks to x/y grid, even for CROISSANT, TOMO, etc.
;	2020/2021-- checked for gridops xxmin/max yymin/max if number before running difference
;	Sept 2021 -- changed so redomap not set for Gridtype Data change
;		  also made exclusion to the Recalculate ngrid/ngy be CARRMAP
;			  because widget CARRMAP will never zoom in by changing
;			  XXMIN/MAX, YYMIN/MAX
;		     added explanatory comments about gridtypeuse
;	          also fixed rounding bug that was changing ngrid unnecessarily and made sure not long (fix(round)
;		  and put in checks for CARRMAP/DATA in gridtest for NGRID and NGY (for all CARRMAP)
;                 made adjustments to account for magmod=-1, -2 for DATA
; 		  and changed "if ne CARRMAP" to "if eq PLANEOFSKY" because there should be no USER here
;		  and put in exception for change of LOSOFFSET from number to 'NULL' which happens for CARRMAP
;			for rerunning
;		  and adjusted time saver on EXPFAC so not for moreplots or noerase (because otherwise might plot things at different CMAP heights)
;		and pass through of azequi
;	Dec 2021 -- passing through distobs
;	Jan 2022 -- passed azequi through for_losdefaults
;	Feb 2022 -- changed xxmin etc reset to 4 for TOMO
;	Mar 2022 -- added hooks for TURBHY
;			changed xoffset --> losoffset
;			got rid of turn off azequi if POS
;	Apr 2022 -- passed azequi and distobs through for_plotdefaults
;	May 2022- strupcase check throughout
;	Oct 2022 -- removed forcing of flag.reset = 6 for model
;			and unset griddops.distobs when rflag.reset set
;	Nov 2022 -- put forcing back on because it was screwing up the reset
;		also added limb keyword to obsdefaults calls
;		 where resolution impacted and updated a couple
;		 of those calls to include grid info
;      Dec 2022 - added NEVIII
;			fixed ge bug in test for coming from polarimeter to PLANEOFSKY
;	Feb 2023 -- changed instr to instrument in for_plotdefaults
;		also changed test of difference in xxmin, xxmax etc to 1d-7 not 1d-8
;       June 2023 -- added SYNCOM hook -- but it is actually commented out
;	August 2023 -- added IONDENS
;	Sept 2023 -- added AWSOM
;	Jan 2024 -- updated UCoMP
;       Feb 2024 -- added MGIX
;		-- added change  for CoMP/UCoMP to not break elsewhwere
;       Jun 2024 -- added BPOS column variables
;	Jul 2024 -- added hooks for ULIMB
;	Aug 2024 -- added WLRAT hooks
;

  
  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops

;
; test for PC
;

  usewindows=0
  if strupcase(!version.os_family) eq 'WINDOWS' then usewindows=1

  slash=path_sep()
  
  if strupcase(tag_names(ev,/structure_name)) eq 'WIDGET_KILL_REQUEST' then begin
	d=DIALOG(/WARNING,'Please use Quit button to close') 

  endif else begin

   if tag_exist(ev,'VALUE') eq 0 then begin
     widget_control, ev.id, get_uvalue=uvalue,get_value=value
     result=[uvalue,value]
   endif else begin
    result = strsplit(ev.value,'.',/EXTRACT)
   endelse

   if result[0] eq 'FORWARD' or result[0] eq 'SAVE' or result[0] eq 'Recalculate' then begin

     if result[0] eq 'SAVE' then begin
       outops.savemap = 1
       outops.extratitle = string(systime(/julian),FORMAT='(f18.7)')
     endif
     if result[0] eq 'Recalculate' and strupcase(modops.model) ne 'DATA' and $
	strupcase(gridops.gridtype) eq 'PLANEOFSKY' then begin
       if is_number(gridops.ngrid) then gridops.ngrid=-1
       if is_number(gridops.ngy) then gridops.ngy=-1
;
; this basically forces a reset to defaults of ngrid and ngy
; because later in this program, ngrid and ngy are changed if e.g. there is a zoom-in
; done in order to allow zoom in on structure without changing resolution
; and so resetting during a Recalculate effectively increases resolution of zoom in
;
; should not be done for CARRMAP because it isn't possible (with the widget) to zoom in by
;  changing XX/YYRANGE for a CARRMAP (stays 0-180, 0-360 lat/long), rather it is done via NGRID
; and should not be done for DATA because it messes up bookkeeping below
;  (basically, ngrid defines the resolution of a save set of all the data)
;   (also ngrid can't change resolution of CARRMAP at the moment)
;
       c = execute("for_griddefaults,modops.model,obsops.Pos,obsops.instrument,obsops.line,gridinputs=gridops"+strings.gridstring)
     endif

     if widgets.outb ne '' then begin
      widget_control,widgets.outb,/DESTROY
      widgets.outb = ''
     endif
     if widgets.setsb ne '' then begin
      widget_control,widgets.setsb,/DESTROY
      widgets.setsb = ''
     endif
     if widgets.date ne '' then begin
      widget_control,widgets.date,/DESTROY
      widgets.date = ''
     endif
        
;
; check for asymmetric integration
;

     d = 'Yes'

     if is_number(losops.nlos) then begin
      if (abs((losops.nlos-1.d0)*losops.losint + 2.d0*losops.losmin)) gt 0.001d0 then d=dialog(/QUESTION,'Warning! This integration wont be symmetric about the plane of sky.  If you want it symmetric, set LOSINT to zero and it will be recalculated. Do you want to continue with the asymmetric integration?')
     endif

     if d eq 'Yes' then begin

         redomap=0
         
;
; note - settings.readmap should never be set, so here is a check
;

         if settings.readmap ne '' then print,'(debug) settings.readmap should not be set?'

         if file_exist('widget_temp.sav') eq 1 and settings.readmap eq '' and result[0] ne 'Recalculate' then begin
            
            restore,'widget_temp.sav'
            
            modtest = compare_struct(modops,modops_last)
            
            vartest = compare_struct(variables,ModelInputs)

 	    gridtest=0
 	    gridcheck=1
	    if n_tags(gridops) ne n_tags(GridInputs) then begin
		gridtest=1 
	    endif else begin
	     tnames=strupcase(tag_names(gridops))
	     t2names=strupcase(tag_names(GridInputs))
	     for i = 0,n_tags(gridops)-1 do begin
	      if tnames[i] ne t2names[i] then begin
		gridtest = 1 
	      endif else begin
	       if strpos(strupcase(tnames[i]),'VAL') lt 0 then begin	
		z=execute("tagcontent1 = gridops."+tnames[i])
		z=execute("tagcontent2 = GridInputs."+t2names[i])
		if strupcase(string(tagcontent1)) ne strupcase(string(tagcontent2)) then begin
                  if strupcase(tnames[i]) ne 'LOSOFFSET' and strupcase(tnames[i]) ne 'XXMIN' and strupcase(tnames[i]) ne 'XXMAX' and strupcase(tnames[i]) ne 'YYMIN' and strupcase(tnames[i]) ne 'YYMAX' and strupcase(tnames[i]) ne 'LIMB' and strupcase(tnames[i]) ne 'NGRID' and strupcase(tnames[i]) ne 'NGY' and strupcase(tnames[i]) ne 'GRIDTYPE' then gridtest=1
		  if strupcase(tnames[i]) eq 'LOSOFFSET' and is_number(tagcontent1) and is_number(tagcontent2) then if tagcontent1 ne tagcontent2 then gridtest=1
		  if strupcase(tnames[i]) eq 'XXMIN' and is_number(tagcontent1) and is_number(tagcontent2) then if tagcontent1 lt tagcontent2 then gridtest=1
		  if strupcase(tnames[i]) eq 'YYMIN' and is_number(tagcontent1) and is_number(tagcontent2) then if tagcontent1 lt tagcontent2 then gridtest=1
		  if strupcase(tnames[i]) eq 'XXMAX' and is_number(tagcontent1) and is_number(tagcontent2) then if tagcontent1 gt tagcontent2 then gridtest=1
		  if strupcase(tnames[i]) eq 'YYMAX' and is_number(tagcontent1) and is_number(tagcontent2) then if tagcontent1 gt tagcontent2 then gridtest=1
	          if strupcase(tnames[i]) eq 'NGRID' then begin
;
; be careful with CARRMAP DATA - at the moment, NGRID changing has no effect, so don't see it as a change
;    might impact NGY -- which also should do nothing for CARRMAP (model and data) and shouldn't change
;
		    if strupcase(gridops.gridtype) eq 'CARRMAP' and strupcase(modops.model) eq 'DATA' then begin
                    endif else begin
	             if is_number(gridops.ngrid) and is_number(gridops.xxmax) and is_number(gridops.xxmin) and is_number(gridpramsstruct.dx) then begin
	 		if abs(gridops.ngrid - fix(round((double(gridops.xxmax)-double(gridops.xxmin))/gridpramsstruct.dx))) gt 1d-7  then gridtest=1
		     endif else gridtest=1
	            endelse
		  endif
	          if strupcase(tnames[i]) eq 'NGY' then begin
		    if strupcase(gridops.gridtype) eq 'CARRMAP' then begin
                    endif else begin
	             if is_number(gridops.ngy) and is_number(gridops.yymax) and is_number(gridops.yymin) and is_number(gridpramsstruct.dy) then begin
	 		if abs(gridops.ngy - fix(round((double(gridops.yymax)-double(gridops.yymin))/gridpramsstruct.dy))) gt 1d-7  then gridtest=1
		     endif else gridtest=1
	            endelse
		  endif
;
; only reason tagcontent1/2 wont be a number is if GRIDTYPE or LIMB has changed
;	(or model to data -- will shift xxmin/max to 'NULL' at least temporarily)
;
;
;  we don't want to trigger redomap just for gridtype or limb change, so created a check
;    (in case there is a save set already there)
;
		  if strupcase(tnames[i]) eq 'LIMB' $
		    and strupcase(gridops.gridtype) eq 'CARRMAP' then begin
	              gridtest=1
		      gridcheck=0
		  endif
		  if strupcase(tnames[i]) eq 'GRIDTYPE' then begin
	           gridtest=1
		   gridcheck=0
		  endif
	        endif
	       endif
	      endelse
	     endfor
	    endelse

	    if gridtest eq 1 and gridcheck eq 1 and strupcase(modops.model) eq 'DATA' then redomap=1

;print,'gridtest=',gridtest
;print,'gridcheck=',gridcheck
;print,'gridops losoffset=',gridops.losoffset
;print,'gridinputs losoffset=',gridinputs.losoffset
            
 	    lostest=0
	    if tag_exist(LosInputs,'incres') eq 0 then begin
                  LosInputs=add_tag(LosInputs,1,'IncRes',index=19)
                  LosInputs=add_tag(LosInputs,'nodisplay','IncResVal',index=20)
	    endif
	    if n_tags(losops) ne n_tags(LosInputs) then begin
             lostest=1 
	    endif else begin
	     tnames=strupcase(tag_names(losops))
	     t2names=strupcase(tag_names(LosInputs))
	     for i = 0,n_tags(losops)-1 do begin
	      if tnames[i] ne t2names[i] then begin
		lostest = 1 
	      endif else begin
	       if strpos(strupcase(tnames[i]),'VAL') lt 0 then begin	
		z=execute("tagcontent1 = losops."+tnames[i])
		z=execute("tagcontent2 = LosInputs."+t2names[i])
		if strupcase(string(tagcontent1)) ne strupcase(string(tagcontent2)) then begin
		  if strpos(strupcase(tnames[i]),'CCULT') gt 0 or strupcase(tnames[i]) eq 'DODISK' then begin
		   if is_number(tagcontent1) and is_number(tagcontent2) then begin
		    if tagcontent2 gt 0 then begin
		      if strupcase(tnames[i]) eq 'UPOCCULT' and strupcase(modops.model) ne 'DATA' then if tagcontent2 lt tagcontent1 then lostest = 1 
		      if strupcase(tnames[i]) eq 'OCCULT' and strupcase(modops.model) ne 'DATA' then if tagcontent2 gt tagcontent1 then lostest = 1 
		    endif
		    if strupcase(tnames[i]) eq 'DODISK' and strupcase(modops.model) ne 'DATA' then if tagcontent1 ne 0. then lostest = 1 
	           endif else lostest = 1
	          endif else lostest = 1
		endif
	       endif
	      endelse
	     endfor
	    endelse
                
;
; the only thing we have to worry about changing in settings
; is MEMORY - if DATE changes, it will result in changes to other
; tags of LOSTEST that will trigger a recalculation; 
; (although I suppose it is remotely possible it could be exactly
; the same CMER and BANG! - so should check it to be sure)
; if READPRAMS or USEDFILE or WORKING_DIR or READMAP are changed
; their information is passed through and
; they get turned off in for_settings_widget_event
; and should not show up here anyway as different.
; MAY NOT BE ROBUST if something new is added to Settings.
; 

	    settest=0
	    if settings.memory ne settings_last.memory then settest=1
	    if settings.date ne settings_last.date then settest=1

            obstest = compare_struct(obsops,ObsInputs)
            
            obsnodif = 1

            if obstest[0].field ne '' then begin
               
                for k = 0, n_elements(obstest) - 1 do begin
                  
                  obsnodifupdate = 1
                  stringdif = obstest[k].field
                  
                  if stringdif eq '.LINE' then begin
 			testcomp1 = strpos(strupcase(obsops.instrument),'OMP')+strpos(strupcase(obsops.instrument),'CORMAG')+1
 			testcomp2 = strpos(strupcase(obsinputs.instrument),'OMP')+strpos(strupcase(obsinputs.instrument),'CORMAG')+1
			if testcomp1 ge 0 and testcomp2 ge 0 and strupcase(obsops.instrument) eq strupcase(obsinputs.instrument) then begin
			    if strpos(strupcase(obsops.line),'DOPPLERVLOS') ge 0 and obsinputs.SeeCompInputs.IWLine ne 1 then obsnodifupdate=0
			endif else begin
			    if strupcase(obsops.instrument) eq strupcase(obsinputs.instrument) and (strupcase(obsops.instrument) eq 'RADIO' or strupcase(obsops.instrument) eq 'FARADAY' or strpos(strupcase(obsops.instrument),'OVI') ge 0 $
    or strpos(strupcase(obsops.instrument),'NEVIII') ge 0 or strpos(strupcase(obsops.instrument),'MGIX') ge 0 $
    or strupcase(obsops.instrument) eq 'LYA' or strupcase(obsops.instrument) eq 'PHYSICAL DIAGNOSTICS') then begin
                                if strupcase(obsops.line) eq 'NINST' or strupcase(obsinputs.line) eq 'NINST' then obsnodifupdate = 0
			    endif else obsnodifupdate=0
			endelse
		  endif else begin
;
; MAY NOT BE ROBUST - if new things added to ObsInputs
;
			 if strpos(strupcase(stringdif),'DONOISE') ge 0 or strpos(strupcase(stringdif),'ROTAZ') ge 0 or strpos(strupcase(stringdif),'SEESPECINPUTS.SEESPEC') ge 0 or strpos(strupcase(stringdif),'POP2ON') ge 0 or strpos(strupcase(stringdif),'SEECOMPINPUTS.SEECOMP') ge 0 or strpos(strupcase(stringdif),'SEECOMPINPUTS.IWLINEVAL') ge 0 or strpos(strupcase(stringdif),'RFILTER') ge 0 then begin
			   if stringdif eq '.RFILTER' and strupcase(modops.model) eq 'DATA' and (strupcase(obsops.rfilter) eq 'AIA_RFILTER' or strupcase(obsinputs.rfilter) eq 'AIA_RFILTER') then obsnodifupdate=0 
			   if strpos(strupcase(stringdif),'POP2ON') ge 0 then obsnodifupdate=0
			 endif else begin
; 
; this is for backward compatibility
                              if stringdif ne '.ALL_TYPES' and stringdif ne '.ALL_INST' and stringdif ne '.ALL_NAMES' and stringdif ne '.PHYS_PARAMS' and strpos(strupcase(stringdif),'VAL') lt 0 then obsnodifupdate = 0
                         endelse

		  endelse

;                  if obsnodifupdate eq 0 then print,stringdif,obsnodifupdate,obsnodif
		  obsnodif = obsnodifupdate*obsnodif
                     
                endfor
	    endif

            if modtest[0].field eq '' and gridtest eq 0 and lostest eq 0 and settest eq 0 and vartest[0].field eq '' and obsnodif eq 1 then settings.readmap = 'widget_temp.sav' else settings.readmap = ''

;	    if settings.readmap ne 'widget_temp.sav' then print,modtest[0].field,gridtest,lostest,settest,vartest[0].field,obsnodif
                  
         endif 

         dontrun=0
	 if strupcase(modops.model) eq 'DATA' and (settings.readmap eq '' or redomap eq 1 or result[0] eq 'Recalculate') then begin
		mapnamesave=outops.mapname
;
; this allows for example passing in a ngrid much too big and then for_plotfits
; will figure out the native highest resolution and pass back an ngrid which will then be adopted
;   redomap should be set if that happens, regardless of whether recalculate is set
;
;  BUT we don't want it to happen for CARRMAP DATA because CARRMAP DATA does not use ngrid
;  and will end up with unnecessary regeneration of files
;
; will also adjust date to the nearest data image in some circumstances
;
		ngriduse=gridops.ngrid
		gridtypeuse=gridops.gridtype
;	        print,'in for_widget_event, before recalc data ngrid=',gridops.ngrid
		dateuse=settings.date
        	if result[0] eq 'Recalculate' then z = execute("for_plotfits,instrument=obsops.instrument,working_dir=settings.working_dir,/redomap,ngrid=ngriduse,gridtype=gridtypeuse,limb=gridops.limb,rheight=gridops.rheight,xxmin=gridops.xxmin,xxmax=gridops.xxmax,yymin=gridops.yymin,yymax=gridops.yymax,line=obsops.line,/noplots,/savemap,mapname=mapname,occult=losops.occult,upoccult=losops.upoccult,date=dateuse,rfilter=obsops.rfilter,GridPramsStruct=GridPramsStruct") else $
        	z = execute("for_plotfits,instrument=obsops.instrument,working_dir=settings.working_dir,redomap=redomap,ngrid=ngriduse,gridtype=gridtypeuse,limb=gridops.limb,rheight=gridops.rheight,xxmin=gridops.xxmin,xxmax=gridops.xxmax,yymin=gridops.yymin,yymax=gridops.yymax,line=obsops.line,/noplots,/savemap,mapname=mapname,occult=losops.occult,upoccult=losops.upoccult,date=dateuse,rfilter=obsops.rfilter,GridPramsStruct=GridPramsStruct")
;
; only do if Plane of sky
;  not CARRMAP
;
	        if strupcase(gridtypeuse) eq 'PLANEOFSKY' then gridops.ngrid=ngriduse
 		gridops.gridtype=gridtypeuse
;	        print,'in for_widget_event, revise after recalc data ngrid=',gridops.ngrid
		settings.date=dateuse
        	if exist(mapname) then if mapname ne '' then readmap=mapname+'.sav' 
  	 	if exist(readmap) eq 0 then readmap = ''
                if readmap ne '' then begin
        	 if settings.working_dir ne '' then begin
                   readmap = settings.working_dir+slash+readmap 
        	 endif
        	 settings.readmap=readmap
        	 flag.reset=7
        	 for_widget
	        endif else begin
	          print,'sorry, problem with fits file access'
	          dontrun=1
	        endelse
		outops.mapname=mapnamesave
      	 endif

         if dontrun eq 0 then begin
	  if outops.psplot eq 1 then begin
           outops.eps=1
	    moreplotsave=outops.moreplots
	    outops.moreplots=0
	  endif
          for_widget_plot
          for_widget_display
          for_widget_print_command,0,printresult=printresult
	  Case 1 of
           strupcase(modops.model) eq 'DATA': for_widget_loading,2
           else:for_widget_loading,1 
          endcase

          if outops.psplot eq 1 then begin
	  	z = execute(printresult+$
			",ModPramsStruct=ModPramsStruct"+$
                     ",GridPramsStruct=GridPramsStruct,ObsPramsStruct=ObsPramsStruct,LosPramsStruct=LosPramsStruct,QuantMap=QuantMap"+$
                     ",ModSolStruct=ModSolStruct,StokesStruct=StokesStruct")
  	        outops.psplot=0
		outops.eps=0
		outops.moreplots=moreplotsave
                for_widget_print_command,0,printresult=printresult
	  endif

          z = execute(printresult+$
			",ModPramsStruct=ModPramsStruct"+$
                     ",GridPramsStruct=GridPramsStruct,ObsPramsStruct=ObsPramsStruct,LosPramsStruct=LosPramsStruct,QuantMap=QuantMap"+$
                     ",ModSolStruct=ModSolStruct,StokesStruct=StokesStruct")
;
; safer to reinit after each run
;
          settings.reinit=1
	  if settings.readmap eq '' or strupcase(modops.model) eq 'DATA' then begin
           GridInputs = gridops
           LosInputs = losops
           ObsInputs = obsops
          endif 

          modops_last = modops
          ModelInputs = variables
          settings.readmap = ''
          outops.savemap=0
	  outops.saveprams=''
          outops.extratitle=''
          if tag_exist(outops,'JPEG') then outops.jpeg=0
          if tag_exist(outops,'GIF') then outops.gif=0
          if tag_exist(outops,'TIFF') then outops.tiff=0
          settings_last = settings

;          print,'in for_widget_event, after for_drive, ngrid=',gridops.ngrid,' ngy=',gridops.ngy
 
;
; warning - if plot is being made of subregion of map,
; which e.g. when only xxmin/xxmax, yymin/yymax are changed
; and calculation is not redone, gridops.ngrid may differ
; from the number of points (in the x direction) on the plot.
; In such cases it is important to have widget change to indicate
; actual ngrid used, and also the save set to be consistent in
; gridinputs vs gridpramsstruct.
;
; this should not happen in CARRMAP because not allowed to change xxmin/max yymin/max
; forcing 2 to 1 ratio and 360 degrees longitude, 180 latitude
;

	  if is_number(gridops.xxmax) then gridops.ngrid = fix(round((double(gridops.xxmax)-double(gridops.xxmin))/gridpramsstruct.dx))
	  if is_number(gridops.yymax) then gridops.ngy = fix(round((double(gridops.yymax)-double(gridops.yymin))/gridpramsstruct.dy))
;	  print,'in for_widget_event, after tweak, ngrid=',gridops.ngrid,' ngy=',gridops.ngy
          for_widget_plot
          gridinputs.ngrid=gridops.ngrid
          gridinputs.ngy=gridops.ngy
	  gridinputs.xxmax=gridpramsstruct.xrange[1]
	  gridinputs.xxmin=gridpramsstruct.xrange[0]
	  gridinputs.yymax=gridpramsstruct.yrange[1]
	  gridinputs.yymin=gridpramsstruct.yrange[0]
          if is_number(gridinputs.xxmin) then gridinputs.xxmin=double(gridinputs.xxmin)
          if is_number(gridinputs.xxmax) then gridinputs.xxmax=double(gridinputs.xxmax)
          if is_number(gridinputs.yymin) then gridinputs.yymin=double(gridinputs.yymin)
          if is_number(gridinputs.yymax) then gridinputs.yymax=double(gridinputs.yymax)

;
; sometimes this introduces a teeny difference between gridops and
; gridinputs that forces an unnecessary recalculation
;
          if is_number(gridops.xxmax) and is_number(gridops.xxmin) then if abs(double(gridops.xxmax)-double(gridinputs.xxmax)) lt 1d-7 then gridops.xxmax=gridinputs.xxmax
          if is_number(gridops.xxmax) and is_number(gridops.xxmin) then if abs(double(gridops.xxmin)-double(gridinputs.xxmin)) lt 1d-7 then gridops.xxmin=gridinputs.xxmin
          if is_number(gridops.yymax) and is_number(gridops.yymin) then if abs(double(gridops.yymax)-double(gridinputs.yymax)) lt 1d-7 then gridops.yymax=gridinputs.yymax
          if is_number(gridops.yymax) and is_number(gridops.yymin) then if abs(double(gridops.yymin)-double(gridinputs.yymin)) lt 1d-7 then gridops.yymin=gridinputs.yymin

;          print,gridops.xxmin,gridinputs.xxmin,gridops.xxmax,gridinputs.xxmax,gridops.yymin,gridinputs.yymin,gridops.yymax,gridinputs.yymax


; check to see if pop2tregime was changed
; (unless data)

	  if strupcase(modops.model) ne 'DATA' then begin
           if ObsPramsStruct.Pop2TRegime ne obsops.Pop2TRegime then obsops.Pop2TRegime=ObsPramsStruct.Pop2TRegime
           if ObsPramsStruct.Pop2TRegime ne ObsInputs.Pop2TRegime then ObsInputs.Pop2TRegime=ObsPramsStruct.Pop2TRegime
          endif

          save,QuantMap,StokesStruct,ModPramsStruct,GridPramsStruct,ObsPramsStruct,$
              LosPramsStruct,ModSolStruct,GridInputs,LosInputs,ModelInputs,$
              ObsInputs,modops_last,settings_last,filename = 'widget_temp.sav'

          widget_control,widgets.load, /DESTROY
	  widgets.load=''
         endif
         if exist(readmap) then if strpos(strupcase(readmap),'SUB') gt 0 then begin
            file_delete, readmap, /quiet
         endif
    endif
   endif
  
   if result[0] eq 'Observables' then begin
     if outops.moreplots eq 1 then flag.obs=2 else flag.obs=1
     
;
; reset reinit in case need to read lookup tables
;
     settings.reinit = 1 

     if strupcase(modops.model) eq 'DATA' then begin
	d=dialog(/warning,'new observable will require a model; choosing GIBLOW')
	modops.model='GIBLOW'
	flag.magmod= 1
	flag.reset = 1
 	gridops.distobs='215.'
	if outops.moreplots eq 1 then flag.reset = 6 
	if outops.noerase eq 1 then flag.reset = 6
	for_widget
        if outops.moreplots eq 1 then flag.mdl=2 else flag.mdl=1
     endif

     instold = obsops.instrument 
     lineold = obsops.line
     obsops.instrument = strupcase(result[2])

     if n_elements(result) eq 4 then begin
;
; I dont think this will ever be false?
;
        
        obsops.line = strupcase(result[3])
        
     endif else begin

        obsops.line = 'NONE'

     endelse

     if strpos(strupcase(result[1]),'LINE-OF-SIGHT INTEGRATED DIAGNOSTICS') ge 0 then begin
        if strupcase(obsops.instrument) eq 'COLUMN_MAGNETIC_ENERGY_DENSITY' then obsops.instrument='BENERGY'
        if strupcase(obsops.instrument) eq 'COLUMN_MAGNETIC_ENERGY_DENSITY_TIMES_DENSITY' then obsops.instrument='BEN_DENS_INT'
        if strupcase(obsops.instrument) eq 'COLUMN_MAGNETIC_MAGNITUDE' then obsops.instrument='B_INT'
        if strupcase(obsops.instrument) eq 'COLUMN_MAGNETIC_MAGNITUDE_TIMES_DENSITY' then obsops.instrument='B_DENS_INT'
        if strupcase(obsops.instrument) eq 'COLUMN_MAGNETIC_POS_MAGNITUDE' then obsops.instrument='B_POS_INT'
        if strupcase(obsops.instrument) eq 'COLUMN_MAGNETIC_POS_MAGNITUDE_TIMES_DENSITY' then obsops.instrument='B_POS_DENS_INT'
     endif
	
     if strpos(strupcase(result[1]),'CORONAL POLARIMETER') ge 0 $
       or strpos(strupcase(result[1]),'UV SPECTROPOLARIMETER') ge 0 then begin
       if strpos(strupcase(result[1]),'CORONAL POLARIMETER') ge 0 then begin
          if strupcase(obsops.instrument) eq 'FE11_7892'  then obsops.instrument='FE11COMP'
          if strupcase(obsops.instrument) eq 'FE13_10747' then obsops.instrument='COMP'
          if strupcase(obsops.instrument) eq 'FE13_10798' then obsops.instrument='OTHERCOMP'
          if strupcase(obsops.instrument) eq 'FE14_5303'  then obsops.instrument='CORMAG'
          if strupcase(obsops.instrument) eq 'SI9_39267'  then obsops.instrument='SI9COMP'
          if strupcase(obsops.instrument) eq 'SI10_14302' then obsops.instrument='SI10COMP'
       endif
;print,obsops.instrument
;
; take care of case where line has changed but not instrument, so dont do full observational
; reset of CoMP defaults which would force a rerun.
;  But if instrument has changed, need to force a rerun because of changes to e.g. wavelength range.
;   even if MOREPLOTS is set
; 
       if strupcase(obsops.instrument) eq strupcase(instold) then flag.obs=2 else flag.obs=1
     endif else begin
;
; if only change is to instrument, not line, dont set FLAG.OBS
; but be careful! a change from comp to cormag for example 
; requires a reset of wavelengths, so thats why I only do this
; in this else statement
;
       if obsops.line eq lineold then flag.obs=0
     endelse
;
; reset frequencies etc if going or coming from RADIO or FARADAY or EIS or CDS or IRIS or MYSPECT or IONDENS
;

     if strupcase(obsops.instrument) eq 'RADIO' or strupcase(instold) eq 'RADIO' or $
	strupcase(obsops.instrument) eq 'FARADAY' or strupcase(instold) eq 'FARADAY' or $
	strupcase(obsops.instrument) eq 'EIS' or strupcase(instold) eq 'EIS' or $
	strupcase(obsops.instrument) eq 'CDS' or strupcase(obsops.instrument) eq 'IRIS' $
	or strupcase(instold) eq 'CDS' or strupcase(instold) eq 'IRIS' or $
	strupcase(obsops.instrument) eq 'MYSPECT' or strupcase(obsops.instrument) eq 'IONDENS' or strupcase(instold) eq 'MYSPECT' or strupcase(instold) eq 'IONDENS' then begin

           if strupcase(obsops.instrument) ne strupcase(instold) then begin
            obsops.frequency_MHz=0.d0
            obsops.dogyro=0.d0
            obsops.fcor=0.d0
            obsops.ulimb=0.63d0
            obsops.numion=0.d0
            obsops.wavelength_Ang=0.d0
            obsops.wavelength2_Ang=0.d0
	    obsops.SeeSpecInputs.LWidth=0.d0
           endif
;
; we only want to reset EIS/CDS/IRIS/MYSPECT/IONDENS, not RADIO or FARADAY, if line changes
;
           if strupcase(obsops.line) ne strupcase(lineold) then begin
             obsops.numion=0.d0
             obsops.wavelength_Ang=0.d0
             obsops.wavelength2_Ang=0.d0
	     obsops.SeeSpecInputs.LWidth=0.d0
           endif
     endif
;
; special arrangements if changing to/from coronal polarimeter or uv spectropolarimeter:
;  don't reset POS or IONEQ if changing between like-type polarimeters 
;  do reset them  if changing to/from polarimeter
;  also different defaults for NGRID,NGY,RESOLUTION IF planeofsky COMP/UV
;  because run time is long

     comptest1=strpos(strupcase(obsops.instrument),'OMP')+strpos(strupcase(obsops.instrument),'CORMAG')+1
     comptest2=strpos(strupcase(instold),'OMP')+strpos(strupcase(instold),'CORMAG')+1
     uvtest1=strpos(strupcase(obsops.instrument),'LYA')+$
       strpos(strupcase(obsops.instrument),'NEVIII')+$
       strpos(strupcase(obsops.instrument),'MGIX')+$
       strpos(strupcase(obsops.instrument),'OVI')+2
     uvtest2=strpos(strupcase(instold),'LYA')+$
       strpos(strupcase(instold),'NEVIII')+$
       strpos(strupcase(instold),'MGIX')+$
       strpos(strupcase(instold),'OVI')+2

     if instold ne obsops.instrument then begin
      if is_number(obsops.Pos) then begin
       obsopschange=0
;
; will be reset to 0 if not change between like polarimeters
; otherwise will keep whatever value it has
;
       if comptest1 ge 0 and comptest2 ge 0 then obsopschange = obsops.Pos 
       if uvtest1 ge 0 and uvtest2 ge 0 then obsopschange = obsops.Pos 
       obsops.Pos = obsopschange
      endif
;
; if changing between like polarimeters or non-polarimeters
;
      if (comptest1 ge 0 and comptest2 ge 0) or $
	(uvtest1 ge 0 and uvtest2 ge 0) or $
	(comptest1 lt 0 and comptest2 lt 0 and uvtest1 lt 0 and uvtest2 lt 0) then begin
        c = execute("for_obsdefaults,flag.magmod,modops.model,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,working_dir=settings.working_dir,obsinputs=obsops"+strings.obsstring)
      endif else begin 
;
; if changing  to/from polarimeters
;
	obsops.seespecinputs.ioneq=''
        if strupcase(gridops.gridtype) eq 'PLANEOFSKY' then begin 
;
; if initiating UV or COMP
;
         if (comptest2 lt 0 and comptest1 ge 0) or (uvtest2 lt 0 and uvtest1 ge 0) then begin 
          obsops.pos=1
          gridops.ngrid = 64
          gridops.ngy =64 
          obsops.donoiseinputs.resolution=0
          c = execute("for_obsdefaults,flag.magmod,modops.model,working_dir=settings.working_dir,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,ngrid=gridops.ngrid,ngy=gridops.ngy,xxmin=gridops.xxmin,xxmax=gridops.xxmax,yymin=gridops.yymin,yymax=gridops.yymax,limb=gridops.limb,obsinputs=obsops"+strings.obsstring)
         endif else begin
          obsops.pos=0
          gridops.ngrid = 256
          gridops.ngy = 256
          obsops.donoiseinputs.resolution=0
          c = execute("for_obsdefaults,flag.magmod,modops.model,working_dir=settings.working_dir,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,ngrid=gridops.ngrid,ngy=gridops.ngy,xxmin=gridops.xxmin,xxmax=gridops.xxmax,yymin=gridops.yymin,yymax=gridops.yymax,limb=gridops.limb,obsinputs=obsops"+strings.obsstring)
         endelse
        endif else c = execute("for_obsdefaults,flag.magmod,modops.model,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,working_dir=settings.working_dir,obsinputs=obsops"+strings.obsstring)
      endelse
     endif else c = execute("for_obsdefaults,flag.magmod,modops.model,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,working_dir=settings.working_dir,obsinputs=obsops"+strings.obsstring)

     c = execute("for_griddefaults,modops.model,obsops.Pos,obsops.instrument,obsops.line,gridinputs=gridops"+strings.gridstring)

;
; run for_viewfromdata in case changed to/from STEREO view
;

     if strpos(strupcase(obsops.instrument),'EUVI') ge 0 or strpos(strupcase(obsops.instrument),'EUVI') ge 0 then begin
      for_viewfromdata,date=settings.date,cmer=cmer,bang=bang,instrument=obsops.instrument 
      if exist(cmer) then losops.cmer=cmer
      if exist(bang) then losops.bang=bang
     endif

     c = execute("for_losdefaults,modops.model,gridops.gridtype,gridops.rheight,gridops.limb,obsops.instrument,obsops.line,obsops.Pos,gridops.azequi,losinputs=losops"+strings.losstring)

;     print,'flag.obs=',flag.obs

     c = execute("for_plotdefaults,flag.magmod,modops.model,plotinputs=plotops,gridtype=gridops.gridtype,azequi=gridops.azequi,distobs=gridops.distobs,dodisk=losops.dodisk,noerase=outops.noerase,line=obsops.line,instrument=obsops.instrument,rotaz=obsops.rotaz,donoise=obsops.DoNoiseInputs.DoNoise,pos=obsops.pos"+strings.plotstring)
     flag.mdl=0
     flag.obs=0

     for_widget_plot

;     for_widget_display

     
   endif 
  
   if result[0] eq 'Physical Diagnostics' then begin
     
     if outops.moreplots eq 1 then flag.obs=2 else flag.obs=1

     if strupcase(modops.model) eq 'DATA' then begin
	d=dialog(/warning,'new observable will require a model; choosing GIBLOW')
	modops.model='GIBLOW'
	flag.magmod=1
	flag.reset = 1
 	gridops.distobs='215.'
	if outops.moreplots eq 1 then flag.reset = 6 
	if outops.noerase eq 1 then flag.reset = 6
	for_widget
        if outops.moreplots eq 1 then flag.mdl=2 else flag.mdl=1
     endif

;
; turn off azequi if widget change to Physical Diagnostics
;  and reset grid
; NO-- not need now
;

;     if gridops.azequi eq 1 then begin
;           gridops.azequi = 0
; 	   if strupcase(modops.model) ne 'TOMO' then begin
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

     lineold = obsops.line
     obsops.line = strupcase(result[1])
     
;
; don't do this if moreplots or noerase is on
; but otherwise -- reset things if switch to/from EXPFAC
; to avoid slow down
;
     if lineold ne obsops.line and (outops.moreplots ne 1 and outops.noerase ne 1) then begin
      if strupcase(obsops.line) eq 'EXPFAC' then begin
       if strupcase(gridops.gridtype) eq 'PLANEOFSKY' then begin
        gridops.ngrid=64
        gridops.ngy=64
        if gridops.xxmin eq -1.5 and gridops.xxmax eq 1.5 and gridops.yymin eq -1.5 and gridops.yymax eq 1.5 then begin
         gridops.xxmin=-2.5
         gridops.xxmax=2.5
         gridops.yymin=-2.5
         gridops.yymax=2.5
        endif
       endif else begin
        gridops.ngrid=45
        gridops.rheight=2.5
       endelse
      endif
      if lineold eq 'EXPFAC' then begin
       if strupcase(gridops.gridtype) eq 'PLANEOFSKY' then begin
        gridops.ngrid=256
        gridops.ngy=256
        if gridops.xxmin eq -2.5 and gridops.xxmax eq 2.5 and gridops.yymin eq -2.5 and gridops.yymax eq 2.5 then begin
         gridops.xxmin=-1.5
         gridops.xxmax=1.5
         gridops.yymin=-1.5
         gridops.yymax=1.5
        endif
       endif else begin
        gridops.ngrid=180
        gridops.rheight=1.05
       endelse
      endif
     endif

     instold = obsops.instrument 
     obsops.instrument =  strupcase(result[0])
     comptest2=strpos(strupcase(instold),'OMP')+strpos(strupcase(instold),'CORMAG')+strpos(strupcase(instold),'LYA')$
         +strpos(strupcase(instold),'NEVIII')$
         +strpos(strupcase(instold),'MGIX')$
         +strpos(strupcase(instold),'OVI')+4
     if comptest2 ge 0 and strupcase(gridops.gridtype) eq 'PLANEOFSKY' then begin
          gridops.ngrid = 256
          gridops.ngy =256
          obsops.donoiseinputs.resolution=0
     endif
     
     obsops.Pos = -1

;     c = execute("for_obsdefaults,flag.magmod,modops.model,working_dir=settings.working_dir,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,obsinputs=obsops"+strings.obsstring)
; changed to below since resolution will change with ngrid,ngy
;
     c = execute("for_obsdefaults,flag.magmod,modops.model,working_dir=settings.working_dir,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,ngrid=gridops.ngrid,ngy=gridops.ngy,xxmin=gridops.xxmin,xxmax=gridops.xxmax,yymin=gridops.yymin,yymax=gridops.yymax,limb=gridops.limb,obsinputs=obsops"+strings.obsstring)

     c = execute("for_griddefaults,modops.model,obsops.Pos,obsops.instrument,obsops.line,gridinputs=gridops"+strings.gridstring)

     c = execute("for_losdefaults,modops.model,gridops.gridtype,gridops.rheight,gridops.limb,obsops.instrument,obsops.line,obsops.Pos,gridops.azequi,losinputs=losops"+strings.losstring)

     c = execute("for_plotdefaults,flag.magmod,modops.model,plotinputs=plotops,gridtype=gridops.gridtype,azequi=gridops.azequi,distobs=gridops.distobs,dodisk=losops.dodisk,noerase=outops.noerase,line=obsops.line,instrument=obsops.instrument,rotaz=obsops.rotaz,donoise=obsops.DoNoiseInputs.DoNoise,pos=obsops.pos"+strings.plotstring)
     flag.mdl=0
     flag.obs=0

     for_widget_plot

;     for_widget_display

   endif 
  
   if result[0] eq 'Models' then begin

	modold=modops.model

	modops.model=strupcase(result[1])

	if strupcase(modops.model) eq 'NUMCUBE' or strupcase(modops.model) eq 'ADAPTCUBE' or strupcase(modops.model) eq 'PSIMAS' or strupcase(modops.model) eq 'AWSOM' then begin
          settings.nreinit=1
          for_widget_loading,3
        endif

;	if strupcase(modops.model) eq 'GIBBAGLOW' or strupcase(modops.model) eq 'NUMCUBE' or strupcase(modops.model) eq 'CROISSANT' or strupcase(modops.model) eq 'TOMO' or strupcase(modops.model) eq 'STRIA' or strupcase(modops.model) eq 'SYNCOM' or strupcase(modops.model) eq 'TURBHY' then begin
;         if outops.moreplots ne 1 then begin
         if outops.moreplots ne 1 and outops.noerase ne 1 then begin
          c = execute("for_griddefaults,modops.model,obsops.pos,obsops.instrument,obsops.line,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,phio=losops.phio,limb=gridops.limb,ngrid=gridops.ngrid,ngy=gridops.ngy,xxmin=xxmin,xxmax=xxmax,yymin=yymin,yymax=yymax,losoffset=gridops.losoffset,rheight=gridops.rheight")
	  gridops.xxmin=xxmin
	  gridops.xxmax=xxmax
	  gridops.yymin=yymin
	  gridops.yymax=yymax
	 endif
;    	endif

 	flag.reset=6
;   not sure this is the best way to do it- I had some trouble before where
;   it was retaining things from other models that were not appropriate
;   but thats why I had all the models reset their grids above if moreplots/noerase not set
;   could comment it out and uncomment the lines which looked for moreplots/noerase
;	if outops.moreplots eq 1 then flag.reset = 6 
;	if outops.noerase eq 1 then flag.reset = 6
;   except then will cause a problem where reset will be set to 0 and won't
;   preserve information about date that is important for example for default model
;   * so keep an eye on this *
;
;   commented below because the mmsave fix to FOR_WIDGET
;  should avoid the problem with passing through magnetic variables
;	if flag.magmod eq 1 and strupcase(modops.model) eq 'CAVMORPH' then flag.reset=1
;
        if strupcase(modold) eq 'DATA' then begin
 	  gridops.distobs='215.'
	endif

        if outops.moreplots eq 1 then flag.mdl=2 else flag.mdl=1
;        c = execute("for_obsdefaults,flag.magmod,modops.model,working_dir=settings.working_dir,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,obsinputs=obsops"+strings.obsstring)
;        c = execute("for_plotdefaults,flag.magmod,modops.model,plotinputs=plotops,gridtype=gridops.gridtype,azequi=gridops.azequi,distobs=gridops.distobs,dodisk=losops.dodisk,noerase=outops.noerase,line=obsops.line,instrument=obsops.instrument,rotaz=obsops.rotaz,donoise=obsops.DoNoiseInputs.DoNoise,pos=obsops.pos"+strings.plotstring)

	for_widget

        flag.mdl=0

	if strupcase(modops.model) eq 'NUMCUBE' or strupcase(modops.model) eq 'ADAPTCUBE' or strupcase(modops.model) eq 'PSIMAS' or strupcase(modops.model) eq 'AWSOM' then begin
          widget_control,widgets.load, /DESTROY
	  widgets.load=''
        endif
   endif

   if result[0] eq 'Data' then begin
           
          obsops.instrument  =  strupcase(result[1])
	  if obsops.instrument eq 'COMP/UCOMP' then obsops.instrument= 'CoMP'

; testing CARRMAP for KCOR
;             When DATA is chosen either by date or name, gridops
;	      still hasn't changed. So gridtypeuse may be changed
;             within for_plotfits -- and if so it will replace gridops.gridtype
;		for example if going from KCOR CARRMAP to AIA (which only does PLANEOFSKY)
;	      So should do whatever gets passed in if KCOR, and
;	      otherwise will revert to PLANEOFSKY as below

	  if obsops.instrument eq 'KCOR' then begin
	    gridtypeuse=strupcase(gridops.gridtype)
	  endif else begin
	   gridtypeuse='PLANEOFSKY'
;
; this next bit is because for non KCOR, DATA is always PLANEOFSKY
;	so it is resetting the widget window to a square
;	in the situation where CARRMAP is set and then DATA is chosen
;
	   if strupcase(gridops.gridtype) eq 'CARRMAP' then begin
             plotops.nwinx=750
             plotops.nwiny=750
           endif
	  endelse

          if widgets.outb ne '' then begin
           widget_control,widgets.outb,/DESTROY
           widgets.outb = ''
          endif
          if widgets.setsb ne '' then begin
           widget_control,widgets.setsb,/DESTROY
           widgets.setsb = ''
          endif
          if widgets.date ne '' then begin
           widget_control,widgets.date,/DESTROY
           widgets.date = ''
          endif

          howfind  =  strupcase(result[2])

          if outops.moreplots eq 1 then flag.dta=2 else flag.dta=1
          modold=modops.model
          modops.model='DATA'
          if strupcase(howfind) eq 'BYDATE' then begin
             if n_elements(result) gt 3 then obsops.line  =  strupcase(result[3]) else obsops.line='0'
          endif else begin
             if strpos(strupcase(obsops.instrument),'COMP') ge 0 or strupcase(obsops.instrument) eq 'CORMAG' then obsops.line  =  strupcase(result[3])
          endelse

; DATA gets magmod=-1 (magnetically dependent observable), -2 (non-magnetically dependent observable)
;
          if strpos(strupcase(obsops.instrument),'COMP') ge 0 or strupcase(obsops.instrument) eq 'CORMAG' then $ 
          flag.magmod = -1 else flag.magmod=-2
 
          dontrun=0
          for_widget_loading,2

          c = execute("for_obsdefaults,flag.magmod,'DATA',working_dir=settings.working_dir,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridtypeuse,obsinputs=obsops"+strings.obsstring)

          c = execute("for_losdefaults,'DATA',gridtypeuse,'NULL','NULL',obsops.instrument,obsops.line,'NULL','NULL',losinputs=losops"+strings.losstring)

	  readmap=''

          if strupcase(gridtypeuse) eq 'PLANEOFSKY' then gridops.ngrid=512 else gridops.ngrid=180
          gridops.ngy=0

	  if strupcase(howfind) eq 'BYDATE' then begin

             if strpos(strupcase(obsops.instrument),'COMP') ge 0 then wlcomp  =  strupcase(result[4])
;
; note right now there is no BYDATE access for CORMAG, but when there is can be done like COMP
;
             if strupcase(obsops.instrument) eq 'CORMAG' then wlcomp  =  strupcase(result[4])

;	     print,'in for_widget_event, before get data ngrid=',gridops.ngrid
;	     print,'in for_widget_event, before get data ngy=',gridops.ngy
;
; note the use of gridtypeuse within for_plotfits below is needed for example if go from KCOR CARRMAP to AIA
;  will want to change to PLANEOFSKY
;
	     ngriduse=gridops.ngrid
             dateuse=settings.date
             if outops.moreplots eq 1 then z = execute("for_plotfits,instrument=obsops.instrument,working_dir=settings.working_dir,line=obsops.line,/noplots,/savemap,mapname=mapname,occult=losops.occult,upoccult=losops.upoccult,date=dateuse,ngrid=ngriduse,gridtype=gridtypeuse,limb=gridops.limb,rheight=gridops.rheight,xxmin=gridops.xxmin,xxmax=gridops.xxmax,yymin=gridops.yymin,yymax=gridops.yymax,wlcomp=wlcomp,rfilter=obsops.rfilter") else $
            	z = execute("for_plotfits,instrument=obsops.instrument,working_dir=settings.working_dir,line=obsops.line,/noplots,/savemap,mapname=mapname,occult=losops.occult,upoccult=losops.upoccult,ngrid=ngriduse,gridtype=gridtypeuse,limb=gridops.limb,rheight=gridops.rheight,date=dateuse,wlcomp=wlcomp,rfilter=obsops.rfilter")
	     if strupcase(gridtypeuse) eq 'PLANEOFSKY' then gridops.ngrid=ngriduse
	     gridops.gridtype=gridtypeuse
             settings.date=dateuse

	  endif else begin
	     if usewindows eq 0 then fitspick=['*'+strlowcase(obsops.instrument)+'*.fts*,*'+strupcase(obsops.instrument)+'*.fts*,*'+strlowcase(obsops.instrument)+'*.fits*,*'+strupcase(obsops.instrument)+'*.fits*'] else $
	        fitspick=[obsops.instrument+'*.F*TS*']
             if strpos(strupcase(obsops.instrument),'EUVIA') ge 0 then if usewindows eq 0 then fitspick=['*euA*.fts*,*EUA*.fts*,*EUVIA*.fts*,*euA*.fits*,*EUA*.fits*,*EUVIA*.fits*'] else fitspick=['EU*A*.F*TS*']
             if strpos(strupcase(obsops.instrument),'EUVIB') ge 0 then if usewindows eq 0 then fitspick=['*euB*.fts*,*EUB*.fts*,*EUVIB*.fts*,*euB*.fits*,*EUB*.fits*,*EUVIB*.fits*'] else fitspick=['EU*B*.F*TS*']
	     fitsfile=dialog_pickfile(path=settings.working_dir,filter=fitspick)
	     if (strpos(strupcase(fitsfile),'FITS') lt 0 and strpos(strupcase(fitsfile),'FTS') lt 0) then mapname='' else begin
	       ngriduse=gridops.ngrid
;	       print,'in for_widget_event, before get data ngrid=',gridops.ngrid
;	       print,'in for_widget_event, before get data ngy=',gridops.ngy
               dateuse=settings.date
               if outops.moreplots eq 1 then z = execute("for_plotfits,instrument=obsops.instrument,filename=fitsfile,date=dateuse,working_dir=settings.working_dir,line=obsops.line,ngrid=ngriduse,gridtype=gridtypeuse,limb=gridops.limb,rheight=gridops.rheight,xxmin=gridops.xxmin,xxmax=gridops.xxmax,yymin=gridops.yymin,yymax=gridops.yymax,/noplots,/savemap,mapname=mapname,occult=losops.occult,upoccult=losops.upoccult,wlcomp=wlcomp,rfilter=obsops.rfilter") else $
               z = execute("for_plotfits,instrument=obsops.instrument,filename=fitsfile,working_dir=settings.working_dir,date=dateuse,/noplots,/savemap,mapname=mapname,ngrid=ngriduse,gridtype=gridtypeuse,limb=gridops.limb,rheight=gridops.rheight,line=obsops.line,occult=losops.occult,upoccult=losops.upoccult,wlcomp=wlcomp,rfilter=obsops.rfilter") 
;
; only do if Plane of sky
;  not CARRMAP
;
	       if strupcase(gridtypeuse) eq 'PLANEOFSKY' then gridops.ngrid=ngriduse
;
; if gridtype has changed, rerun the defaults
;
	       if strupcase(gridops.gridtype) ne strupcase(gridtypeuse) then begin 
                    c = execute("for_obsdefaults,flag.magmod,'DATA',working_dir=settings.working_dir,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridtypeuse,obsinputs=obsops"+strings.obsstring)
                    c = execute("for_losdefaults,'DATA',gridtypeuse,'NULL','NULL',obsops.instrument,obsops.line,'NULL','NULL',losinputs=losops"+strings.losstring)
	            gridops.gridtype=gridtypeuse
               endif
               settings.date=dateuse
	     endelse
	  endelse

	  if exist(mapname) then if mapname ne '' then readmap=mapname+'.sav'

          c = execute("for_plotdefaults,flag.magmod,'DATA',plotinputs=plotops,gridtype=gridtypeuse,azequi=gridops.azequi,distobs=gridops.distobs,dodisk=losops.dodisk,noerase=outops.noerase,line=obsops.line,instrument=obsops.instrument,rotaz=obsops.rotaz,donoise=obsops.DoNoiseInputs.DoNoise,pos=obsops.pos"+strings.plotstring)
          flag.dta=0
 
          widget_control,widgets.load, /DESTROY
	  widgets.load=''

          if readmap ne '' then begin
             if settings.working_dir ne '' then begin
               readmap=settings.working_dir+slash+readmap
             endif

 	     settings.readmap=readmap
	     flag.reset=7
	     for_widget
	  endif else begin
	     print,'sorry, problem with fits file access'
             settings.readmap=''
	     dontrun=1
	  endelse

          if dontrun eq 0 then begin
           for_widget_plot
           for_widget_display
           for_widget_print_command,0,printresult=printresult
	   z = execute(printresult+$
			",ModPramsStruct=ModPramsStruct"+$
                     ",GridPramsStruct=GridPramsStruct,ObsPramsStruct=ObsPramsStruct,LosPramsStruct=LosPramsStruct,QuantMap=QuantMap"+$
                     ",ModSolStruct=ModSolStruct,StokesStruct=StokesStruct")
           settings.readmap = ''
           outops.savemap=0
           if tag_exist(outops,'JPEG') then outops.jpeg=0
           if tag_exist(outops,'GIF') then outops.gif=0
           if tag_exist(outops,'TIFF') then outops.tiff=0
           modops_last = modops
           settings_last = settings
           ModelInputs = variables
           GridInputs = gridops
           LosInputs = losops
           ObsInputs = obsops

;	   print,'in for_widget_event, after get data ngrid=',gridops.ngrid,' ngy=',gridops.ngy
           gridops.ngy=gridpramsstruct.ngy
;
; warning - if plot is being made of subregion of map,
; which e.g. when a save map for whole field of view is used so that
; calculation is not redone, gridops.ngrid may differ
; from the number of points (in the x direction) on the plot.
; In such cases it is important to have widget change to indicate
; actual ngrid used, and also the save set to be consistent in
; gridinputs vs gridpramsstruct.
;
	   if is_number(gridops.xxmax) then gridops.ngrid = fix(round((double(gridops.xxmax)-double(gridops.xxmin))/gridpramsstruct.dx))
	   if is_number(gridops.yymax) then gridops.ngy = fix(round((double(gridops.yymax)-double(gridops.yymin))/gridpramsstruct.dy))
;	   print,'in for_widget_event, after tweak, ngrid=',gridops.ngrid,' ngy=',gridops.ngy
           for_widget_plot
           gridinputs.ngrid=gridops.ngrid
           gridinputs.ngy=gridops.ngy
	   gridinputs.xxmax=gridpramsstruct.xrange[1]
	   gridinputs.xxmin=gridpramsstruct.xrange[0]
	   gridinputs.yymax=gridpramsstruct.yrange[1]
	   gridinputs.yymin=gridpramsstruct.yrange[0]
           if is_number(gridinputs.xxmin) then gridinputs.xxmin=double(gridinputs.xxmin)
           if is_number(gridinputs.xxmax) then gridinputs.xxmax=double(gridinputs.xxmax)
           if is_number(gridinputs.yymin) then gridinputs.yymin=double(gridinputs.yymin)
           if is_number(gridinputs.yymax) then gridinputs.yymax=double(gridinputs.yymax)
;
; sometimes this introduces a teeny difference between gridops and
; gridinputs that forces an unnecessary recalculation
;
           if is_number(gridops.xxmax) and is_number(gridops.xxmin) then if abs(double(gridops.xxmax)-double(gridinputs.xxmax)) lt 1d-7 then gridops.xxmax=gridinputs.xxmax
           if is_number(gridops.xxmax) and is_number(gridops.xxmin) then if abs(double(gridops.xxmin)-double(gridinputs.xxmin)) lt 1d-7 then gridops.xxmin=gridinputs.xxmin
           if is_number(gridops.yymax) and is_number(gridops.yymin) then if abs(double(gridops.yymax)-double(gridinputs.yymax)) lt 1d-7 then gridops.yymax=gridinputs.yymax
           if is_number(gridops.yymax) and is_number(gridops.yymin) then if abs(double(gridops.yymin)-double(gridinputs.yymin)) lt 1d-7 then gridops.yymin=gridinputs.yymin

;           print,gridops.xxmin,gridinputs.xxmin,gridops.xxmax,gridinputs.xxmax,gridops.yymin,gridinputs.yymin,gridops.yymax,gridinputs.yymax

           save,QuantMap,StokesStruct,ModPramsStruct,GridPramsStruct,ObsPramsStruct,$
              LosPramsStruct,ModSolStruct,GridInputs,LosInputs,ModelInputs,$
              ObsInputs,modops_last,settings_last,filename = 'widget_temp.sav'

	  endif
          if exist(readmap) then if strpos(strupcase(readmap),'SUB') gt 0 then begin
                   file_delete, readmap, /quiet
          endif
   endif

   if result[0] ne 'SAVE' and result[0] ne 'Models' and result[0] ne 'Observables' and result[0] ne 'Physical Diagnostics' and result[0] ne 'FORWARD' and result[0] ne 'Data' and result[0] ne 'Recalculate' then begin
           
           case result[0] of 
              
              'Settings' : begin
                 
                    for_widget_settings
                    
              end
              
              'Output' : begin
                 
                    for_widget_output
                    
              end
              
              'Calendar' : begin

                    for_widget_calendar

              end
              
              'Gather Windows' : begin
                 
                 if flag.tab eq 0 then begin

                ;    d = dialog(/warning,'Another way to gather windows. '+$
                ;               'OS X Users: [COMMAND] + [TAB] '+$
                ;               'Windows Users: [CTRL] + [TAB]. '+$
                ;               'Use these keyboard commands and select'+$
                ;               ' Quarts or X11.')
                    
                    flag.tab = 1
                    
                 endif
                 
                 widnames = tag_names(widgets)
                 
                 for i = 0 , n_elements(widnames) - 1 do begin 
                    
                    x = execute("if widgets."+widnames[i]+" ne '' then Widget_Control, widgets."+widnames[i]+",/Show")
                    
                 endfor   
                 
              end              

              'Print Command' : begin

		  tempnreinit=settings.nreinit
 		  settings.nreinit=1
                  if strupcase(modops.model) ne 'DATA' then for_widget_print_command,1 else print,'Sorry, cannot make print command for data'
 		  settings.nreinit=tempnreinit
 	      end

              'Reset' : begin
                 
                 flag.reset = 1
                 
                 for_widget
                 
              end
              
              'Help' : begin

                  filename=file_dirname(GET_ENVIRON('FORWARD'))+slash+file_basename(GET_ENVIRON('FORWARD'))+slash+'GUI'+slash+'TOP'+slash+'tophelp.txt'

                  xdisplayfile,filename

              end
              
              'Quit' : begin
                 
                 if file_exist('widget_temp.sav') then begin
                   file_delete, 'widget_temp.sav', /quiet
                 endif

;
;  by setting flag = 0 we will fool FOR_WIDGET into thinking it is the first
;  time if called again
;

		 undefine,flag

                 widget_control,widgets.top, /DESTROY

		 widgets.top=''

; delete any open windows

 		 while !d.window ne -1 do wdelete, !d.window
 		 widget_control,/reset
                    
              end
              
           endcase
           
   endif
  endelse
     
END
  
