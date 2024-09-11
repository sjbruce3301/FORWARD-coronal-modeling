PRO for_widget_settings_event,ev2

;+
;  Name:  FOR_WIDGET_SETTINGS_EVENT
;
;  This program makes changes when buttons pushed on settings menu
;
;  External calls FOR_WIDGET_LOADING, FOR_WIDGET, FOR_WIDGET_MODEL
;		FOR_WIDGET_DISPLAY, FOR_WIDGET_PRINTCOMMAND
;
; Written by Sarah Gibson, Blake Forland 2013-2014
; Version 2.0 July 2014
;
; Sept 2020 -- added hooks for CROISSANT and TOMO model
; Jan 2021 -- added hooks for STRIA
; Feb 2022 -- passed through BG_INPUT
; Mar 2022 -- added hooks for TURBHY
; June 2023 -- added SYNCOM hooks
; July 2024 -- added CIRTOY hooks
;

  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops

if strupcase(tag_names(ev2,/structure_name)) eq 'WIDGET_KILL_REQUEST' then begin
        d=DIALOG(/WARNING,'Please use Close button to close') 

endif else begin

   widget_control, ev2.id, get_uvalue=uvalue,get_value=value

   if strupcase(uvalue) eq 'CLOSE' then begin
     
     widget_control,widgets.setsb,/DESTROY
     
     widgets.setsb = ''
     
   endif else begin
  
     for_widget_build,settings,changevalue=ev2

     if strpos(strupcase(value),'READPRAMS') ge 0 then begin
	   if settings.readprams ne '' and strpos(strupcase(settings.readprams),'GEN') gt 0 then begin
	    modelold=modops.model
	    restgen,temptemp,file=settings.readprams
	    if tag_exist(temptemp,'name') then modelname=temptemp.name $
;
; deal with old parameter files which may not have a model name tag
; note -- adaptcube was not around then, so it is ok to choose numcube based on cuberot
;
	    else begin
	   		if tag_exist(temptemp,'depbase') then modelname='cavmorph'
	   		if tag_exist(temptemp,'cuberot') then modelname='numcube'
	   		if tag_exist(temptemp,'frontheight') then modelname='liteslow'
	   		if tag_exist(temptemp,'cme_distance') then modelname='croissant'
	   		if tag_exist(temptemp,'wwidth') then modelname='cirtoy'
	   		if tag_exist(temptemp,'ht_asympt_fast') then modelname='tomo'
	   		if tag_exist(temptemp,'syncom_dens_scale') then modelname='syncom'
	   		if tag_exist(temptemp,'stria_blend') then modelname='stria'
	   		if tag_exist(temptemp,'turb_alpha') then modelname='turbhy'
	   		if tag_exist(temptemp,'thetopen') then modelname='mydipole'
	   		if tag_exist(temptemp,'bubbleonly') and tag_exist(temptemp,'frontheight') then modelname='giblow'
			if tag_exist(temptemp,'C_open') then modelname='gibbaglow'
	   		if tag_exist(temptemp,'bxex') then modelname='lowhund'
	   		if tag_exist(temptemp,'rtop') then modelname='pfssmod'
	    endelse

	    if modelname ne modelold then begin
;
;  if model has changed, do fuller reset 
;
	     	modops.model=modelname
	     	flag.reset = 3
            endif  else  flag.reset = 2
	    for_widget
           endif else d = dialog('Not a valid parameter file',/warning)
;
; may as well turn it off at this point -- its information has been preserved in
; the Strings and Inarrays
;
	   settings.readprams=''
     endif

     if strpos(strupcase(value),'READMAP') ge 0 then begin
	 if settings.readmap ne '' and strpos(strupcase(settings.readmap),'.SAV') gt 0 then begin
	   flag.reset=4
           if outops.moreplots eq 1 then flag.rmp=2 else flag.rmp=1
           for_widget
           flag.rmp=0
           for_widget_display
	   for_widget_print_command,0,printresult=printresult
           z = execute(printresult+$
                        ",ModPramsStruct=ModPramsStruct"+$
                     ",GridPramsStruct=GridPramsStruct,ObsPramsStruct=ObsPramsStruct,LosPramsStruct=LosPramsStruct,QuantMap=QuantMap"+$
                     ",ModSolStruct=ModSolStruct,StokesStruct=StokesStruct")
           modops_last = modops
           settings_last = settings
           ModelInputs = variables
           GridInputs = gridops
           LosInputs = losops
           ObsInputs = obsops
           save,QuantMap,StokesStruct,ModPramsStruct,GridPramsStruct,ObsPramsStruct,$
              LosPramsStruct,ModSolStruct,GridInputs,LosInputs,ModelInputs,$
              ObsInputs,modops_last,settings_last,filename = 'widget_temp.sav'

         endif else d = dialog('Not a valid map file',/warning)
	 settings.readmap=''
     endif

     if strpos(strupcase(value),'USEDFILE') ge 0 then begin
	 if settings.usedfile ne '' and strpos(strupcase(settings.usedfile),'.F') gt 0 then begin
	   flag.reset=5
           for_widget
;
; its information has been preserved in
; the Strings and Inarrays
; but actually it gets turned off in for_widget
;
         endif else d = dialog('Not a valid fits file',/warning)
;
; may as well turn it off at this point -- its information has been preserved in
; the Strings and Inarrays
;
	   settings.usedfile=''
     endif

     if strpos(strupcase(value),'WORKING_DIR') ge 0 $
      or strpos(strupcase(value),'BG_INPUT') ge 0 $
          then for_widget_settings
  
   endelse

endelse
  
END
