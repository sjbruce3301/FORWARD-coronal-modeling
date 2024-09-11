PRO for_widget_calendar_event,ev10
  
;
; responds to changes to Calendar widget
;
; Calls  FOR_VIEWFROMDATA, FOR_MODELDEFAULTS, FOR_WIDGET_PLOT, FOR_WIDGETDISPLAY
;
; Written by Sarah Gibson, Blake Forland 2013-2014
; Version 2.0 July 2014
;
; August 2020 -- Added hooks for TOMO and STRIA
; January 2022 -- ran check for date change, and made sure it passed through
;			also ran for_widget_top to update DATE in widget if this happens
; Jun 2022 -- got rid of redundant widgets
; June 2023 -- added SYNCOM hooks
; Sept 2023 -- added AWSOM hooks


  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops
 
  if strupcase(tag_names(ev10,/structure_name)) eq 'WIDGET_KILL_REQUEST' then begin
        d=DIALOG(/WARNING,'Please use Close button to close')

  endif else begin

   widget_control, ev10.id, get_uvalue=uvalue,get_value=value
  
   if strupcase(uvalue) eq 'DATE' then begin
     
     flag.date = value

   endif

   if strupcase(uvalue) eq 'CHANGEDATE' and flag.date ne '' then begin
     
        settings.date=anytim(flag.date,/ccsds)
        widget_control,widgets.date, /DESTROY
     
        widgets.date = ''

	outops.mapname='defaults'
;
; a change in date should necessarily turn off READMAP
;

	settings.readmap=''

;
; if PFSS or PSIMAS or AWSOM --this new date should change the PFSSFILE or CUBENAME to match
; also TOMO and STRIA and SYNCOM
; so need to remove whatever PFSSFILE or CUBENAME was in MODOPS.MODELSTRING
; also TOMOFILE
; also STRIAFILE
; also SYNCOMFILE
; and get new data
;  and make sure any date correction (e.g. PFSSMOD) propagates
;

        if strupcase(modops.model) eq 'PFSSMOD' or strupcase(modops.model) eq 'PSIMAS' or strupcase(modops.model) eq 'AWSOM' or strupcase(modops.model) eq 'TOMO' or strupcase(modops.model) eq 'STRIA' or strupcase(modops.model) eq 'SYNCOM' then begin
            usedate=settings.date
	    if strupcase(modops.model) eq 'PSIMAS' or strupcase(modops.model) eq 'AWSOM' then begin
             variables.cubename=''
             nreinit=1
             for_widget_loading,3
	     z=execute("for_modeldefaults,'"+modops.model+"'"+modops.modelstring+",date=usedate,working_dir='"+settings.working_dir+"',nreinit=nreinit,ModelInputs=variables")
             settings.nreinit=nreinit
            endif else begin
	      if strupcase(modops.model) eq 'STRIA' then variables.striafile=''
	      if strupcase(modops.model) eq 'SYNCOM' then variables.syncomfile=''
	      if strupcase(modops.model) eq 'TOMO' then variables.tomofile=''
	      if strupcase(modops.model) eq 'PFSSMOD' then variables.pfssfile=''
	      z=execute("for_modeldefaults,'"+modops.model+"'"+modops.modelstring+",date=usedate,working_dir='"+settings.working_dir+"',ModelInputs=variables")
            endelse
            if settings.date ne usedate then settings.date=usedate
	    for_widget_model
        endif

;
; need to recalculate viewing position
;

	z=execute("for_viewfromdata,instrument='"+obsops.instrument+"',date='"+settings.date+"',cmer=cmer,bang=bang")

	if exist(bang) then begin
		losops.bang=bang
		losops.cmer=cmer
	endif

;
; get rid of old widgets
;
        disbsave=widgets.disb
        widget_control,widgets.top,/DESTROY
        loadsave=widgets.load
        widgets={top:'',modb:'',plotb:'',setsb:'',disb:'',outb:'',date:'',help:'',load:loadsave}
        if outops.moreplots eq 1 or outops.noerase eq 1 then widgets.disb=disbsave else if disbsave ne '' then widget_control,disbsave,/DESTROY
        for_widget_top
        if strupcase(modops.model) ne 'DATA' then for_widget_model
        for_widget_plot
        if outops.moreplots eq 0 then for_widget_display

	if strupcase(modops.model) eq 'PSIMAS' or strupcase(modops.model) eq 'AWSOM' then begin
          widget_control,widgets.load, /DESTROY
          widgets.load=''
        endif
   endif
  
   if strupcase(uvalue) eq 'CLOSE' then begin
     
     widget_control,widgets.date, /DESTROY
     
     widgets.date = ''

   endif

  endelse

END
