PRO for_widget_model_event,ev1

;+
;  Name:  FOR_WIDGET_MODEL_EVENT
;
;  This program controls the buttons and input fields the model widget 
;
;  INPUTS
;
;  EV1 - This structure contains the values that are input into the
;       for_widget_model.
;  
; External Calls: FOR_WIDGET_BUILD, FOR_MODELDEFAULTS, FOR_VIEWFROMDATA, FOR_WIDGET_MODEL
;		   FOR_WIDGET_PLOT
;
; Written by Sarah Gibson, Blake Forland 2013-2014
; Version 2.0 July 2014
;   June 2019 -- used slash for PC compatibility
;   Sept 2019 -- made widget update with change in hydro choice
;   Sept 2021 -- adjusted widget change with hydro to deal with models without temperature explicit
;		passed through azequi
;   Dec 2021 -- passed through distobs
;   Feb 2022 -- updated resets for HYDRO in CROISSANT and SIMPLE in TOMO model
;   May 2022 -- got rid of redundant widgets
;   Nov 2022 -- added nreinit=0 for hydro for_modeldefaults
;   June 2023 -- added SYNCOM hooks
;   July 2023 -- added working_dir to call for_modeldefaults
;   Sept 2023 -- added AWSOM hooks
; 

  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops

  slash=path_sep()
  
;
;
 if strupcase(tag_names(ev1,/structure_name)) eq 'WIDGET_KILL_REQUEST' then begin

  d=DIALOG(/WARNING,'Widgets should be closed using top widget Quit button')

 endif else begin

;
; don't do anything if just clicking in box or adding negative sign
;
  dostuff=0
  if tag_exist(ev1,'type') then begin
	    if ev1.type eq 2 then dostuff=1
  endif
  if tag_exist(ev1,'LENGTH') eq 0 then dostuff=1

  if tag_exist(ev1,'VALUE') eq 0 then begin
        widget_control,ev1.id,get_uvalue=uvalue,get_value=value
  endif else begin
    result = strsplit(ev.value,'.',/EXTRACT)
    uvalue=result[0]
    value=result[1]
  endelse

  if value eq '-' or value eq '-.' or value eq '.' or value eq '' then dostuff = 0

  if strupcase(uvalue) eq 'HELP' then begin
         filename=file_dirname(GET_ENVIRON('FORWARD'))+slash+file_basename(GET_ENVIRON('FORWARD'))+slash+'MODELS'+slash+strupcase(modops.model)+slash+strupcase(modops.model+'HELP.TXT')
        xdisplayfile,filename

  endif
 
  if dostuff eq 1 then begin
   for_widget_build,variables,changevalue=ev1

; 
;  special cases of PFSSFILE and TOMOFILE and RTOP or TOPOLOGY
;  and STRIA and SYNCOM
;
   if strupcase(modops.model) eq 'PFSSMOD' or strupcase(modops.model) eq 'TOMO' or strupcase(modops.model) eq 'STRIA' or strupcase(modops.model) eq 'SYNCOM' then begin

	if strupcase(uvalue) eq 'RTOPVAL' then variables.pfssfile=''
	if strupcase(uvalue) eq 'TOPOLOGYVAL' then variables.pfssfile=''

	if strupcase(uvalue) eq 'SIMPLEVAL' or strupcase(uvalue) eq 'TOPOLOGYVAL' or strupcase(uvalue) eq 'RTOPVAL' then begin
	   z=execute("for_modeldefaults,'"+modops.model+"'"+modops.modelstring+",date=usedate,working_dir='"+settings.working_dir+"',ModelInputs=variables")
	   for_widget_model
        endif

	if strupcase(uvalue) eq 'PFSSFILEVAL' or strupcase(uvalue) eq 'TOMOFILEVAL' or strupcase(uvalue) eq 'STRIAFILEVAL' or strupcase(uvalue) eq 'SYNCOMFILEVAL' then begin
	   usedate=settings.date
	   z=execute("for_modeldefaults,'"+modops.model+"'"+modops.modelstring+",date=usedate,working_dir='"+settings.working_dir+"',ModelInputs=variables")
	   if settings.date ne usedate then settings.date=usedate

           z=execute("for_viewfromdata,instrument='"+obsops.instrument+"',date='"+settings.date+"',cmer=cmer,bang=bang")

           if exist(bang) then begin
                losops.bang=bang
                losops.cmer=cmer
           endif

           for_widget_plot

	   for_widget_model

;           for_widget_display

	endif
   endif
; 
;  special cases of NUMCUBE/ADAPTCUBE (or PSIMAS or AWSOM), variable CUBENAME
;
   if strupcase(modops.model) eq 'NUMCUBE' or strupcase(modops.model) eq 'ADAPTCUBE' or strupcase(modops.model) eq 'PSIMAS' or strupcase(modops.model) eq 'AWSOM' then begin
	if strupcase(uvalue) eq 'CUBENAMEVAL' then begin
	   nreinit=1
	   if strupcase(modops.model) eq 'NUMCUBE' or strupcase(modops.model) eq 'ADAPTCUBE' then usedate=settings.date else usedate=''
	   z=execute("for_modeldefaults,'"+modops.model+"'"+modops.modelstring+",date=usedate,working_dir='"+settings.working_dir+"',nreinit=nreinit,ModelInputs=variables")
           c = execute("for_obsdefaults,variables.magmod,modops.model,azequi=gridops.azequi,distobs=gridops.distobs,gridtype=gridops.gridtype,working_dir=settings.working_dir,obsinputs=obsops"+strings.obsstring)
	   if settings.date ne usedate then settings.date=usedate
           settings.nreinit=nreinit

           z=execute("for_viewfromdata,instrument='"+obsops.instrument+"',date='"+settings.date+"',cmer=cmer,bang=bang")

           if exist(bang) then begin
                losops.bang=bang
                losops.cmer=cmer
           endif else begin
              losops.bang=0.
              losops.cmer=-90.
           endelse

;
; get rid of old widgets
;
           disbsave=widgets.disb
           widget_control,widgets.top,/DESTROY
           loadsave=widgets.load
           widgets={top:'',modb:'',plotb:'',setsb:'',disb:'',outb:'',date:'',help:'',load:loadsave}
           if outops.moreplots eq 1 or outops.noerase eq 1 then widgets.disb=disbsave else if disbsave ne '' then widget_control,disbsave,/DESTROY
           for_widget_plot
           if strupcase(modops.model) ne 'DATA' then for_widget_model
           for_widget_top
           if outops.moreplots eq 0 then for_widget_display

        endif
	if strupcase(uvalue) eq 'TOPOLOGYVAL' then variables.cubename=''
   endif

;
; special case, eigenvalue for giblow or liteslow
;
   if strupcase(modops.model) eq 'GIBLOW' then begin
	if strupcase(uvalue) eq 'ALNOTRBUBVAL' then begin
          if variables.alnotrbub gt 3 or variables.alnotrbub lt 1 or double(value) ne double(variables.alnotrbub) then begin
		   if abs(variables.alnotrbub) gt 6 or abs(variables.alnotrbub) lt 1 then begin
    		    message,/info,'please use eigenvalues >/= 1 and </= 3 (or negative of this); resetting to 1'
    		    variables.alnotrbub=1
                   endif
	           for_widget_model
	  endif
        endif
   endif

    
;
; special case, changing some parameters having dependencies in CAVMORPH
;

   if strupcase(modops.model) eq 'CAVMORPH' then begin
	if strupcase(uvalue) eq 'THCSVAL' then begin
	  variables.cavtop_th=value
	  variables.nougtop_th=value
	  variables.nougthcs=value 
	  for_widget_model
 	endif
	if strupcase(uvalue) eq 'PHCSVAL' then begin
	  variables.nougphcs=value 
	  for_widget_model
 	endif
	if strupcase(uvalue) eq 'MANGVAL' then begin
	  variables.noug_mang=value 
	  for_widget_model
 	endif
	if strupcase(uvalue) eq 'CAVTOP_THVAL' then begin
	  variables.nougtop_th=value
	  for_widget_model
 	endif
	if strupcase(uvalue) eq 'DEPBASEVAL' then begin
	  variables.depbase=value 
	  usecavscales=variables.cavscales
	  z=execute("for_modeldefaults,'"+modops.model+"'"+modops.modelstring+",working_dir='"+settings.working_dir+"',ModelInputs=variables")
	  for_widget_model
 	endif
   endif

; change in hydro changes defaults
; commented out outer conditional -- I think this should be true for all models
;  EXCEPT LOWHUND which is different
;   if strupcase(modops.model) eq 'GIBBAGLOW' or strupcase(modops.model) eq 'MYDIPOLE' then begin
    if strupcase(modops.model) ne 'LOWHUND' then begin
	if strupcase(uvalue) eq 'HYDROVAL' then begin
	  if (strupcase(modops.model) eq 'PFSSMOD' or $
	   strupcase(modops.model) eq 'MYDIPOLE' or $
	   strupcase(modops.model) eq 'GIBBAGLOW') and (value eq 0 or value gt 5) then begin
	    if value eq 0 then print,'hydro=0 (no density) not allowed, changing to default hydro=3 (Vasquez)' $
	 	else print,'hydro must be between 1 and 5, changing to default hydro=3 (Vasquez)'
	    variables.hydro=3
	    value=3
	  endif
;
; note Te is always open, external
;
          if tag_exist(variables,'Te') then begin
              variables.Te='1.5d6'
              if value eq 5 then variables.Te='-2.'
	      if value eq 2 then variables.Te='NULL'
	  endif
;
; note T0 is closed because that is what PFSSMOD uses
;
          if tag_exist(variables,'T0') then begin
	      variables.T0='1.5d6'
              if value eq 5 then variables.T0='-4.'
	      if value eq 2 then variables.T0='NULL'
	  endif
 	  if tag_exist(variables,'oT0') then begin
             if value ne 3 and value ne 4 then variables.oT0='1d6' else variables.oT0='1.5d6'
              if value eq 5 then variables.oT0='-2.'
	      if value eq 2 then variables.oT0='NULL'
	  endif
 	  if tag_exist(variables,'cT0') then begin
              variables.cT0='1.5d6'
              if value eq 5 then variables.cT0='-4.'
	      if value eq 2 then variables.cT0='NULL'
	  endif
	  cduse='1.'
	  oduse='1.'
	  if value eq 5 then cduse='6.'
	  if value eq 4 then cduse='NULL'
	  if value eq 0 then begin 
	   cduse='NULL'
	   oduse='NULL'
	  endif
	  if tag_exist(variables,'T0') then duse = cduse else duse = oduse
          if tag_exist(variables,'densprof') then variables.densprof=duse
          if tag_exist(variables,'odensprof') then variables.odensprof=oduse
          if tag_exist(variables,'cdensprof') then variables.cdensprof=cduse
;
; we don't want to overwrite the change in hydro -- which can happen for numcube
;  so set nreinit=0
;
	  z=execute("for_modeldefaults,nreinit=0,'"+modops.model+"'"+modops.modelstring+",working_dir='"+settings.working_dir+"',ModelInputs=variables")
	  for_widget_model
 	endif
    endif

  endif
 endelse
  
END
