PRO for_widget_build,inputs,inputsname=inputsname,stringout=stringout,titlestring=titlestring,printresult=printresult,bar=bar,dotab=dotab,changevalue=changevalue,dostruc=dostruc,close=close

;+ 
;  Name:  FOR_WIDGET_BUILD
;
;  For each INPUTS type, this program can fill in widgets taking 
;	into account types of input parameters,
;	e.g., strings, number, array, toggle, calendar, etc.
;
;	also can generate a string command from the
;  	FOR_*DEFAULTS program Inputs files, to be used in
;  	FOR_WIDGET structures STRINGS
;	
;	also can generate print command
;
; 	also can check widget input change against
;	INPUTS and change value within INPUTS
;
;  Called by FOR_WIDGET, FOR_WIDGET_PRINT_COMMAND, 
;	FOR_WIDGET_MODEL,FOR_WIDGET_PLOT
;	FOR_WIDGET_OUTPUT,FOR_WIDGET_SETTINGS
;	and EVENT versions of all of the above
;
;  Calls FOR_WIDGET_BUILD_SETUP, FOR_WIDGET_BUILD_SIMPLE, FOR_WIDGET_BUILD_STRING2
;	  FOR_WIDGET_BUILD_COMPLEX, FOR_WIDGET_DOSTRUC
;
;  Possible Outputs: 
;
;	1) strings of names of parameters (not explicit, e.g.
;		ObsOps.Line) - these get set up in FOR_WIDGET on reset
;		or first time. Uses keywords INPUTS, INPUTSNAME,
;		STRINGOUT and TITLESTRING (for Observables menu bar).
;
;	2) printcommand - this is the concatenation of the strings,
;		with the parameter values made explicit, e.g.
;		ObsOps.Line='pB'. Called by FOR_WIDGET_PRINT_COMMAND,
;		and can change any time a widget is altered.	
;		Uses keyword INPUTS, PRINTRESULT.
;
;	3) widget button content - this determines name and content of 
;		parameters, with the widget presentation depending on 
;		type. Called by	any of the widget setup programs.
;		Uses keyword INPUTS, BAR, DOTAB, DOSTRUC, CLOSE.
;
;	4) widget change check and inputs change - this figures out which 
;		INPUTS parameter has changed. Called from the widget EVENT
;		codes. Uses keyword INPUTS, CHANGEVALUE.
;
; Written by Sarah Gibson 2013-2014
; Version 2.0 July 2014
;


tnames=tag_names(inputs)

if n_elements(stringout) ne 0 then begin
 	stringout = ','
endif

if n_elements(titlestring) ne 0 then begin
 	titlestring = ''
endif

if  keyword_set(bar) and  keyword_set(dostruc) eq 0 then begin
	usebar=bar
	if  keyword_set(dotab) then $
		usebar = widget_base(bar,title=dotab,COLUMN=2)
;
; left hand widget descriptor position
; right hand widget value position
;
 	left=widget_base(usebar,/column)
	right=widget_base(usebar,/column)
endif

if  keyword_set(changevalue) then begin
 	  if tag_exist(changevalue,'VALUE') eq 0 then begin
	  	widget_control,changevalue.id,get_uvalue=uvalue,get_value=value
	  endif else begin
;
; a tag of VALUE means it is an array type tab, and we need to rerun
; making the plot widget. This will happen in FOR_WIDGET_PLOT_EVENT
; but, since the value will contain its name as well at its value,
; it needs to be extracted differently here
;
		result = strsplit(changevalue.value,'.',/EXTRACT)
		uvalue = result[0]
		uvalue = strsplit(uvalue,':',/EXTRACT)
	        uvalue=uvalue[0] + 'VAL'
		value = result[1]
	  endelse
endif


if  keyword_set(dostruc) eq 0 then begin

 for i = 0,n_elements(tnames)-1 do begin

  for_widget_build_setup,inputs,tnames,i,pramval,valval,test1

;
; DOSTRING2
;

  if exist(titlestring) then for_widget_build_string2,inputs,pramval,valval,tnames,i,titlestring

;
; now do the STRING, PRINT, and most of the WIDG/CHANGE calculations
; 	(in this SIMPLE routine, won't do NULL, TOG, structure,
;       or other complicated type for WIDG/CHANGE - 
;       these will be dealt with next)
;

  if test1 lt 0 then $
    for_widget_build_simple,inputs,pramval,valval,i,tnames,stringout=stringout,printresult=printresult,inputsname=inputsname,value=value,uvalue=uvalue,left=left,right=right


 endfor

;
; now take care of widget building/change for non-simple parameter types
;
;  First, everything but structures
;

 if  keyword_set(bar) or exist(value) then begin

  for i = 0,n_elements(tnames)-1 do begin

    for_widget_build_setup,inputs,tnames,i,pramval,valval,test1

    if test1 lt 0 then $
      for_widget_build_complex,inputs,pramval,valval,i,tnames,value=value,uvalue=uvalue,left=left,right=right

  endfor

 endif

endif 

if  keyword_set(dostruc) or exist(value) then begin

;
; finally, do the structures for the widget and change
;
  for i = 0,n_elements(tnames)-1 do begin

    for_widget_build_setup,inputs,tnames,i,pramval,valval,test1

    if test1 lt 0 and (strupcase(valval) eq 'STRUCTURE' or strupcase(valval) eq 'STRUCT') then begin
	for_widget_dostruc,inputs,pramval,left=left,right=right,value=value,uvalue=uvalue,bar=bar
    endif
                    
  endfor
endif

if  keyword_set(close) and  keyword_set(bar) then begin
  if exist(usebar) ne 1 then usebar=bar
  if exist(left) ne 1 then left=widget_base(usebar,/column)
  newselect = widget_base(left,/column)
  closebtn=widget_button (newselect, uvalue='close', value='Close')
endif

END
