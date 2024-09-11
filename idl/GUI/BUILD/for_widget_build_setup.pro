PRO for_widget_build_setup,inputs,tnames,istep,pramval,valval,test1

;+ 
;  Name:  FOR_WIDGET_BUILD_SETUP
;
; Sets up before stepping through widget types
;
;  Called by FOR_WIDGET_DOSTRUC, FOR_WIDGET_BUILD
;
; Written Sarah Gibson, Blake Forland, 2013-2014
; Version 2.0 July 2014

;
; check to be sure not a VAL
;	(test1 would be positive)
;

  test1 = strpos(strupcase(tnames[istep]),'VAL')

;
; will need to look at contents of the parameter itself
;

  a=execute("pramval = inputs."+tnames[istep])

;
;  and at VAL contents 
;  if they are there in next array element
;  (true for all but model parameters - 
; 	for model parameters do a type check)
;

  valval=size(pramval,/tname)
  if istep lt n_elements(tnames)-1 then begin
	if strpos(strupcase(tnames[istep+1]),'VAL') gt 0 then begin
		a=execute("valval = inputs."+tnames[istep+1])
		if n_elements(valval) ne 1 then valval='ARRAY'
	endif
  endif

;
;  ignore ALL_INST, ALL_LINES, and PHYS_PARAMS
;	in OBSOPS for most purposes -- 
;	these are used only to build STRINGOUT2
;	

  if strupcase(valval) eq 'STRUCT1' $
	or strupcase(valval) eq 'STRUCT2' $
	or strupcase(valval) eq 'STRUCT3' $
	or strupcase(valval) eq 'STRUCT4' then test1=1

;
; similarly, ignore model name parameter in model parameters inputs array
;	for purposes of widgets and strings and printcommand, since duplicated by
;	ModOps.ModName and appears on top of widget box and controled
;	by top widget.
;

  if strupcase(tnames[istep]) eq 'NAME' then test1 = 1.

end
