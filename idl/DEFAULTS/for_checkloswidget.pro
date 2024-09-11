pro for_checkloswidget,cmer=cmer,upoccult=upoccult,axisym=axisym,notpos,notcarr,notuser

;
; program test if coming from widget, and if so,
; what previous grid type
;
;
; Called by FOR_LOSDEFAULTS
;
; Written Sarah Gibson 2014
; Version 2.0 July 2014


notpos=0
notcarr=0
notuser=0

;
; first be sure coming from widget
;

  if n_elements(axisym) ne 0 then begin
    if strupcase(axisym) eq 'NULL' then begin
	if n_elements(upoccult) ne 0 then if strupcase(string(upoccult)) eq 'NULL' then notpos = 1
	if n_elements(upoccult) ne 0 then if strupcase(string(upoccult)) ne 'NULL' then notcarr = 1
	if n_elements(upoccult) ne 0 then if strupcase(string(upoccult)) ne 'NULL' then notuser = 1
	if n_elements(cmer) ne 0 then if strupcase(string(cmer)) eq 'NULL' then notcarr = 1
;
; note -- since USER can now have CMER set, the above test will not always work (thus, there may be some cases of coming from USER where NOTCARR not set)
; also, the command below has been commented because it is no longer a good test
; for NOTUSER
;  This should be fixed if USER ever gets brought into the widget interface; 
;  otherwise, it does not matter
;
;	if n_elements(cmer) ne 0 then if strupcase(string(cmer)) ne 'NULL' then notuser = 1
    endif
  endif


end
