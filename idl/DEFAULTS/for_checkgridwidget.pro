pro for_checkgridwidget,rheight=rheight,ngy=ngy,ruser=ruser,notpos,notcarr,notuser

;
; program test if coming from widget, and if so,
; what previous grid type
;
; Called by FOR_GRIDDEFAULTS
;
; Written Sarah Gibson 2014
; Version 2.0 July 2014
;
; August 2021 -- changed test for notpos
; Sept 2021 - instituted changeaz
; Oct 2021 - removed changeaz 

notpos=0
notcarr=0
notuser=0

if n_elements(ngy) ne 0 then $
	if strupcase(string(ngy)) eq 'NULL' then notpos = 1
;
;  added this next because DATA may have ngy defined for CARRMAP
;
if n_elements(rheight) ne 0 then $
	if strupcase(string(rheight)) ne 'NULL' then notpos = 1

if n_elements(rheight) ne 0 then $
	if strupcase(string(rheight)) eq 'NULL' then notcarr = 1

if n_elements(ruser) ne 0 then begin
	test=where(strupcase(string(ruser)) eq 'NULL') 
        if min(test) ne -1 then notuser = 1
endif

end
