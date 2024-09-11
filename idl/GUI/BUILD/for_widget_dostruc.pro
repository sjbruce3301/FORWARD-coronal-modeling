PRO for_widget_dostruc,inputs,pramval,left=left,right=right,value=value,uvalue=uvalue,bar=bar

;+ 
;  Name:  FOR_WIDGET_DOSTRUC
;
; Deal with widget input in Structure form
;
; Called by FOR_WIDGET_BUILD
;
;  External calls: FOR_WIDGET_PLOT, FOR_WIDGET_BUILD_SETUP, FOR_WIDGET_BUILD_SIMPLE, FOR_WIDGET_BUILD_COMPLEX
;
; Written Blake Forland, Sarah Gibson 2013-2014
; Version 2.0 July 2014

 common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops

structname=tag_names(pramval)
x=execute("thingtochange=inputs."+structname[0]+"INPUTS")

;
; check to see if structure is activated
; by looking at its first element
;

for_widget_build_setup,pramval,structname,0,strucval,strucvalval,test1

if strupcase(strucvalval) ne 'NULL' then begin

   structog=strucval

   if  keyword_set(bar) then begin
    usebar = widget_base(bar,title=structname[0],COLUMN=2)
    left=widget_base(usebar,/column)
    right=widget_base(usebar,/column)
   endif

;
; simple stuff
;

   for j = 2, n_elements(structname) - 1 do begin

 	if structog gt 0 then begin

 	  for_widget_build_setup,pramval,structname,j,strucval,strucvalval,test1

   	  if test1 lt 0 then begin
 		for_widget_build_simple,thingtochange,strucval,strucvalval,j,structname,value=value,uvalue=uvalue,left=left,right=right
 	        if n_elements(value) ne 0 then x=execute("inputs."+structname[0]+"INPUTS."+structname[j]+"=thingtochange."+structname[j])
	  
 	  endif
 	endif

   endfor

;
; complex stuff
;

   for j = 2, n_elements(structname) - 1 do begin

 	if structog gt 0 then begin

 	  for_widget_build_setup,pramval,structname,j,strucval,strucvalval,test1

   	  if test1 lt 0 then $
 		for_widget_build_complex,thingtochange,strucval,strucvalval,j,structname,value=value,uvalue=uvalue,left=left,right=right
 	        if n_elements(value) ne 0 then x=execute("inputs."+structname[0]+"INPUTS."+structname[j]+"=thingtochange."+structname[j])

 	endif

   endfor

;
; now do main toggle button
;
   for_widget_build_setup,pramval,structname,0,strucval,strucvalval,test1
   for_widget_build_complex,thingtochange,strucval,strucvalval,0,structname,value=value,uvalue=uvalue,left=left,right=right

   if n_elements(value) ne 0 then begin 
     x=execute("inputs."+structname[0]+"INPUTS."+structname[0]+"=thingtochange."+structname[0])
   endif
endif

END
