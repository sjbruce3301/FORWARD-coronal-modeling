PRO for_widget_build_simple,inputs,pramval,valval,istep,tnames,stringout=stringout,printresult=printresult,inputsname=inputsname,value=value,uvalue=uvalue,left=left,right=right

;+ 
;  Name:  FOR_WIDGET_BUILD_SIMPLE
;
;
; at first, don't do NULL, TOG, structure, 
;	or other complicated type, at least for widget and changebutton
;	(these will be dealt with later)
;
; Calls FOR_WIDGET_DONORM
;
; Called by FOR_WIDGET_BUILD, FOR_WIDGET_DOSTRUC
;
; Written by Blake Forland, Sarah Gibson 2013-2014
; Version 2.0 July 2014
;
; added hooks for TOMO, STRIA (Jan 2021)
; June 2023 -- added SYNCOM hooks


test2=0

checksize=size(pramval)
css=fix(checksize[0])
if css eq 0 then begin
  if strupcase(pramval) eq 'NULL' then test2=1
endif

if strupcase(valval) eq 'TOG' $
		or strupcase(valval) eq 'STRUCTURE' $
		or strupcase(valval) eq 'STRUCT' $
		or strupcase(valval) eq 'CALENDAR' $
		or strupcase(valval) eq 'SEARCHFILEIFSET' $
		or strupcase(valval) eq 'SEARCHDIRIFSET' $
		or strupcase(valval) eq 'ARRAY' $
		or strupcase(valval) eq 'NODISPLAY' $
		or strupcase(tnames[istep]) eq 'CUBENAME' $
		or strupcase(tnames[istep]) eq 'PFSSFILE' $
		or strupcase(tnames[istep]) eq 'TOMOFILE' $
		or strupcase(tnames[istep]) eq 'STRIAFILE' $
		or strupcase(tnames[istep]) eq 'SYNCOMFILE' $
	  		then test2 = 1
  
if istep lt n_elements(tnames) - 1 then addstring="," else addstring=""
if istep eq n_elements(tnames) - 2 then begin
	if strpos(strupcase(tnames[istep+1]),'VAL') gt 0 then addstring=""
endif

if strupcase(valval) ne 'STRUCTURE' and strupcase(valval) ne 'STRUCT' and size(pramval,/tname) ne 'STRUCT' then begin
;
; DOSTRING - non-structure
;
    	if n_elements(stringout) ne 0 then $
		stringout = stringout+tnames[istep]+'='+inputsname+'.'+tnames[istep]+addstring 
;
; check to see if array
	pramarray=0
 	if n_elements(pramval) gt 1 then pramarray=1	

        addpram=""
        if size(pramval,/tname) eq 'STRING' then addpram="'"

	if pramarray eq 0 then begin

	    name=strtrim(pramval,2)
;
; DOPRINT - non-structure, non-array
;
	    if n_elements(printresult) ne 0 then $
		printresult=printresult+tnames[istep]+"="+addpram+name+addpram+addstring

;
; DOCHANGE - simple, non-array
;

;	    if n_elements(value) ne 0 then begin
;		print,uvalue,tnames[istep]
;		if strupcase(uvalue) eq tnames[istep]+'VAL' then stop
;	    endif
	    if n_elements(value) ne 0 and test2 eq 0 then begin
		x=execute("if strupcase(uvalue) eq '"+tnames[istep]+"VAL' then inputs."+tnames[istep]+"=value")
	    endif

;
; DOWIDG - simple, non-array
;

	    if n_elements(left) ne 0 and test2 eq 0 then begin

		for_widget_donorm,left,right,tnames[istep],name

	    endif

	endif else begin

	    for k = 0,n_elements(pramval)-1 do begin
	    	name=strtrim(pramval[k],2)
	        if k eq 0 then arrstr="["+addpram+name+addpram+","
	        if k gt 0 and k lt n_elements(pramval)-1 then arrstr=arrstr+$
			addpram+name+addpram+","
	        if k eq n_elements(pramval) - 1 then arrstr=arrstr+addpram+name+addpram+"]"
;
; DOWIDG - simple, array
;

	        if n_elements(left) ne 0 and test2 eq 0 then begin

		   for_widget_donorm,left,right,tnames[istep]+"_"+strtrim(k+1,2),name

	         endif
;
; DOCHANGE - simple, array
;
	    	if n_elements(value) ne 0 and test2 eq 0  then $
			x=execute("if strupcase(uvalue) eq '"+tnames[istep]+"_"+strtrim(k+1,2)+"VAL' then inputs."+tnames[istep]+"["+strtrim(k,2)+"]=value")


	    endfor
;
; DOPRINT - non-structure, array
;
	    if n_elements(printresult) ne 0 then $
		printresult=printresult+tnames[istep]+"="+arrstr+addstring

	endelse

endif else begin

	structnames=tag_names(pramval)
	for k = 0,n_elements(structnames)-1 do begin
           if k lt n_elements(structnames) - 1 then addstring2="," else addstring2=addstring
    	   if k eq n_elements(structnames) - 2 then $
			if strpos(strupcase(structnames[k+1]),'VAL') gt 0 then addstring2=addstring
           z=execute('structval = pramval.'+structnames[k])
	   valtest=strpos(strupcase(structnames[k]),'VAL')
	   if valtest lt 0 then begin
;
; DOSTRING - structure
;
	     if n_elements(stringout) ne 0 then $ 
		stringout = stringout+structnames[k]+'='+inputsname+'.'$
			+tnames[istep]+'.'+structnames[k]+addstring2 
;
; DOPRINT - structure
;
	    if size(structval,/tname) eq 'BYTE' then structval=fix(structval)
	    name=strtrim(structval,2)
            addpram2=""
            if size(structval,/tname) eq 'STRING' then addpram2="'"
	    if n_elements(printresult) ne 0 then $
		printresult=printresult+structnames[k]+"="+addpram2+name+addpram2+addstring2

	   endif

	endfor

endelse

end
