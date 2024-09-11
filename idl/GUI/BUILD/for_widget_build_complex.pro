PRO for_widget_build_complex,inputs,pramval,valval,istep,tnames,value=value,uvalue=uvalue,left=left,right=right

;+ 
;  Name:  FOR_WIDGET_BUILD_COMPLEX
;
; Sets up widget for Toggle, Calendar, File/Directory I/O, Array
;
;
; Calls FOR_WIDGET_DOTOG, FOR_WIDGET_DOARR
;
; Called by FOR_WIDGET_BUILD, FOR_WIDGET_DOSTRUC, DIALOG_PICKFILE
;
; Written by Sarah Gibson, Blake Forland, 2013-2014
; Version 2.0 July 2014
; added hooks for TOMO, STRIA - Jan 2021
; June 2023 -- added SYNCOM hooks


  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops

if strupcase(valval) eq 'TOG' $
	  or strupcase(valval) eq 'CALENDAR' $
	  or strupcase(valval) eq 'SEARCHFILEIFSET' $
	  or strupcase(valval) eq 'SEARCHDIRIFSET' $
	  or strupcase(valval) eq 'ARRAY' $
	  or strupcase(tnames[istep]) eq 'CUBENAME' $
	  or strupcase(tnames[istep]) eq 'PFSSFILE' $
	  or strupcase(tnames[istep]) eq 'TOMOFILE' $
	  or strupcase(tnames[istep]) eq 'STRIAFILE' $
	  or strupcase(tnames[istep]) eq 'SYNCOMFILE' $
	  then begin

		ignore=0
  		checksize=size(pramval)
		css=fix(checksize[0])
		if css eq 0 then begin
		   if strupcase(pramval) eq 'NULL' then ignore=1
		endif 
		if strupcase(valval) eq 'ARRAY' then begin
 		   a=execute("valvaluse = inputs."+tnames[istep+1])
		   if strupcase(valvaluse[0]) eq 'NODISPLAY' then ignore=1
	 	endif

		if ignore ne 1 then begin

		 if n_elements(left) ne 0 then begin

 			 raduse = widget_base(left,/column,/nonexclusive)

  			 selectuse = widget_base(right,/column)

		 endif

		 if strupcase(valval) eq 'TOG' then begin

;
; if pramval for a TOG has been set to -1 then it will not be added to widget
; (essentially same as ignore)
;

;
; DOWIDG - TOG
;

		  if n_elements(left) ne 0 then for_widget_dotog,raduse,tnames[istep],pramval

;
; DOCHANGE - TOG
;
	    	  if n_elements(value) ne 0 then begin

                   if strupcase(uvalue) eq tnames[istep]+"VAL" then begin

                     if pramval eq 0  then $
                       c = execute("inputs."+tnames[istep]+"=1") $
                       else c = execute("inputs."+tnames[istep]+"=0")

		     flag.widgregen=1

		   endif

		  endif

		 endif

		 if strupcase(valval) eq 'ARRAY' then begin

;
; DOWIDG - ARRAY
;
			if n_elements(left) ne 0 then for_widget_doarr,pramval,valvaluse,tnames[istep],selectuse
;
; DOCHANGE - ARRAY
;
	    	 	if n_elements(value) ne 0 then begin
                         if strupcase(uvalue) eq tnames[istep]+"VAL" then begin
		  		x=execute("inputs."+tnames[istep]+"=value")
		     		flag.widgregen=1
			 endif
			endif
		 endif

		 if strupcase(valval) eq 'SEARCHFILEIFSET' or strupcase(valval) eq 'SEARCHDIRIFSET' or strupcase(tnames[istep]) eq 'CUBENAME' or strupcase(tnames[istep]) eq 'PFSSFILE' or strupcase(tnames[istep]) eq 'TOMOFILE' or strupcase(tnames[istep]) eq 'STRIAFILE' or strupcase(tnames[istep]) eq 'SYNCOMFILE' then begin

;
; DOWIDG - SEARCHIFSET
;
                   if n_elements(left) ne 0 then x=execute("rad=widget_button(selectuse,value='"+tnames[istep]+":"+pramval+"',uvalue='"+tnames[istep]+"VAL')")

;
; DOCHANGE - SEARCHIFSET
;
		   if n_elements(value) ne 0 then begin
		     if strupcase(uvalue) eq tnames[istep]+'VAL' then begin
		       usedir='.'
	 	       if settings.working_dir ne '' then usedir=settings.working_dir
   		       if strupcase(valval) eq 'SEARCHDIRIFSET' then begin
			dirname = dialog_pickfile(/directory,path=usedir)
			dirname2 = strjoin(strsplit(dirname,/extract,'/'),'/')
			dirname2='/'+dirname2
		        if dirname2 eq '/' then dirname2=''
			x=execute("inputs."+tnames[istep]+" = dirname2")
		       endif
   		       if strupcase(valval) eq 'SEARCHFILEIFSET' or strupcase(tnames[istep]) eq 'CUBENAME' or strupcase(tnames[istep]) eq 'PFSSFILE' or strupcase(tnames[istep]) eq 'TOMOFILE' or strupcase(tnames[istep]) eq 'STRIAFILE' or strupcase(tnames[istep]) eq 'SYNCOMFILE' then x=execute("inputs."+tnames[istep]+" = dialog_pickfile(path=usedir)")

		     endif

		   endif

                 endif

                endif

endif

end
