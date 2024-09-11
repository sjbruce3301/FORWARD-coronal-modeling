PRO for_widget_build_string2,inputs,pramval,valval,tnames,istep,titlestring

;+ 
;  Name:  FOR_WIDGET_BUILD_STRING2
;
; Creates string used in call to FOR_DRIVE within FOR_WIDGET
;
; Called by FOR_WIDGET_BUILD
;
; Written by Blake Forland, Sarah Gibson, 2013-2014
; Version 2.0 July 2014

;
; DOSTRING2
;

  if strupcase(valval) eq 'STRUCT1' $
	or strupcase(valval) eq 'STRUCT2' $
	or strupcase(valval) eq 'STRUCT3' $
	or strupcase(valval) eq 'STRUCT4' then begin

  	if strupcase(valval) eq 'STRUCT3' then begin
           titlestring = titlestring+"'1\"+pramval[0]+"',"
           for k = 1, n_elements(pramval) - 1 do begin
              if k lt n_elements(pramval) - 1 then numuse='0'
              if k eq n_elements(pramval) - 1 then numuse='2'
              titlestring = titlestring+"'"+numuse+"\"+pramval[k]+"',"
	   endfor
        endif else if strupcase(valval) eq 'STRUCT4' then begin
           stringstruct4 = tag_names(pramval)
           z=execute('pramsstruct2 = inputs.'+tnames(istep+4))
           stringstruct2 = tag_names(pramsstruct2)
           z=execute('pramsstring1 = inputs.'+tnames(istep+2))
           titlestring = titlestring+"'1\"+pramsstring1[0]+"',"
	   typetest4=strarr(n_elements(stringstruct4))
           for k = 0,n_elements(stringstruct4)-1 do x = execute("typetest4[k] = pramval."+stringstruct4[k])
	   tottypes=n_elements(uniq(typetest4))
           for k = 0,n_elements(stringstruct4)-1 do begin
              if k eq 0 then typetest4last='null'
              if k ne 0 then x = execute("typetest4last = pramval."+stringstruct4[k-1])
              if (typetest4[k] ne typetest4last) then begin
               titlestring = titlestring+"'1\"+typetest4[k]+"',"
               for kk = 0,n_elements(stringstruct4)-1 do begin
                 x = execute("typetest4loop = pramval."+stringstruct4[kk])
                 if typetest4loop eq typetest4[k] then begin
                   if strupcase(pramsstring1[kk+1]) ne 'KCOR' then begin
                     titlestring = titlestring+"'1\"+pramsstring1[kk+1]+"',"
	             x= execute("typetest2 = pramsstruct2."+stringstruct2[kk])
                     for kkk = 0, n_elements(typetest2) - 1 do begin
                      if kkk lt n_elements(typetest2) -1 then titlestring = titlestring+"'0\"+typetest2[kkk]+"',"
                      if kkk eq n_elements(typetest2) -1 then titlestring = titlestring+"'2\"+typetest2[kkk]+"',"
	             endfor
	           endif
	        endif
	       endfor
               titlestring=titlestring+"'2\',"
	      endif
           endfor
           titlestring=titlestring+"'2\',"
        endif

  endif

end
