PRO for_widget_doarr,pramval,valvaluse,tname,selectuse

;+ 
;  Name:  FOR_WIDGET_DOARR
;
; Deals with widget inputs in array form
;
; Called by FOR_WIDGET_BUILD_COMPLEX
;
; Written by Blake Forland, Sarah Gibson 2013-2014
; Version 2.0 July 2014
;
			arrtest=n_elements(valvaluse)

              		dropstring = "["
              
			for j = 0 , arrtest do begin
                    
                    	  if j eq 0 then begin
                       
                       		dropname=tname+':'+strtrim(pramval,2)
                       
                       		dropop = "{MENU,1,'"+dropname+"','"+dropname+"'},"
                       
                       		dropstring = dropstring + dropop
                       
                    	  endif 
                    
                          if j gt 0 and j lt arrtest then begin
                       
                       		dropname=strtrim(valvaluse[j-1],2)
                       
                       		dropop = "{MENU,0,'"+dropname+"','"+dropname+"'},"
                       
                       		dropstring = dropstring + dropop
                       
                          endif
                    
                          if j eq arrtest then begin
                       
                       		dropname=strtrim(valvaluse[j-1],2)
                       
                       		dropop = "{MENU,2,'"+dropname+"','"+dropname+"'}]"
                       
                       		dropstring = dropstring + dropop
                       
                    	  endif
                    
                        endfor
                 
                        z = execute("drop = cw_pdmenu(selectuse,"+dropstring+",/RETURN_FULL_NAME)")

end
