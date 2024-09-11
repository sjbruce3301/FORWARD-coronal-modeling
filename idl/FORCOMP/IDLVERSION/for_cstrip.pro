;
;**************************************************************
;
pro for_cstrip,filen,nosignal=nosignal,comment=comment,noblanks=noblanks
;+
; for_cstrip,filen,nosignal=nosignal,comment=comment,noblanks=noblanks
; strip comment lines from input file
;
; CALLED BY FOR_ATOM
;-
text=' '
if(n_elements(nosignal) eq 0) then $ 
  print,'stripping comments from ', filen
openr,lu1,filen,/get
openw,lu2,'dums.dat',/get
if(n_elements(comment) eq 0) then comment='*' else $
  print,'stripping lines starting with ', comment
while not eof(lu1) do begin
  readf,lu1,text
  if(n_elements(noblanks) gt 0) then text=strtrim(text,2)
  if (strmid(text,0,1) ne comment) then begin
      printf,lu2,text,format = '(a)'
  endif
endwhile
free_lun,lu1
free_lun,lu2
return
end
;
;*****************************************
;
