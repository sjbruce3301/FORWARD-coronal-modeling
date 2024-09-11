FUNCTION for_atomn, x, number = number,string = string
;+
; PROJECT:
;       HAOS-DIPER
;
; NAME:	
;       for_atomn
;
; PURPOSE: converts between element atomic number (int) and name (str)
; twofold:  
; 1.  to give atomic number of element if x is a string containing
;     the name of the element, e.g. if input  is 'H' it will return 1
; 2.  to give name of element if x is an integer containing
;     the atomic number of the element, e.g. if input  is 1 it will return 'H'
;       
; CATEGORY: HAOs DIPER basic atomic naming conventions
;   
; EXPLANATION:
;       
; CALLING SEQUENCE: 
;       Result = atomn(i)
;
; INPUTS:
;       x string or integer, can be an array
; OPTIONAL INPUTS: 
;       None.
;
; OUTPUTS:
;       integer or string, depending on the output
;
; OPTIONAL OUTPUTS:
;       None.
;
; KEYWORD PARAMETERS: 
;       /number - force output to be the atomic number (integer)
;       /string - force output to be a string (either 'C' or '6')
;
; CALLED BY FOR_EQION
;
; COMMON BLOCKS:
;       None.
;
; RESTRICTIONS: 
;
; SIDE EFFECTS:
;       None.
;
; CATEGORY:
;       
; PREVIOUS HISTORY:
;       Written April 25, 1996, by Philip G. Judge
;
; MODIFICATION HISTORY:
;       
; VERSION:
;       Version 1, April 25, 1996
;-
;
if(n_params(0) lt 1) then begin
;  print,'atomn(x)'
  return,-1
end
IF(n_elements(string) NE 0 AND n_elements(number) NE 0) THEN BEGIN
   message,/info,'must set EITHER /string or /number, not both'
ENDIF

sz=size(x)
;
; if string given and /str set then return input
;
yesstr = sz(1+sz(0)) EQ 7
IF(yesstr AND n_elements(string) NE 0) THEN return,strupcase(x)
;
; if number given and /number set then return input
;
yesstr = sz(1+sz(0)) EQ 7
IF(NOT yesstr AND n_elements(number) NE 0) THEN return,x
;
;
;  atomic data
;
name_data=['H','HE','LI','BE','B','C','N','O','F','NE',$
           'NA','MG','AL','SI','P','S','CL','AR',$
           'K','CA','SC','TI','V','CR','MN','FE','CO','NI','CU',$
           'ZN','GA','GE','AS','SE','BR','KR','RB','SR','Y','ZR','NB']
name2 = ['MO','TC','RU','RH','PD','AG','CD','IN','SN','SB','TE','I',$
           'XE','CS','BA','LA','CE','PR','ND','PM','SM','EU','GD','TB','DY',$
           'HO','ER','TM','YB','LU','HF','TA','W','RE','OS','IR','PT','AU',$
           'HG','TL','PB','BI','PO','AT','RN','FR','RA','AC','TH','PA','U']
name_data = [name_data,name2]
n=n_elements(name_data)
an_data=indgen(n)+1
;
;
;  is it an array or scalar?
;
if(sz(0) eq 0) then input=[x] else input=x
;
;  is it a string?
;
if(sz(1+sz(0)) eq 7) then begin 
   strin = 1
   input=strupcase(x) 
   output=intarr(n_elements(x))
   for i=0,n_elements(input)-1 do begin
      j=where(name_data eq input(i),kount)
      if(kount eq 1) then output(i)=an_data(j(0)) ELSE output(i) = -1
   endfor
;
;  or an integer/ real etc,
;
endif else begin 
   strin = 0
   input=round(x)
   output=strarr(n_elements(x))+'NOID'
   for i=0,n_elements(input)-1 do begin
      j=where(an_data eq input(i),kount)
      if(kount eq 1) then output(i)=name_data(j(0))ELSE output(i) = 'No ID'
   endfor
endelse
if(sz(0) eq 0) then output=output(0)
return,output
end
