function for_gammae,ij,k
;
;
; purpose: compute elastic multipolar collisional rate
;
; inputs:
;       ij   level index
;       k    multipolar component
; outputs:
;
; common:
;
; restrictions:  
;
; CALLED BY FOR_CE
; CALLS FOR_FUN3J
;
; comments: october 6, 1999, p. judge
; 
@include.icle
@include.icle.cse
@include.icle.input
@include.icle.thermal
@include.icle.corona
gammae=0.d0
if(input.icoll  eq  0) then return,gammae
if (k  eq  2) then gammae=input.cecoef*ed
;
; eq before (a4.10b) of landi's book:
;
aa=cse.aj(ij) 
fk=double(k)
for  an=-aa,aa do begin 
   for  anp=-aa,aa do begin 
      for  p=-fk,fk do begin 
         n=fix(an)
         np=fix(anp)
         f3=for_fun3j(aa,aa,fk,-an,anp,p)
         gammae+=cmm(ij,ij,n,np)*f3*f3*(2.d0*fk+1.d0)/(2.d0*aa+1.d0)
      endfor 
   endfor 
endfor 
return,gammae
end


