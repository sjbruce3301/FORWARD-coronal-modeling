pro FOR_SE0_COPY,NDIM,SOL
;
; PURPOSE: COPIES SOLUTION OF S.E. EQUATIONS INTO THE DENSITY-MATRIX VECTOR
;
; INPUTS:
;       NDIM
;       SOL
; OUTPUTS:
;       RHO (SET IN COMMON CSE)
; COMMON:
;
;
; CALLED BY FOR_M1SYNTH
;
@include.icle.atom
@include.icle.cse
@include.icle.input
;dk=2
;mxk=2
i=-1
if(input.idebug ne 0) then print,'level      multipole  index matrix    rho '
for ij=0,atom.nk-1 do begin 
   i2j=round(2.d0*cse.aj[ij])
;   for k=0,i2j<mxk,dk do begin 
; this doesnt work with collisions
   for k=0,i2j do begin 
      i+=1
      cse.rho(ij,k)=sol(i)
      if(input.idebug ne 0) then print,ij,k,i,cse.rho(ij,k)
   endfor
   if(input.idebug ne 0) then print,' ALIGNMENT FACTOR, ',cse.rho(ij,2)/cse.rho(ij,0)
endfor


if(input.idebug ne 0) then print,'total sqrt(g)*rho(K=0)',total(cse.rho[0:atom.nk-1,0]*sqrt(lvl.g))



return
end
