pro for_solve,ndim,b
;
;
; returns solution to SE matrix equation
;
; CALLED BY FOR_M1SYNTH

@include.icle.input
@include.icle.cse
;
;  cse.weight is set in for_se0_build
;  cse.scoeff is set in for_se0_build
;
;
a=cse.scoeff(0:ndim-1,0:ndim-1)
b=reform(a(0,*))*0.d0
;
if(input.idebug ne 0) then begin
   s=string(ndim)
   print,'rate matrix no isum'
   print,a,form='('+s+'(f9.3,1x))'
endif
;
;  particle number density conservation
;
isum=input.isum-1
a=transpose(a)
a[*,isum]=cse.weight[0:ndim-1]
;
if(input.idebug ne 0) then begin
   print,'rate matrix including isum'
   print,a,form='('+s+'(f9.3,1x))'
endif
;

la_ludc,a,index,/double
b(isum)=1.d0
b=la_lusol(a,index,temporary(b),/double)

return
end
