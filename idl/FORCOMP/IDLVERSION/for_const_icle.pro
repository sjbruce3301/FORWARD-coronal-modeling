pro for_const_icle
;
;
; define a structure containing all the constants to be used, CGS units
;
; physical constants
;
;
; CALLED BY FOR_ICLESTART
;

@include.icle
;
;  calculate and store log_e factorials and signum for 3nj programs
;
;  Note that sg is defined from 
fl=dblarr(param.msgn)
sg=fl+1.
for i=1,param.msgn-1 do begin 
   i1=i-1
   fl(i)=fl(i1)+alog(double(i))
   sg(i)=-sg(i1)   
endfor
;
const = {ee:1.602189d-12,hh:6.626176d-27,cc:2.99792458d+10, $
         em:9.109534d-28,uu:1.6605655d-24,bk:1.380662d-16, $
         pi:3.14159265359d0,rsuncm:6.9599d+10,        fl:fl,sg:sg}
;
return
end

