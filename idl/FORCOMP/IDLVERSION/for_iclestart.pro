pro for_iclestart
;
; CALLS FOR_CONST_ICLE, FOR_PARAM_ICLE, FOR_ATOM, FOR_ATOM1, FOR_RINPUT
; 
; CALLED BY FORCOMP
;

@include.icle
@include.icle.input
@include.icle.atom
@include.icle.cse
@include.icle.emerge
;
;  set up parameters (limits)
;
for_param_icle
;
;  store constants including some variables depending on the above parameters 
;
for_const_icle
;
; read input file
;
for_rinput
;
for_atom
;
for_atom1
;
emerge={emiss:dblarr(atom.nline,5,param.mq),emerge:dblarr(atom.nline,5,param.mq)}
;
end
