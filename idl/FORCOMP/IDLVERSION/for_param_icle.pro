pro for_param_icle,dummy ; include file, use @parameters
;+
; set parameters for ICLE, stored in common block sructure called
; param.
;
; CALLED BY FOR_ICLESTART
;-
@include.icle
;
mk=3  ; max number of levels in atomic models
mline=5 ; max number of lines 
mwide=mline
mjmax=3 ; maximum total angular momentum qn
;
param={mk:mk,mline:mline,mwide:mwide,msgn:1000,mjmax:mjmax,$
       mrad:mline+mwide,mq:100,mjtot:mk*(2*mjmax+1),mtgrid:30}
return
end
