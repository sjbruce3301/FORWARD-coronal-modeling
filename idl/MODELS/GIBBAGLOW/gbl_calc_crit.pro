pro gbl_calc_crit,apar,bpar,norm1,norm3,gamma1,gamma3


;=======================Calculate critical points===================================
;

rcrit1=1.d0
rcrit1b=1.d0
rcrit2=bpar
rcrit2b=(bpar+apar)/(1.+apar)

Dstrfunc_eq_1=gbl_diff_streamfunc(rcrit1b,!dpi/2.,bpar,norm1=norm1,norm3=norm3)
Dstrfunc_eq_2=gbl_diff_streamfunc(rcrit2b,!dpi/2.,bpar,norm1=norm1,norm3=norm3)

Dsf1_req1=Dstrfunc_eq_1.Dsf1_r
Dsf1_theq1=Dstrfunc_eq_1.Dsf1_th
Dsf3_req1=Dstrfunc_eq_1.Dsf3_r
Dsf3_theq1=Dstrfunc_eq_1.Dsf3_th
Dsf1_req2=Dstrfunc_eq_2.Dsf1_r
Dsf1_theq2=Dstrfunc_eq_2.Dsf1_th
Dsf3_req2=Dstrfunc_eq_2.Dsf3_r
Dsf3_theq2=Dstrfunc_eq_2.Dsf3_th

;Br = (1./rpB^2/sin(thetapB))*(gamma1*Dsf1_theq+gamma3*Dsf3_theq)
;Bth = (-1./rpB/sin(thetapB)/(1.+apar))*(gamma1*Dsf1_req+gamma3*Dsf3_req)
;we want Bmag=0

T11=gamma1*gamma1*(Dsf1_theq1*Dsf1_theq1/rcrit1^2 + Dsf1_req1*Dsf1_req1/(1.+apar)/(1.+apar))
T21=2*gamma1*(Dsf1_theq1*Dsf3_theq1/rcrit1^2 + Dsf1_req1*Dsf3_req1/(1.+apar)/(1.+apar))
T31=(Dsf3_theq1*Dsf3_theq1/rcrit1^2 + Dsf3_req1*Dsf3_req1/(1.+apar)/(1.+apar))

T12=gamma1*gamma1*(Dsf1_theq2*Dsf1_theq2/rcrit2^2 + Dsf1_req2*Dsf1_req2/(1.+apar)/(1.+apar))
T22=2*gamma1*(Dsf1_theq2*Dsf3_theq2/rcrit2^2 + Dsf1_req2*Dsf3_req2/(1.+apar)/(1.+apar))
T32=(Dsf3_theq2*Dsf3_theq2/rcrit2^2 + Dsf3_req2*Dsf3_req2/(1.+apar)/(1.+apar))

; T3*g3*g3 + T2*g3 + T1 = 0

sqrtarg1=(T21^2 - 4.*T31*T11)
sqrtarg2=(T22^2 - 4.*T32*T12)

;
; sometimes if the difference is small
;  numerical error may result in small negative number
;

if sqrtarg1 lt 0. then begin
 print,'setting negative sqrtarg1 to zero ',sqrtarg1
 sqrtarg1=0.d0
endif
if sqrtarg2 lt 0. then begin
 print,'setting negative sqrtarg2 to zero ',sqrtarg2
 sqrtarg2=0.d0
endif

g3crit1a=(-T21 + sqrt(sqrtarg1))/2./T31
g3crit1b=(-T21 - sqrt(sqrtarg1))/2./T31

g3crit2a=(-T22 + sqrt(sqrtarg2))/2./T32
g3crit2b=(-T22 - sqrt(sqrtarg2))/2./T32

;
; test for NaNs
;

;if g3crit1a*0. ne 0. or g3crit1b*0. ne 0. or g3crit2a*0. ne 0. or g3crit2b*0. ne 0 then begin
;  print,'NaNs'
  print,'rcrit1=',rcrit1
  print,'rcrit2=',rcrit2
  print,'g3crit1a=',g3crit1a
  print,'g3crit1b=',g3crit1b
  print,'g3crit2a=',g3crit2a
  print,'g3crit2b=',g3crit2b
;  stop
;endif

end
