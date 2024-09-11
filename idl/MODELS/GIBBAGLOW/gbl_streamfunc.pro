function gbl_streamfunc,r,theta,bpar,norm1=norm1,norm3=norm3

; Name: GBL_STREAMFUNC
;
; Purpose: calculate the stream function
;
; Inputs:
;      rpB, thetapB, phipB -- position in 3D space where model is to be evaluated
;
; Keywords: NORM1, NORM3 -- allows calculating stream function
;          derivatives for Low86 vs GBL conventions
;
; Outputs: Strfunc -- structure with Stream functions for dipole and octopole
;
; Called by GIBBAGLOW
; Calls GBL_BASEFUNC
;
; Written: Jie Zhao and S. Gibson
;       June 2018
;--------------------------------------------------------------------------

Basefunc=gbl_basefunc(r,theta,bpar)

frb=Basefunc.frb
frc=Basefunc.frc
frb2=Basefunc.frb2
frc2=Basefunc.frc2
u=Basefunc.u
v=Basefunc.v
eta=Basefunc.eta

; Eq.(A3)

Ufunc = eta

test=where(theta le !pi/2.)
if min(test) ne -1 then Ufunc[test]=-1.*Ufunc[test]


; dipole streamfunction
; Eq.(A1)


strfunc1 = (r*(1.-v^2)*((1.+u^2)*ATAN(1./u)-u))
strfunc1 = strfunc1 -!pi*bpar*bpar*(SIN(theta))^2/r/2.
 
; Gibson, Bagenal and Low 1996 says
;
; strfunc1 = strfunc1 + (2./(!pi*bpar))*Ufunc
;
;  I believe there was an error-- 
; instead of Ufunc it should be eta, and  it should be 4./(!pi*bpar)
; since its normalized by 2./(!pi*bpar^2)
; we should use the equation as described in Low, 1986

strfunc1 = strfunc1 + 2*bpar*eta
strfunc1 = norm1*strfunc1

; octopole streamfunction
; Eq.(A2)

strfunc3 = 0.75*$
	(r*(1.-v^2)*(5.*v^2-1)*(-3.*(1.+u^2)*(5*u^2+1.)*ATAN(1./u)+15.*u^3+13.*u))
strfunc3 = strfunc3 + 45.*!pi*(bpar^4)*(SIN(theta))^2*(5.*(COS(theta))^2-1.)/(8.*r^3)
strfunc3 = strfunc3 + 9.*!pi*(bpar^2)*(SIN(theta))^2/(2.*r)
;
; as above this is from Gibson, Bagenal and Low 1996
; strfunc3 = strfunc3 - 12.*Ufunc/(45*!pi*bpar^3)
;
; however there is an error -- instead of Ufunc this should be eta
;
; using Low 1986

strfunc3 = strfunc3 + 12.*eta*bpar
strfunc3 = norm3*strfunc3

;print,norm1,norm3
Streamfunc={strfunc1:strfunc1,strfunc3:strfunc3}

return,Streamfunc

end
