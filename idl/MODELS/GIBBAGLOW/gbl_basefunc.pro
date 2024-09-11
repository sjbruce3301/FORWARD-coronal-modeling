function gbl_basefunc, r, theta, bpar

; Name: GBL_BASEFUNC
;
; Purpose: calculate the coordinates, eta, u, v
;  	and some functions of them
;
; Inputs:
;      rpB, thetapB, phipB -- position in 3D space where model is to be evaluated
;
; Outputs: Basefunc -- structure with the coordinates and fun ctions
;
; Called by GBL_STREAMFUNC, GBL_DIFF_STREAMFUNC, GBL_DIFF_DIFF_STREAMFUNC
;
; Written: Jie Zhao and S. Gibson 
;	June 2018
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;--------------------------------------------------------------------------

sign=1.
; spheroidal coordinate eta
; Eq.(A4) GBL
; bpar is constant coefficient which can be refered to Table1

frb=(r^2)/(bpar^2) - 1.
frc=2.*r*COS(theta)/bpar

eta=sign*sqrt(-0.5*frb + 0.5*sqrt(frb^2+frc^2))

; inverted spheroidal coordinates u and v
; Eq.(2) GBL

frb2=(bpar^2)/(r^2) - 1.
frc2= 2.*bpar*COS(theta)/r


u=sign*sqrt(0.5*frb2 + 0.5*sqrt(frb2^2+frc2^2))

v=sqrt(-0.5*frb2 + 0.5*sqrt(frb2^2+frc2^2))

test=where(v*0. ne 0.)
if min(test) ne -1 then v[test] = 0.
test=where(eta*0. ne 0.)
if min(test) ne -1 then eta[test] = 0.

Basefunc={frb:frb,frc:frc,frb2:frb2,frc2:frc2,u:u,v:v,eta:eta}

return,Basefunc
end
