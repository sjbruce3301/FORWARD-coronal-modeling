function gbl_diff_streamfunc, r, theta, bpar, norm1=norm1, norm3=norm3

; Name: GBL_DIFF_STREAMFUNC
;
; Purpose: take derivative of the stream function
;
; Inputs:    
;      rpB, thetapB, phipB -- position in 3D space where model is to be evaluated
;
; Keywords: NORM1, NORM3 -- allows calculating stream function
;	   derivatives for Low86 vs GBL conventions
;
; Outputs: Dstrfunc -- structure with derivatives
;
; Called by GIBBAGLOW
; Calls GBL_BASEFUNC, GBL_DIFF_BASEFUNC
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

;According to Low (1986) (B3) and (B4) 

Dsf1_u = r*(1. - v^2 )*(-2. + 2.*u*atan(1./u))  ;1st term
Dsf1_v = -2.*r*v*(-u + (1. + u^2 )*atan(1./u))                                       ;1st term

Dsf3_u =  0.75*r*(1.-v^2)*(5.*v^2-1.)*(-6.*u*(5.*u^2+1.)*atan(1./u) $
         -30.*(1.+u^2)*u*atan(1./u) + 3.*(5.*u^2+1.) + 45.*u^2 + 13.)
Dsf3_v =  0.75*r*(-2.*v*(5.*v^2-1.)+(1.-v^2)*10.*v)*(-3.*(1.+u^2)*(5.*u^2+1.)*atan(1./u)+15.*u^3+13.*u)

Dbasefunc= gbl_diff_basefunc(r,theta,bpar)
Du_r=Dbasefunc.Du_r
Dv_r=Dbasefunc.Dv_r
Du_th=Dbasefunc.Du_th
Dv_th=Dbasefunc.Dv_th
Deta_r=Dbasefunc.Deta_r
Deta_th=Dbasefunc.Deta_th

;coefficient norm1 eq 2./!pi/bpar/bpar

Dsf1_r = ((1.-v^2)*((1.+u^2)*atan(1./u)-u)) + Dsf1_u*Du_r + Dsf1_v*Dv_r
Dsf1_r = Dsf1_r + !pi*bpar*bpar*sin(theta)^2/r^2/2.
Dsf1_r = Dsf1_r + (2.*bpar)*Deta_r
Dsf1_r = norm1*Dsf1_r

Dsf1_th = Dsf1_u*Du_th + Dsf1_v*Dv_th
Dsf1_th = Dsf1_th + (-!pi*bpar^2*sin(theta)*cos(theta)/r)
Dsf1_th = Dsf1_th + (2.*bpar)*Deta_th
Dsf1_th = norm1*Dsf1_th

;coefficient norm3 eq -1./45./!pi/bpar/bpar/bpar/bpar

Dsf3_r = 0.75*((1.-v^2)*(5.*v^2-1.)*(-3.*(1.+u^2)*(5.*u^2+1.)*atan(1./u)+15.*u^3+13.*u)) + Dsf3_u*Du_r + Dsf3_v*Dv_r
Dsf3_r = Dsf3_r + (sin(theta))^2*(5.*(cos(theta))^2-1.)*(-3.)*(45.*!pi*bpar^4)/(8.*r^4)
Dsf3_r = Dsf3_r - (sin(theta))^2*(9.*!pi*bpar^2)/(2.*r^2)
Dsf3_r = Dsf3_r + 12.*bpar*Deta_r
Dsf3_r = norm3*Dsf3_r

Dsf3_th = Dsf3_u*Du_th + Dsf3_v*Dv_th
Dsf3_th = Dsf3_th + (45.*!pi*bpar^4)*2.*sin(theta)*cos(theta)*(5.*(cos(theta))^2-1.)/(8.*r^3)+ (45.*!pi*bpar^4)*(sin(theta))^2*(-10.*cos(theta)*sin(theta))/(8.*r^3)
Dsf3_th = Dsf3_th + (9.*!pi*bpar^2)*sin(theta)*cos(theta)/r
Dsf3_th = Dsf3_th + 12.*bpar*Deta_th
Dsf3_th = norm3*Dsf3_th

Dstrfunc={Dsf1_r:Dsf1_r, Dsf1_th:Dsf1_th, Dsf3_r:Dsf3_r, Dsf3_th:Dsf3_th}

return,Dstrfunc

end


