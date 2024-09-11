function gbl_diffdiff_streamfunc, r, theta, bpar, norm1=norm1,norm3=norm3

; Name: GBL_DIFFDIFF_STREAMFUNC
;
; Purpose: take second derivatives of the stream function
;
; Inputs:
;      rpB, thetapB, phipB -- position in 3D space where model is to be evaluated
;
; Keywords: NORM1, NORM3 -- allows calculating stream function
;          derivatives for Low86 vs GBL conventions
;
; Outputs: DDstrfunc -- structure with derivatives
;
; Called by GIBBAGLOW
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

Dbasefunc=gbl_diff_basefunc(r,theta,bpar)
Du_r=Dbasefunc.Du_r
Dv_r=Dbasefunc.Dv_r
Du_th=Dbasefunc.Du_th
Dv_th=Dbasefunc.Dv_th
DDu_rr=Dbasefunc.DDu_rr
DDv_rr=Dbasefunc.DDv_rr
DDeta_rr=Dbasefunc.DDeta_rr
DDu_thr=Dbasefunc.DDu_thr
DDv_thr=Dbasefunc.DDv_thr
DDeta_thr=Dbasefunc.DDeta_thr
DDu_thth=Dbasefunc.DDu_thth
DDv_thth=Dbasefunc.DDv_thth
DDeta_thth=Dbasefunc.DDeta_thth

; According to Low (1986) (B3) and (B4)
Dsf1_u = r*(1. - v^2 )*(-2. + 2.*u*atan(1./u))  ;1st term
DDsf1_ur =   (1. - v^2 )*(-2. + 2.*u*atan(1./u)) $
           + (r*(1. - v^2 )*(2.*atan(1./u)-2.*u/(u^2+1.))) * Du_r $
           -  2.*r*v*(-2. + 2.*u*atan(1./u)) * Dv_r
DDsf1_uth =   (r*(1. - v^2 )*(2.*atan(1./u)-2.*u/(u^2+1.))) * Du_th $
           -  2.*r*v*(-2. + 2.*u*atan(1./u)) * Dv_th
;==========================================================
Dsf1_v = -2.*r*v*(-u + (1. + u^2 )*atan(1./u))                                       ;1st term
DDsf1_vr=     -2.*v*(-u + (1. + u^2)*atan(1./u)) $
              -2.*r*v*(-1. - 1. + 2.*u*atan(1./u))* Du_r $
              -2.*r*(-u + (1. + u^2)*atan(1./u)) * Dv_r
DDsf1_vth=    -2.*r*v*(-1. - 1. + 2.*u*atan(1./u))* Du_th $
              -2.*r*(-u + (1. + u^2)*atan(1./u)) * Dv_th
;==========================================================
Dsf3_u =  0.75*r*(1.-v^2)*(5.*v^2-1)*(-3.*2.*u*(5.*u^2+1.)*atan(1./u)-3.*(1.+u^2)*10.*u*atan(1./u) $
         -3.*(1.+u^2)*(5.*u^2+1.)*(-1./(u^2+1.))+45.*u^2+13.)
DDsf3_ur =   0.75*(1.-v^2)*(5.*v^2-1.)*(-3.*2.*u*(5.*u^2+1.)*atan(1./u)-3.*(1.+u^2)*10.*u*atan(1./u) $
            -3.*(1.+u^2)*(5.*u^2+1.)*(-1./(u^2+1.))+15.*u^2+5.)$
            +(0.75* r *(1. - v^2 ) *(-1 + 5. *v^2 )*(120. *u +  30. *(1. + u^2 )/((1. + 1./u^2)* u) $ 
            +6.*(1. + 5.* u^2 )/((1. + 1./u^2)* u) -120.*u^2*atan(1./u)-30.* (1. + u^2 ) *atan(1./u) - 6. *(1. + 5. *u^2 ) *atan(1./u))) *Du_r $
            +((7.5*r* v *(1. - v^2 )* (13. + 45. *u^2  + 3.* (1. + 5.* u^2 ) - 30.* u* (1. + u^2 )*atan(1./u)-6.* u* (1. + 5. *u^2 )*atan(1./u))) $
            +(-1.5*r*v*(-1.+5.*v^2)*(13. + 45. *u^2  + 3.* (1. + 5.* u^2 ) - 30.* u* (1. + u^2 )*atan(1./u)-6.* u* (1. + 5. *u^2 )*atan(1./u))))*Dv_r
DDsf3_uth =  (0.75* r *(1. - v^2 ) *(-1 + 5. *v^2 )*(120. *u +  30. *(1. + u^2 )/((1. + 1./u^2)* u) $ 
            +6.*(1. + 5.* u^2 )/((1. + 1./u^2)* u) -120.*u^2*atan(1./u)-30.* (1. + u^2 ) *atan(1./u) - 6. *(1. + 5. *u^2 ) *atan(1./u))) *Du_th $
            +((7.5*r* v *(1. - v^2 )* (13. + 45. *u^2  + 3.* (1. + 5.* u^2 ) - 30.* u* (1. + u^2 )*atan(1./u)-6.* u* (1. + 5. *u^2 )*atan(1./u))) $
            +(-1.5*r*v*(-1.+5.*v^2)*(13. + 45. *u^2  + 3.* (1. + 5.* u^2 ) - 30.* u* (1. + u^2 )*atan(1./u)-6.* u* (1. + 5. *u^2 )*atan(1./u))))*Dv_th
;=========================================================
Dsf3_v = 0.75*r*((-2.*v)*(5.*v^2-1.)+(1.-v^2)*10.*v)*(-3.*(1.+u^2)*(5.*u^2+1.)*atan(1./u)+15.*u^3+13.*u)
DDsf3_vr =  0.75*((-2.*v)*(5.*v^2-1.)+(1.-v^2)*10.*v)*(-3.*(1.+u^2)*(5.*u^2+1.)*atan(1./u)+15.*u^3+13.*u) $
          + 0.75*r*((-2.*v)*(5.*v^2-1)+(1.-v^2)*10.*v)* $
          (-6.*u*(5.*u^2+1.)*atan(1./u)-3.*(1.+u^2)*10.*u*atan(1./u)-3.*(1.+u^2)*(5.*u^2+1.)*(-1./(u^2+1.))+45.*u^2+13.)* Du_r $
          + 0.75*r*(-2.*(5.*v^2-1.)+(-2.*v)*10.*v+(-2.*v)*10.*v+(1.-v^2)*10.)*(-3.*(1.+u^2)*(5.*u^2+1.)*atan(1./u)+15.*u^3+13.*u)* Dv_r
DDsf3_vth =  0.75*r*((-2.*v)*(5.*v^2-1)+(1.-v^2)*10.*v)* $
          (-6.*u*(5.*u^2+1.)*atan(1./u)-3.*(1.+u^2)*10.*u*atan(1./u)-3.*(1.+u^2)*(5.*u^2+1.)*(-1./(u^2+1.))+45.*u^2+13.)* Du_th $
          + 0.75*r*(-2.*(5.*v^2-1.)+(-2.*v)*10.*v+(-2.*v)*10.*v+(1.-v^2)*10.)*(-3.*(1.+u^2)*(5.*u^2+1.)*atan(1./u)+15.*u^3+13.*u)* Dv_th
;=============================================================
;coefficient norm1 eq 2./!pi/bpar/bpar
DDsf1_rr =  (1. - v^2)*(-2. + 2.*u*atan(1./u)) *Du_r $
          - 2.*v*(-u + (1. + u^2)*atan(1./u)) *Dv_r $
          + DDsf1_ur*Du_r + Dsf1_u*DDu_rr + DDsf1_vr*Dv_r +Dsf1_v*DDv_rr 
DDsf1_rr =  DDsf1_rr - (!pi*bpar^2*sin(theta)^2/r^3) 
DDsf1_rr =  DDsf1_rr + (2.*bpar)*DDeta_rr
DDsf1_rr =  norm1*DDsf1_rr

DDsf1_thr = DDsf1_ur*Du_th + Dsf1_u*DDu_thr +DDsf1_vr*Dv_th + Dsf1_v*DDv_thr 
DDsf1_thr = DDsf1_thr + !pi*bpar^2*sin(theta)*cos(theta)/r^2 
DDsf1_thr = DDsf1_thr + (2.*bpar)*DDeta_thr
DDsf1_thr = norm1*DDsf1_thr

DDsf1_thth = DDsf1_uth*Du_th + Dsf1_u*DDu_thth +DDsf1_vth*Dv_th + Dsf1_v*DDv_thth
DDsf1_thth = DDsf1_thth + (-!pi*bpar^2*((cos(theta))^2-(sin(theta))^2)/r)
DDsf1_thth = DDsf1_thth + (2.*bpar)*DDeta_thth
DDsf1_thth = norm1*DDsf1_thth

;coefficient norm3 eq -1./45./!pi/bpar/bpar/bpar/bpar
DDsf3_rr =  0.75*(1.-v^2)*(5.*v^2-1.)*$
          (-6.*u*(5.*u^2+1.)*atan(1./u)-3.*(1.+u^2)*10.*u*atan(1./u)-3.*(1.+u^2)*(5.*u^2+1.)*(-1./(u^2+1.))+45.*u^2+13.)*Du_r $
           +0.75*(-2.*v*(5.*v^2-1.)+(1.-v^2)*10.*v)*(-3.*(1.+u^2)*(5.*u^2+1.)*atan(1./u)+15.*u^3+13.*u)*Dv_r $
           + DDsf3_ur*Du_r +Dsf3_u*DDu_rr + DDsf3_vr*Dv_r + Dsf3_v*DDv_rr 
           
DDsf3_rr = DDsf3_rr + (sin(theta))^2*(5.*(cos(theta))^2-1.)*3.*(45.*!pi*bpar^4)/(2.*r^5)
DDsf3_rr = DDsf3_rr + (sin(theta))^2*(9.*!pi*bpar^2)/r^3
DDsf3_rr = DDsf3_rr + 12.*bpar*DDeta_rr
DDsf3_rr = norm3*DDsf3_rr

DDsf3_thr = DDsf3_ur*Du_th + Dsf3_u*DDu_thr + DDsf3_vr*Dv_th + Dsf3_v*DDv_thr
DDsf3_thr = DDsf3_thr + (45.*!pi*bpar^4)*2.*sin(theta)*cos(theta)*(5.*(cos(theta))^2-1.)*(-3.)/(8.*r^4)$
           + (45.*!pi*bpar^4)*(sin(theta))^2*(-10.*cos(theta)*sin(theta))*(-3.)/(8.*r^4)
DDsf3_thr = DDsf3_thr - (9.*!pi*bpar^2)*sin(theta)*cos(theta)/r^2
DDsf3_thr = DDsf3_thr + 12.*bpar*DDeta_thr
DDsf3_thr = norm3*DDsf3_thr

DDsf3_thth = DDsf3_uth*Du_th + Dsf3_u*DDu_thth + DDsf3_vth*Dv_th + Dsf3_v*DDv_thth
DDsf3_thth = DDsf3_thth + (45.*!pi*bpar^4)*$
                          ((cos(theta))^2*(5.*(cos(theta))^2-1.)-(sin(theta))^2*(5.*(cos(theta))^2-1.)-10.*(sin(theta)*cos(theta))^2)/(4.*r^3)$
                          + (45.*!pi*bpar^4)*(10.*(sin(theta))^4-30.*(cos(theta))^2*(sin(theta))^2)/(8.*r^3)
DDsf3_thth = DDsf3_thth + (9.*!pi*bpar^2)*((cos(theta))^2-(sin(theta))^2)/r
DDsf3_thth = DDsf3_thth + 12.*bpar*DDeta_thth
DDsf3_thth = norm3*DDsf3_thth


;===============================================================================

DDstrfunc={DDsf1_rr:DDsf1_rr,DDsf1_thr:DDsf1_thr,DDsf1_thth:DDsf1_thth,$
DDsf3_rr:DDsf3_rr,DDsf3_thr:DDsf3_thr,DDsf3_thth:DDsf3_thth}
return,DDstrfunc

end
