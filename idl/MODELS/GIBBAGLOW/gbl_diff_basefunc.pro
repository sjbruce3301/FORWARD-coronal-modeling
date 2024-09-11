function gbl_diff_basefunc,r,theta,bpar

; Name: GBL_DIFF_BASEFUNC
;
; Purpose: take derivatives of base functions and coordinates
;
; Inputs:
;      rpB, thetapB, phipB -- position in 3D space where model is to be evaluated
;
; Outputs: Dbasefunc -- structure with the derivatives
;
; Calls GBL_BASEFUNC
; Called by GBL_DIFF_STREAMFUNC, GBL_DIFF_DIFF_STREAMFUNC
;
; Written: Jie Zhao and S. Gibson
;       June 2018
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;--------------------------------------------------------------------------

Basefunc=gbl_basefunc(r,theta,bpar)
u=Basefunc.u
v=Basefunc.v
eta=Basefunc.eta
frb=Basefunc.frb
frc=Basefunc.frc
frb2=Basefunc.frb2
frc2=Basefunc.frc2

Du_r= (-1.*bpar^2/r^3 + (0.25*(-4.*bpar^2*(-1. + bpar^2/r^2)/r^3 - 8.*bpar^2*cos(theta)^2/r^3))/sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2))$
        / (2.*sqrt(0.5*(-1. + bpar^2/r^2) + 0.5*sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2)))

Du_th=  (-1.*bpar^2*cos(theta)*sin(theta)) / (r^2*sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2)$ 
              *sqrt(0.5*(-1. + bpar^2/r^2) + 0.5*sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2)))

Dv_r= (1.*bpar^2/r^3 + (0.25*(-4.*bpar^2*(-1. + bpar^2/r^2)/r^3 - 8.*bpar^2*cos(theta)^2/r^3)) / sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2))$
        /  (2.*sqrt(-0.5*(-1. + bpar^2/r^2) + 0.5*sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2)))

Dv_th= (-1.*bpar^2*cos(theta)*sin(theta)) / (r^2*sqrt((-1. + bpar^2/r^2)^2 + 4.*bpar^2*cos(theta)^2/r^2)$
             *sqrt(-0.5*(-1.+ bpar^2/r^2) + 0.5*sqrt((-1. + bpar^2/r^2)^2 + 4.*bpar^2*cos(theta)^2/r^2)))

Deta_r=  (-1.*r/bpar^2 + 0.25*(4.*r*(-1. + r^2/bpar^2)/bpar^2 + 8.*r*cos(theta)^2/bpar^2)/(sqrt((-1. + r^2/bpar^2)^2  + 4.*r^2*cos(theta)^2/bpar^2)))$
           / (2.*sqrt(-0.5*(-1. + r^2/bpar^2) + 0.5*sqrt((-1. + r^2/bpar^2)^2  + 4.*r^2*cos(theta)^2/bpar^2)))

Deta_th= (-1.*r^2*cos(theta)*sin(theta))$ 
         /(bpar^2*sqrt((-1. + r^2/bpar^2)^2 + 4.*r^2*cos(theta)^2/bpar^2)*sqrt(-0.5*(-1.+ r^2/bpar^2) + 0.5*sqrt((-1. + r^2/bpar^2)^2+4.*r^2*cos(theta)^2/bpar^2)))

DDu_r_r= (-1.*(-1.*bpar^2/r^3 + 0.25*(-4*bpar^2*(-1. + bpar^2/r^2)/r^3 - 8.*bpar^2*cos(theta)^2/r^3)/sqrt((-1. + bpar^2/r^2)^2 + 4.*bpar^2*cos(theta)^2/r^2))^2$  
            / (4.*(0.5*(-1. + bpar^2/r^2) + 0.5*sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2))^(3./2.))$
            + (3.*bpar^2/r^4 - 0.125*(-4.*bpar^2*(-1. + bpar^2/r^2)/r^3 - 8.*bpar^2*cos(theta)^2/r^3)^2/((-1. + bpar^2/r^2)^2 $
            + 4.*bpar^2*cos(theta)^2/r^2)^(3./2.)$
            + 0.25*(8.*bpar^4/r^6 + 12.*bpar^2*(-1. + bpar^2/r^2)/r^4 + 24.*bpar^2*cos(theta)^2/r^4)/sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2))$
            / (2.*sqrt(0.5*(-1. + bpar^2/r^2) + 0.5*sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2))))

DDu_th_r=    ((0.5*bpar^2*cos(theta)*(-1.*bpar^2/r^3 + 0.25*(-4.*bpar^2*(-1. + bpar^2/r^2)/r^3 - 8.*bpar^2*cos(theta)^2/r^3)$
             /sqrt((-1.+ bpar^2/r^2)^2+4.*bpar^2*cos(theta)^2/r^2))*sin(theta))/(r^2*sqrt((-1. + bpar^2/r^2)^2 $
             + 4.*bpar^2*cos(theta)^2/r^2)*(0.5*(-1.+ bpar^2/r^2)$
             + 0.5*sqrt((-1.+ bpar^2/r^2)^2 + 4.*bpar^2*cos(theta)^2/r^2))^(3./2.))$
             + (0.5*bpar^2*cos(theta)*(-4.*bpar^2*(-1. + bpar^2/r^2)/r^3 - 8.*bpar^2*cos(theta)^2/r^3)*sin(theta)) $
             / (r^2*((-1.+ bpar^2/r^2)^2 + 4.*bpar^2*cos(theta)^2/r^2)^(3./2.)*sqrt(0.5*(-1. + bpar^2/r^2)$
             + 0.5*sqrt((-1. + bpar^2/r^2)^2 + 4.*bpar^2*cos(theta)^2/r^2)))$
             + (2.*bpar^2*cos(theta)*sin(theta)) / (r^3*sqrt((-1. + bpar^2/r^2)^2 + 4.*bpar^2*cos(theta)^2/r^2) $
             * sqrt(0.5*(-1. + bpar^2/r^2) + 0.5*sqrt((-1. + bpar^2/r^2)^2 + 4.*bpar^2*cos(theta)^2/r^2))))

DDu_th_th=  (-bpar^2*cos(theta)^2)/(r^2*sqrt((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)*sqrt(0.5*(-1+bpar^2/r^2)+0.5*sqrt((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)))$
            -(bpar^4*cos(theta)^2*sin(theta)^2)/(r^4*((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)*(0.5*(-1+bpar^2/r^2)+0.5*sqrt((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2))^(3./2.))$
            -(4.*bpar^4*cos(theta)^2*sin(theta)^2)/(r^4*((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)^(3./2.)*sqrt(0.5*(-1+bpar^2/r^2)+0.5*sqrt((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)))$
            +(bpar^2*sin(theta)^2)/(r^2*sqrt((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)*sqrt(0.5*(-1+bpar^2/r^2)+0.5*sqrt((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)))

DDv_r_r=    -(1.*bpar^2/r^3 + (0.25*(-4*bpar^2*(-1. + bpar^2/r^2)/r^3 - 8.*bpar^2*cos(theta)^2/r^3)) $
            / sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2))^2 $
            / (4.*(-0.5*(-1. + bpar^2/r^2) + 0.5*sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2))^(3./2.))$
            + (-3.*bpar^2/r^4 - (0.125*(-4.*bpar^2*(-1. + bpar^2/r^2)/r^3 - 8.*bpar^2*cos(theta)^2/r^3)^2 ) $
            / ((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2)^(3./2.) $
            + (0.25*(8.*bpar^4/r^6 + 12.*bpar^2*(-1. + bpar^2/r^2)/r^4 + 24.*bpar^2*cos(theta)^2/r^4)) $
            / sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2)) / (2.*sqrt(-0.5*(-1. + bpar^2/r^2) $
            + 0.5*sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2)))

DDv_th_r=   (0.5*bpar^2*cos(theta)*(1.*bpar^2/r^3 + 0.25*(-4.*bpar^2*(-1. + bpar^2/r^2)/r^3 - 8.*bpar^2*cos(theta)^2/r^3) $
            /sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2))*sin(theta))$
            /(r^2*sqrt((-1. + bpar^2/r^2)^2  + 4.*bpar^2*cos(theta)^2/r^2) $
            *(-0.5*(-1. + bpar^2/r^2) + 0.5*sqrt((-1. + bpar^2/r^2)^2 + 4.*bpar^2*cos(theta)^2/r^2))^(3./2.))$
            +(0.5*bpar^2*cos(theta)*(-4.*bpar^2*(-1. + bpar^2/r^2)/r^3 - 8.*bpar^2*cos(theta)^2/r^3)*sin(theta))$
            /(r^2*((-1. + bpar^2/r^2)^2 + 4.*bpar^2*cos(theta)^2/r^2)^(3./2.)    $
            *sqrt(-0.5*(-1. + bpar^2/r^2) + 0.5*sqrt((-1. + bpar^2/r^2)^2 + 4.*bpar^2*cos(theta)^2/r^2))) $
            +(2.*bpar^2*cos(theta)*sin(theta)) / (r^3*sqrt((-1. + bpar^2/r^2)^2 + 4.*bpar^2*cos(theta)^2/r^2)*sqrt(-0.5*(-1. + bpar^2/r^2) $
            +0.5*sqrt((-1. + bpar^2/r^2)^2 + 4.*bpar^2*cos(theta)^2/r^2)))

DDv_th_th= (-bpar^2*cos(theta)^2)/(r^2*sqrt((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)*sqrt(-0.5*(-1+bpar^2/r^2)+0.5*sqrt((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)))$
            -(bpar^4*cos(theta)^2*sin(theta)^2)/(r^4*((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)*(-0.5*(-1+bpar^2/r^2)+0.5*sqrt((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2))^(3./2.))$
            -(4.*bpar^4*cos(theta)^2*sin(theta)^2)/(r^4*((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)^(3./2.)*sqrt(-0.5*(-1+bpar^2/r^2)+0.5*sqrt((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)))$
            +(bpar^2*sin(theta)^2)/(r^2*sqrt((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)*sqrt(-0.5*(-1+bpar^2/r^2)+0.5*sqrt((-1+bpar^2/r^2)^2 + 4*bpar^2*cos(theta)^2/r^2)))

DDeta_r_r=   -(-1.*r/bpar^2 + 0.25*(4.*r*(-1. + r^2/bpar^2)/bpar^2 + 8.*r*cos(theta)^2/bpar^2)/sqrt((-1. + r^2/bpar^2)^2 + 4.*r^2*cos(theta)^2/bpar^2))^2 $
              / (4.*(-0.5*(-1. + r^2/bpar^2) + 0.5*sqrt((-1. + r^2/bpar^2)^2  + 4.*r^2*cos(theta)^2/bpar^2))^(3./2.)) $
              + $
              (-1./bpar^2 - 0.125*(4.*r*(-1. + r^2/bpar^2)/bpar^2 + 8.*r*cos(theta)^2/bpar^2)^2/((-1. + r^2/bpar^2)^2 + 4.*r^2*cos(theta)^2/bpar^2)^(3./2.)$
              +  0.25*(8.*r^2/bpar^4 + 4.*(-1. + r^2/bpar^2)/bpar^2 + 8.*cos(theta)^2/bpar^2)/sqrt((-1. + r^2/bpar^2)^2  + 4.*r^2*cos(theta)^2/bpar^2))     $
              / (2.*sqrt(-0.5*(-1. + r^2/bpar^2) + 0.5*sqrt((-1. + r^2/bpar^2)^2  + 4.*r^2*cos(theta)^2/bpar^2)))

DDeta_th_r=     (0.5*r^2*cos(theta)*(-1.*r/bpar^2 + 0.25*(4.*r*(-1. + r^2/bpar^2)/bpar^2 + 8.*r*cos(theta)^2/bpar^2)$
              / sqrt((-1. + r^2/bpar^2)^2  + 4.*r^2*cos(theta)^2/bpar^2))*sin(theta)) $
              / (bpar^2*sqrt((-1. + r^2/bpar^2)^2 + 4.*r^2*cos(theta)^2/bpar^2)*(-0.5*(-1. + r^2/bpar^2) $
              + 0.5*sqrt((-1. + r^2/bpar^2)^2  + 4.*r^2*cos(theta)^2/bpar^2))^(3./2.)) $
              + (0.5*r^2*cos(theta)*(4.*r*(-1. + r^2/bpar^2)/bpar^2 + 8.*r*cos(theta)^2/bpar^2)*sin(theta)) $
              / (bpar^2*((-1. + r^2/bpar^2)^2  + 4.*r^2*cos(theta)^2/bpar^2)^(3./2.)    $
              * sqrt(-0.5*(-1. + r^2/bpar^2) + 0.5*sqrt((-1. + r^2/bpar^2)^2  + 4.*r^2*cos(theta)^2/bpar^2))) $
              - (2.*r*cos(theta)*sin(theta)) / (bpar^2*sqrt((-1. + r^2/bpar^2)^2  + 4.*r^2*cos(theta)^2/bpar^2) $
              * sqrt(-0.5*(-1. + r^2/bpar^2) + 0.5*sqrt((-1. + r^2/bpar^2)^2  + 4.*r^2*cos(theta)^2/bpar^2)))

DDeta_th_th=(-r^2*cos(theta)^2)/(bpar^2*sqrt((-1+r^2/bpar^2)^2 + 4*r^2*cos(theta)^2/bpar^2)*sqrt(-0.5*(-1+r^2/bpar^2)+0.5*sqrt((-1+r^2/bpar^2)^2 + 4*r^2*cos(theta)^2/bpar^2)))$
            -(r^4*cos(theta)^2*sin(theta)^2)/(bpar^4*((-1+r^2/bpar^2)^2 + 4*r^2*cos(theta)^2/bpar^2)*(-0.5*(-1+r^2/bpar^2)+0.5*sqrt((-1+r^2/bpar^2)^2 + 4*r^2*cos(theta)^2/bpar^2))^(3./2.))$
            -(4.*r^4*cos(theta)^2*sin(theta)^2)/(bpar^4*((-1+r^2/bpar^2)^2 + 4*r^2*cos(theta)^2/bpar^2)^(3./2.)*sqrt(-0.5*(-1+r^2/bpar^2)+0.5*sqrt((-1+r^2/bpar^2)^2 + 4*r^2*cos(theta)^2/bpar^2)))$
            +(r^2*sin(theta)^2)/(bpar^2*sqrt((-1+r^2/bpar^2)^2 + 4*r^2*cos(theta)^2/bpar^2)*sqrt(-0.5*(-1+r^2/bpar^2)+0.5*sqrt((-1+r^2/bpar^2)^2 + 4*r^2*cos(theta)^2/bpar^2)))
;=======================================================
test=where(du_r*0. ne 0.)
if min(test) ne -1 then du_r[test]=0.
test=where(dv_r*0. ne 0.)
if min(test) ne -1 then dv_r[test]=0.
test=where(dv_th*0. ne 0.)
if min(test) ne -1 then dv_th[test]=0.
test=where(du_th*0. ne 0.)
if min(test) ne -1 then du_th[test]=0.
test=where(deta_th*0. ne 0.)
if min(test) ne -1 then deta_th[test]=0.
test=where(deta_r*0. ne 0.)
if min(test) ne -1 then deta_r[test]=0.
test=where(ddu_r_r*0. ne 0.)
if min(test) ne -1 then ddu_r_r[test]=0.
test=where(ddu_th_r*0. ne 0.)
if min(test) ne -1 then ddu_th_r[test]=0.
test=where(ddu_th_th*0. ne 0.)
if min(test) ne -1 then ddu_th_th[test]=0.
test=where(ddv_r_r*0. ne 0.)
if min(test) ne -1 then ddv_r_r[test]=0.
test=where(ddv_th_r*0. ne 0.)
if min(test) ne -1 then ddv_th_r[test]=0.
test=where(ddv_th_th*0. ne 0.)
if min(test) ne -1 then ddv_th_th[test]=0.
test=where(ddeta_r_r*0. ne 0.)
if min(test) ne -1 then ddeta_r_r[test]=0.
test=where(ddeta_th_r*0. ne 0.)
if min(test) ne -1 then ddeta_th_r[test]=0.
test=where(ddeta_th_th*0. ne 0.)
if min(test) ne -1 then ddeta_th_th[test]=0.
;=======================================================

Dbasefunc={Du_r:Du_r, Du_th:Du_th, Dv_r:Dv_r, Dv_th:Dv_th, Deta_r:Deta_r, Deta_th:Deta_th,$
 DDu_rr:DDu_r_r, DDu_thr:DDu_th_r, DDu_thth:DDu_th_th, DDv_rr:DDv_r_r, DDv_thr:DDv_th_r, DDv_thth:DDv_th_th, DDeta_rr:DDeta_r_r, DDeta_thr:DDeta_th_r, DDeta_thth:DDeta_th_th}
return, Dbasefunc

end
