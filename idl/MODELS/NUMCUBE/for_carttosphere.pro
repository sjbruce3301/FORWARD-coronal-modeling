PRO FOR_CARTTOSPHERE, p1, p2, p3, v1, v2, v3

;+
;
; NAME: FOR_CARTTOSPHERE
;
; PURPOSE: transforms vector and position from cartesian to 
;          spherical coordinates. transformation occur in
;          place. vector is optional.
;
; CALLING SEQUENCE: 
;
;       FOR_CARTTOSPHERE, p1, p2, p3 [,v1, v2, v3]
;
; INPUTS: 
;
;        p1,p2,p3: position of the vector, px,py,pz
;
;        v1,v2,v3: 3-array of the vector, vx,y,vz (optional)
;
;
; OUTPUTS: 
;
;        p1,p2,p3: position in spherical coords, pr,pth,pph
;      
;        v1,v2,v3: vector in spherical coords, vr,vth,vph, only comes
;                  back if a vector was passed in
;
; Called by FOR_INTERP_CUBE
;
; Written Laurel Rachmeler 2011
; Version 2.0 July 2014
;-

COMPILE_OPT IDL2 ;default long and square brackets for array subscripts

;**note on variable names:**
;
;p1,p2,p3 are the cartesian positions, x,y,z
;p1s,p2s,p3s are the sperical positions, r,th,phi
;
;v1,v2,v3 are the cartesian vectors, vx, vy, vz
;v1s,v2s,v3s are the sperical vectors, vr, vth, vph

;**transform the positions**
p1s=SQRT(p1^2 + p2^2 + p3^2)  ;r
p2s=ACOS(p3/p1s);acos(z,r) th should be 0-pi
p3s=ATAN(p2,p1) ;atan(y,x)    ;phi -pi to pi

;IF (TOTAL(p3s[*] LT 0)) THEN p3s=p3s + (p3s[*] LT 0)*2.d0*!DPI         ;0 to 2pi
;IF (TOTAL(p3s[*] GT 2.d0*!DPI)) THEN p3s=p3s - (p3s[*] GT 2.d0*!DPI)*2.d0*!DPI     ;0 to 2pi

;**make the in-place variable changes for the position**
p1=p1s
p2=p2s
p3=p3s

sth=SIN(p2)
cth=COS(p2)

sph=SIN(p3)
cph=COS(p3)

params=N_PARAMS() ;number of parameters this code was called with 
IF (params EQ 6) THEN  BEGIN
   ;**transform the vectors**
   v1s=v1 * sth * cph + $
       v2 * sth * sph + $
       v3 * cth

   v2s=v1 * cth * cph + $
       v2 * cth * sph + $
       v3 * (-1.)*sth

   v3s=v1 * (-1.)*sph + $
       v2 * cph
   
   ;**make the in-place variable changes for the vector**
   v1=v1s
   v2=v2s
   v3=v3s

ENDIF

END
