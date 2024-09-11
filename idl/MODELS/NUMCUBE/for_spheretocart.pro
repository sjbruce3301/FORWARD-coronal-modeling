PRO FOR_SPHERETOCART, p1, p2, p3, v1, v2, v3

;+
;
; NAME: FOR_SPHERETOCART
;
; PURPOSE: transforms vector, from spherical to 
;          cartesian coordinates. transformation are done in place. 
;          vector field is optional
;
; CALLING SEQUENCE: 
;
;       FOR_SPHERETOCART, p1, p2, p3 [,v1, v2, v3]
;
; INPUTS: 
;
;        p1,p2,p3: 3-array position of the vector, pr,pth,pph
;
;        v1,v2,v3: 3-array of the vector, vr,vth,vph (optional)
;
;
; OUTPUTS: 
;
;        p1,p2,p3: position in cartesian coords, px,py,pz
;      
;        v1,v2,v3: vector in cartesian coords, vx,vy,vz (only comes
;                  back if vector is passed in). 
;
; Called by FOR_INTERP_CUBE
;
; Written by Laurel Rachmeler 2011
; Version 2.0 July 2014
;-

COMPILE_OPT IDL2 ;default long and square brackets for array subscripts



;**note on variable names:**
;
;p1,p2,p3 are the sperical positions, r,th,phi 
;p1c,p2c,p3c are the cartesian positions, x,y,z
;
;v1,v2,v3 are the spherical vectors, vr, vth, vph
;v1c,v2c,v3c are the cartesian vectors, vx, vy, vz

sth=SIN(double(p2))
cth=COS(double(p2))

sph=SIN(double(p3))
cph=COS(double(p3))

;**transform the positons**
p1c = p1 * sth * cph;x
p2c = p1 * sth * sph;y 
p3c = p1 * cth      ;z

;print, p1c,p2c,p3c

params=N_PARAMS() ;number of parameters this code was called with 
IF (params EQ 6) THEN  BEGIN
      ;**transform the vectors**
   v1c=v1 * sth * cph + $
       v2 * cth * cph + $
       v3 * (-1)*sph

   v2c=v1 * sth * sph + $
       v2 * cth * sph + $
       v3 * cph

   v3c=v1 * cth + $
       v2 * (-1)*sth

   ;**make the in-place variable changes for the vector**
   v1=v1c
   v2=v2c
   v3=v3c
ENDIF

;**make the in-place variable changes for the position**
p1=p1c
p2=p2c
p3=p3c

END
