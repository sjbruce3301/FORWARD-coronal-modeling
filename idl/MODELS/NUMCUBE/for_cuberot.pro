PRO FOR_CUBEROT, cuberot, px, py, pz, vx, vy, vz

;+
;
; NAME: FOR_CUBEROT
;
; PURPOSE: rotates a cartesian vector (vx,vy,vz) at a given position,
;          (px,py,pz) couterclockwise about the x-axis at an angle 
;          of cuberot. rotates the vector in place. Can be used with just
;          a position, or a position and a vector.
;
; CALLING SEQUENCE: 
;
;       FOR_CUBEROT, cuberot, px, py, pz, [,vx, vy, vz]
;
; INPUTS: 
;
;        px,py,pz: scalar position of the vector
;
;        vx,vy,vz: magnitudes of the vector in the x, y, and z
;                  direction (optional)
;
;        cuberot: angle of rotation about the x-axis in radians,
;              counterclockwise!
;
; OUTPUTS: 
;      
;        rotated position and magnitude vectors
;
; Called by FOR_INTERP_CUBE
;
; Written Laurel Rachmeler 2011
; Version 2.0 July 2014
;-

COMPILE_OPT IDL2 ;default long and square brackets for array subscripts

cuberot=DOUBLE(cuberot)
cx=COS(cuberot)
sx=SIN(cuberot)

;rotation matrix about x-axis

;rotation matrix about x-axis, can't use because we don't know how
;many points are called at a time
;Rx=[[ 1.0,   0.0,           0.0     ],$
;    [ 0.0, COS(cuberot), -1.*SIN(cuberot) ],$
;    [ 0.0, SIN(cuberot), COS(cuberot)     ]  ]

;p1 = px ; cuberotation so px does not change
p2 = cx*py - sx*pz
p3 = sx*py + cx*pz

py=p2
pz=p3

params=N_PARAMS() ;number of parameters this code was called with 
IF (params EQ 7) THEN  BEGIN

  ;v1 = vx ;cuberotation, so vx does not change
   v2 = cx*vy - sx*vz
   v3 = sx*vy + cx*vz

   vy=v2
   vz=v3
   
ENDIF

END
