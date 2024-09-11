;+
;
; NAME:
; croissant_rot_3d
;
; PURPOSE:
;
; Rotates input x,y,z coordinates in 3 dimensions. User specifies angles of rotation around z axis (x-y plane), 
; y axis (x-z) plane, and x axis (y-z) plane. Order of rotation is first around z, then y, then x axis. 
; Default anti-clockwise rotation as viewed down the axis of rotation. Vector, or clockwise, keyword rotates
; clockwise.
;
; CALLING SEQUENCE:
; croissant_rot_3d,rotxy,rotxz,xin,yin,zin,xout,yout,zout[,vector=vector,degree=degree,clockwise=clockwise]
;
; INPUTS:
;   rotxy: angle of rotation around z axis
;
;   rotxz: angle of rotation around y axis
;   
;   rotyz: angle of rotation around x axis
;
;   x,y,z: input x,y,and z coordinates. Can be scalars or arrays, but same size OR one or more of these can be a single value.
;
; OUTPUTS:
;   xout,yout,zout: output rotated x,y and z coordinates
;
; OPTIONAL KEYWORD
;   vector: Flag, if set, rotates the point rather than the coordinates. Default is to rotate the coordinate system.
;           See https://mathworld.wolfram.com/RotationMatrix.html
;
;   degree: Flag, if set, then rot angles are supplied in degrees. Default radians
;
;   Clockwise: Flag, if set, then rotations are made in a clockwise sense (when looking down at that plane along the relevant axis).
;               Default is anti-clockwise.
;
; OPTIONAL INPUTS
;   None

;
; OPTIONAL OUTPUTS
;   None

;
; PROCEDURE:
; Order of rotation is first around z axis, then around y axis, then around x axis. Take care with this: 
; call this routine several times, rotating by one angle at a time in the required order.
;
; USE & PERMISSIONS
; Any problems/queries, or suggestions for improvements, please email Huw Morgan, hmorgan@aber.ac.uk
;
; ACKNOWLEDGMENTS:
;  This code was developed with the financial support of:
;  STFC Consolidated grant to Aberystwyth University (Morgan)
;
; MODIFICATION HISTORY:
; Created at Aberystwyth University - Huw Morgan hmorgan@aber.ac.uk
; First public availability through Solarsoft Forward package 08/2020
;
;-


pro croissant_rot_3d,rotxy0,rotxz0,rotyz0,x,y,z,xn,yn,zn,vector=vector,degree=degree,clockwise=clockwise

  if rotxy0 ne 0 then begin
    rotxy=keyword_set(degree)?rotxy0*!dtor:rotxy0
    rotxy=keyword_set(vector)?-rotxy:rotxy
    rotxy=keyword_set(clockwise)?-rotxy:rotxy
  
    xn=x*cos(rotxy)+y*sin(rotxy)
    yn=-x*sin(rotxy)+y*cos(rotxy)
  endif else begin
    xn=x
    yn=y
  endelse

  if rotxz0 ne 0 then begin
    rotxz=keyword_set(degree)?rotxz0*!dtor:rotxz0
    rotxz=keyword_set(vector)?-rotxz:rotxz
    rotxz=keyword_set(clockwise)?-rotxz:rotxz
    
    xn1=xn*cos(rotxz)+z*sin(rotxz)
    zn=-xn*sin(rotxz)+z*cos(rotxz)
    xn=xn1
  endif else begin
    zn=z
  endelse

  if rotyz0 ne 0 then begin
    rotyz=keyword_set(degree)?rotyz0*!dtor:rotyz0
    rotyz=keyword_set(vector)?-rotyz:rotyz
    rotyz=keyword_set(clockwise)?-rotyz:rotyz
    
    yn1=yn*cos(rotyz)+zn*sin(rotyz)
    zn=-yn*sin(rotyz)+zn*cos(rotyz)
    yn=yn1
  endif
  
end
