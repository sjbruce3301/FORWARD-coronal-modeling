pro for_fixangle,r3D,theta3D,phi3D,Br,Bth,Bph,thetaB,chiB,bigtheta

; Goal of this code is to from FORWARD coordinate
; to local Hanle coordinate 
;
; Inputs: r3D, theta3D, phi3D, Br, Bth, Bph
;        Position vector and magnetic vector
;        in FORWARD spherical (global) geometry
;
; Outputs: thetaB, chiB, bigtheta
;	polar angles of magnetic vector
;	in rotated Hanle frame:
;
;   THETAB -- angle between local vertical and the magnetic vector 
;	(polar angle of the magnetic vector in the Hanle frame)
;       For a given point along the line of sight, there will be a "local vertical" 
;	which is just the radial line from that point to the center of the sun 
;	(z' in Silvano's figure, rpB in FORWARD coordinates)
;
;   CHIB -- azimuthal angle of the magnetic vector in the Hanle frame
;
;   BIGTHETA - polar angle of the line of sight in the Hanle frame
;		or, the angle between the line of sight and local vertical (Z')
;		identical to pi/2 + alpha (Casini angle defined below)
;   

;
; convert to Cartesian
;

x = r3D*sin(theta3D)*cos(phi3D)
y = r3D*sin(theta3D)*sin(phi3D)
z = r3D*cos(theta3D)

st=sin(theta3D)
ct=cos(theta3D)
sp=sin(phi3D)
cp=cos(phi3D)

Bx = Br*st*cp + Bth*ct*cp - Bph*sp
By = Br*st*sp + Bth*ct*sp + Bph*cp
Bz = Br*ct - Bth*st

;
; from Casini notes 
; rotate clockwise in both alpha and beta
; to go into a local (Hanle) geometry
;

alpha = -atan(x,sqrt(y^2+z^2))
beta = atan(y,z)

bigtheta=!dpi/2.d0 + alpha

;
; express magnetic vector 
; in rotated Hanle frame
;

Bxp=Bx*cos(alpha) + By*sin(alpha)*sin(beta) + Bz*sin(alpha)*cos(beta)
Byp=By*cos(beta) - Bz*sin(beta)
Bzp=-Bx*sin(alpha)+By*cos(alpha)*sin(beta)+Bz*cos(alpha)*cos(beta)

;
; go from Cartesian to polar angles
; of B in Hanle frame
;

thetaB=atan(sqrt(Bxp^2+Byp^2),Bzp)
chiB=atan(Byp,Bxp)



; stuff to maybe test is below


;xp=x*cos(alpha) + y*sin(alpha)*sin(beta) + z*sin(alpha)*cos(beta)
;yp=y*cos(beta) - z*sin(beta)
;zp=-x*sin(alpha)+y*cos(alpha)*sin(beta)+z*cos(alpha)*cos(beta)

; 
; xp and yp should be zero
; zp should be r3D
; CHECK THIS
;

;thp=acos(zp/r3D)
;php = atan(yp,xp)

; in this system, thp=0.,php=?

;stp=sin(thp)
;ctp=cos(thp)
;stp=0
;ctp=1.
;spp=sin(php)
;cpp=cos(php)


;Brp = Byp*stp*spp + Bxp*stp*cpp + Bzp*ctp
;Bthp = Byp*ctp*spp + Bxp*ctp*cpp - Bzp*stp
;Bphp = -Bxp*spp + Byp*cpp


end
