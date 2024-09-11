pro for_uvangle,r3D,theta3D,phi3D,Br,Bth,Bph,thetaB,chiB,bigtheta,bigchi

;
;  Name:  FOR_UVANGLE
;
;  rotates spherical vector into local vertical Hanle coordinates
;
;REQUIRED INPUTS
;
;  R3D,THETA3D,PHI3D -- position in spherical coordinates
;       Phi3D measured clockwise from Sun-observer line (x axis)
;       Theta3D measured clockwise from plane-of-sky vertical (z axis)
;	IN RADIANS
;
;  BR, BTH, BPH -- magnetic field vector in spherical coordinates
;	in observer's frame of reference
;	IN GAUSS
;
;OUTPUTS
;       THETAB, CHIB:       polar and azimuthal angles of magnetic vector,
;               IN RADIANS
;       BIGTHETA: polar angle of the line of sight
;	BIGCHI: 0 for us ( angle between the plane containing the 
;		line of sight and plane perpendicular to the plane of the sky)
;
; Called by FOR_UVMODEL

;
; Written Sarah Gibson, Silvano Fineschi, Roberto Susino 2017
;	Based on notes from Roberto Casini

        st=sin(theta3D)
        ct=cos(theta3D)
        sp=sin(phi3D)
        cp=cos(phi3D)

        Bx= Br*st*cp + Bth*ct*cp - Bph*sp
        By = Br*st*sp + Bth*ct*sp + Bph*cp
        Bz = Br*ct - Bth*st

        x = r3D*sin(theta3D)*cos(phi3D)
        y = r3D*sin(theta3D)*sin(phi3D)
        z = r3D*cos(theta3D)

	alpha=-1.d0*atan(x,sqrt(y^2+z^2))
 	beta=atan(y,z)

        Bxp=cos(double(alpha))*Bx  + sin(double(alpha))*sin(double(beta))*By + $
		sin(double(alpha))*cos(double(beta))*Bz
        Byp= cos(double(beta))*By - sin(double(beta))*Bz
        Bzp=-1.d0*sin(double(alpha))*Bx  + cos(double(alpha))*sin(double(beta))*By + $
		cos(double(alpha))*cos(double(beta))*Bz

;
; write in polar coordinates
;
	thetaB=atan(sqrt(Bxp*Bxp+Byp*Byp),Bzp)
	chiB=atan(Byp,Bxp)
	
	bigtheta=!dpi/2.+alpha
  	bigchi=0.*bigtheta

end
