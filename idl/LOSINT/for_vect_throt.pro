;************************************************************************

	pro for_vect_throt,Br,Bth,Bph,r,th,ph,lat

;
;  Name:  FOR_vect_THROT
;
;  projects vector angular components
;  onto coordinate system
;  rotated an angle lat about Y axis
;
; Calls FOR_THROT
; Called by FOR_VECTUNROT
;
; Written Sarah Gibson 2012-2013
; Version 2.0 July 2014

	st=sin(th)
	ct=cos(th)
	sp=sin(ph)
	cp=cos(ph)

        Bx1= Br*st*cp + Bth*ct*cp - Bph*sp
        By = Br*st*sp + Bth*ct*sp + Bph*cp
        Bz1 = Br*ct - Bth*st

	Bx=cos(double(lat))*Bx1  + sin(double(lat))*Bz1
	Bz=-sin(double(lat))*Bx1  + cos(double(lat))*Bz1

        for_throt,r,th,ph,lat

	st2=sin(th)
	ct2=cos(th)
	sp2=sin(ph)
	cp2=cos(ph)

	Br = By*st2*sp2 + Bx*st2*cp2 + Bz*ct2
	Bth = By*ct2*sp2 + Bx*ct2*cp2 - Bz*st2
	Bph = -Bx*sp2 + By*cp2

	end
