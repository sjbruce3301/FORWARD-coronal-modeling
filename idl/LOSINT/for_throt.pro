;************************************************************************

	pro for_throt,r3D,theta3D,phi3D,lat

;
;  Mame:  FOR_THROT
;
;  sets up new coordinate system
;  rotated an angle lat about Y axis
;
;  Arfkin p 199
;
; Called by FOR_DOROTS, FOR_VECT_THROT
;
; Transcribed Sarah Gibson 2012-2013
; Version 2.0 July 2014
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)


   	x = r3D*sin(theta3D)*cos(phi3D)
   	y = r3D*sin(theta3D)*sin(phi3D)
   	z = r3D*cos(theta3D)

        yp=y
        xp=x*cos(double(lat)) + z*sin(double(lat))
        zp=-x*sin(double(lat)) + z*cos(double(lat))

;
; fix rounding error
;
	rat=zp/r3D
        testpos = where(rat gt 1.)
        testneg = where(rat lt -1.)
	if min(testpos) ne -1 then rat[testpos]=1.
	if min(testneg) ne -1 then rat[testneg]=-1.
        
        theta3D=acos(rat)
        phi3D = atan(yp,xp)

	end
