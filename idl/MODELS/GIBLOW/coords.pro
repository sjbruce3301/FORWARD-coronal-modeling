	pro coords,rpB,thetapB,phipB

	common gltranscoord,rlam
	common glshiftcoord,Rcap,Thcap,Phcap
	common glsscoord,rsquig,phiss
	common glprams,apar,xo,rbub,ao,Pio,alpha,eta
	common glprams2,aa,bb,cc,dd,ee,ff,alnot,sig1,out

;
;  CALLED BY GIBLOW
;
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;	Jul 2023 -- commented out if NaN stop -- because it can happen
;		for ok reasons-- namely, a USER request for pB
;		and the modsolstruct is evaluated assuming
;		losuse='tau', which evalutates at the thomson sphere
;		and some points in front of the disk could have the thompson
;		sphere inside r=1. Better to pass trhough the nans, which
;		forward can deal with

;  
;  there are so many coordinate transforms, most (all but
;  the transform from plane-of-sky coords to the physical pB integration
;  coords, which is done in intensint) will be done
;  in this procedure to keep them straight.
;
;  the self-similar transformation:

		rsquig = rpB/phiss
;
;  the radial coordinate is now transformed introducing expansion of
;  the field
;

		rlam = rsquig + apar

;  The force balance equations will actually
;  be solved in yet another system, this one with a shifted center,
;  This shifted symmetry introduces asymmetry.
;  We must first move to Cartesian coordinates.
;
;  note I have changed the coordinates to a right-hand system
;  (finally)
;  the shift xo is along the x axis, so now one needs to set 
;  central meridian to -90 to see the bubble at the West limb
;
		xtr = rlam*sin(thetapB)*cos(phipB) - xo
		ytr = rlam*sin(thetapB)*sin(phipB) 
		ztr = rlam*cos(thetapB) 

; now transform to spherical coords

		Rcap = sqrt(xtr*xtr + ytr*ytr + ztr*ztr)
;
; we want to rotate the coordinates in the bubble 
;  about the x-axis by an angle sig1 (parameter)
;
		sigma = sig1
;
; note the counterclockwise rotation
; this is necessary to move into the bubble coordinates
; which are rotated sigma clockwise from the physical coordinates
;

		ytilde = ytr*cos(sigma) - ztr*sin(sigma)
		ztilde = ztr*cos(sigma) + ytr*sin(sigma)
;
		rat = (ztilde/Rcap)
                testpos = where(rat gt 1.)
                testneg = where(rat lt -1.)
                if min(testpos) ne -1 then rat[testpos] = 1.
                if min(testneg) ne -1 then rat[testneg] = -1.

		Thcap = acos(rat)
;
		Phcap = atan(ytilde,xtr)
;
;		test = where(thcap*0. ne 0 or phcap*0. ne 0 or rcap*0. ne 0.)
;	        if min(tesT) ne -1 then stop
		end
