	pro litescoords,rpB,thetapB,phipB

	common llshiftcoord,Rcap,Thcap,Phcap
	common llprams,xo,rbub,ao
	common llprams2,aa,bb,cc,dd,ee,ff,alnot,sig1,out

;
;  CALLED BY LITESLOW
;
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)

;  
;  there are so many coordinate transforms, most (all but
;  the transform from plane-of-sky coords to the physical pB integration
;  coords, which is done in intensint) will be done
;  in this procedure to keep them straight.
;
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
		xtr = rpB*sin(thetapB)*cos(phipB) - xo
		ytr = rpB*sin(thetapB)*sin(phipB) 
		ztr = rpB*cos(thetapB) 

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

;
		Phcap = atan(ytilde,xtr)
;
		test = where(thcap*0. ne 0 or phcap*0. ne 0 or rcap*0. ne 0.)
	        if min(tesT) ne -1 then stop
		end
