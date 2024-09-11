	pro litesgetoutside,rtemp,thtemp,phtemp

; 
; CALLED BY FIELDCALC
;
	common llprams,xo,rbub,ao
	common llprams2,aa,bb,cc,dd,ee,ff,alnot,sig1,out
	common lloutblock,Streamout,brlambout,bthlambout,bphlambout

; Written SEG 2010
;  Modified March 11 2010 to fix bug in rotation
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)


;
;  Calculate the external (to bubble) field, Appendix B1
;
;  we are going to rotate the outside field so that its axis
;  of symmetry lies along the vector r1 defined by offset
;  xo -- basically a rotation about the y axis 
;  we do this by transforming through cartesian coords
;  note we have now changed naming conventions to be
;  consistent with right-hand coordinate system
;
;
		xtr = rtemp*sin(thtemp)*cos(phtemp)
		ytr = rtemp*sin(thtemp)*sin(phtemp) 
		ztr = rtemp*cos(thtemp) 

; now transform to spherical coords (radius is unchanged)

		rout = rtemp
;
; the tilde coords are the new x and z with up defined as above
; rotated 90 about z, and then 90 about y
; from the physical coordinate system
; (so that the r1 vector points up and the outside current sheet 
;  is in the y-z plane)
;  this is because the inner solution has its axis of symmetry naturally
;  at the equator, and the outer at the pole
;
; first rotate about z

		xtilde =   -ytr
		yt2 =  xtr
; now about xtilde
		ytilde =  -ztr
		ztilde =   yt2
                rat = (ztilde/rout)
                testpos = where(rat gt 1.)
                testneg = where(rat lt -1.)
                if min(testpos) ne -1 then rat[testpos] = 1.
                if min(testneg) ne -1 then rat[testneg] = -1.

                thout = acos(rat)

		phout =  atan(ytilde,xtilde)
;
;
; make sure phout is betweeen 0. and 2.d0*!dpi
;

		test = where(phout lt 0.)
		if min(test) ne -1 then phout[test]=2.d0*(!dpi)+1.d0*phout[test]
		test = where(phout gt 2.d0*!dpi)
		if min(test) ne -1 then phout[test] = phout[test]-2.d0*(!dpi)

        r1 = sqrt(xo*xo )
;
;    break down bits of stream function
;
	mu = cos(thout)
	al1 = r1*r1 - rbub*rbub 
	al2 = 2*r1*r1 - rbub*rbub
;
; eq. B2

	psi0 = mu
;
; eq. B3

	f = r1*(rout*rout + al1) - rout*mu*al2
	g = al1*al1 + r1*r1*rout*rout - 2*rout*r1*al1*mu
	h = 1/(sqrt(g))
	psi1 = (-1/rbub)*f*h 
;
; eq. B4

	i = rout*rout + r1*r1 - 2*rout*r1*mu
	psi3 = (1/rbub)*sqrt(i)
;
; eq. B5

	Streamout = psi0 + psi1 + psi3  
;
;  now take derivatives for magnetic fields, etc.
;
	dpsi0dr = 0.
	dpsi0dth = - sin(thout)
;
	dfdr = 2*r1*rout - mu*al2
	dfdmu = -rout*al2
;
	dgdr = 2*rout*r1*r1 - 2*r1*al1*mu
	dgdmu = -2*rout*r1*al1
;
	dhdr = -dgdr/2/(g^1.5)
	dhdmu = -dgdmu/2/(g^1.5)
;
	dpsi1dr = (-1/rbub)*(dfdr*h + f*dhdr)
	dpsi1dmu = (-1/rbub)*(dfdmu*h + f*dhdmu)
	dpsi1dth = -sin(thout)*dpsi1dmu
;
	didr = 2*rout - 2*r1*mu
	didmu = -2*rout*r1
;	
	dpsi3dr = (1/rbub)*didr/2/sqrt(i)
	dpsi3dmu = (1/rbub)*didmu/2/sqrt(i)
	dpsi3dth = -sin(thout)*dpsi3dmu
;
	dAdr = dpsi0dr + dpsi1dr + dpsi3dr
	dAdth = dpsi0dth + dpsi1dth + dpsi3dth
;
;  now calculate field
;
;  eq B1

	brlambout1 =  dAdth/rout/rout/sin(thout)
	bthlambout1 =  -dAdr/rout/sin(thout)
	bphlambout1 = 0.

;
; But now we need to get blittle and derivatives of blittle with respect to
; the physical coord r, which we need for density and pressure  (eq. A18-A19)
;  We do so by going through the cartesian coordinates.
;
		st = sin(thout)
		ct = cos(thout)
		sp = sin(phout)
		cp = cos(phout)
;
		blittleX1 = brlambout1*st*cp + bthlambout1*ct*cp	
		blittleY1 = brlambout1*st*sp + bthlambout1*ct*sp	
		blittleZ1 = brlambout1*ct - bthlambout1*st	


;
; but now we have to translate to the xtilde-rotated frame for x and z
;
		blittleY2 = blittleZ1 
		blittleZ = -blittleY1 
;
; and now about the z-rotated frame for x and y
;
		blittleX = blittleY2 
		blittleY = -blittleX1 
;
; and finally back to spherical coords
;
		streal = sin(thtemp)
		ctreal = cos(thtemp)
		spreal = sin(phtemp)
		cpreal = cos(phtemp)
		brlambout = blittleY*streal*spreal + blittleX*streal*cpreal+ blittleZ*ctreal
;	
		bthlambout =blittleY*ctreal*spreal+blittleX*ctreal*cpreal - blittleZ*streal
;
		bphlambout = -blittleX*spreal + blittleY*cpreal
;
;   and finally flip the sign about the equator
;
;		south = where(phout gt 0.d0  and phout lt (!dpi))
		south = where(phout gt (!dpi)  and phout lt 2.d0*(!dpi))
		if min(south) ne -1 then brlambout[south] = -1*brlambout[south]
		if min(south) ne -1 then bthlambout[south] = -1*bthlambout[south]
		if min(south) ne -1 then bphlambout[south] = -1*bphlambout[south]
	end

