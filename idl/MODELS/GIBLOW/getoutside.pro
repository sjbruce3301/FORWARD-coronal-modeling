	pro getoutside,rtemp,thtemp,phtemp
; 
; CALLED BY FIELDCALC
;
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)

; CALLS ZERO2TINY
;
	common gltranscoord,rlam
	common glsscoord,rsquig,phiss
	common glprams,apar,xo,rbub,ao,Pio,alpha,eta
	common glprams2,aa,bb,cc,dd,ee,ff,alnot,sig1,out
	common gloutblock,Streamout,brlambout,bthlambout,bphlambout,tderivout,brderivout,bthphderivout
	common gljoutblock,jrlambout,jthlambout,jphlambout

; Written SEG 2010
;  Modified March 11 2010 to fix bug in rotation
;  Feb 2018 added tests for small numbers, replaced sin(thout) with st, cos(thout) with mu
;	also strengthened NaN test
;  March 2018 added zero2tiny

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
 		st=sin(thout)
 		mu=cos(thout)
;
; problems occur for near-zero theta if the number
;  gets in the noise 
;
		zero2tiny,st
		zero2tiny,mu

		phout =  atan(ytilde,xtilde)
;
;
; make sure phout is betweeen 0. and 2.d0*!dpi
;

		test = where(phout lt 0.)
		if min(test) ne -1 then phout[test]=2.d0*double(!dpi)+phout[test]
		test = where(phout gt 2.d0*!dpi)
		if min(test) ne -1 then phout[test] = phout[test]-2.d0*double(!dpi)

        r1 = sqrt(xo*xo )
;
;    break down bits of stream function
;
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
	dpsi0dth = -st
	d2psi0dth2 = -mu
	dpsi0dr2 = 0.
	dpsi0drdth = 0.
;
	dfdr = 2*r1*rout - mu*al2
	dfdmu = -rout*al2
	dfdrdmu = -al2
	dfdr2 = 2*r1
;
	dgdr = 2*rout*r1*r1 - 2*r1*al1*mu
	dgdmu = -2*rout*r1*al1
	dgdrdmu = -2*r1*al1
	dgdr2 = 2*r1*r1 
;
	dhdr = -dgdr/2/(g^1.5)
	dhdmu = -dgdmu/2/(g^1.5)
	d2hdmu2 = 3.*dgdmu/4/(g^2.5)
	dhdrdmu = -dgdrdmu/2/(g^1.5) + 3*dgdmu*dgdr/4/(g^2.5)
	dhdr2 = -dgdr2/2/(g^1.5) + 3*dgdr*dgdr/4/(g^2.5)
;
	dpsi1dr = (-1/rbub)*(dfdr*h + f*dhdr)
	dpsi1dr2 = (-1/rbub)*(2*dfdr*dhdr + dfdr2*h + f*dhdr2)
;
	dpsi1dmu = (-1/rbub)*(dfdmu*h + f*dhdmu)
	d2psi1dmu2 = (-1/rbub)*(2.*dfdmu*dhdmu + f*d2hdmu2)
	dpsi1drdmu = (-1/rbub)*(dfdmu*dhdr + dfdr*dhdmu + dfdrdmu*h + f*dhdrdmu)
;	
	dpsi1dth = -st*dpsi1dmu
	dpsi1drdth = -st*dpsi1drdmu
	d2psi1dth2 = -mu*dpsi1dmu +(st^2)*d2psi1dmu2
;
	didr = 2*rout - 2*r1*mu
	didmu = -2*rout*r1
	didrdmu = -2*r1
	didr2 = 2.
;	
	dpsi3dr = (1/rbub)*didr/2/sqrt(i)
	dpsi3dmu = (1/rbub)*didmu/2/sqrt(i)
	d2psi3dmu2 = -(1/rbub)*didmu/4/(i^(1.5))
	dpsi3drdmu = (1/rbub)*(didrdmu/2/sqrt(i) - didmu*didr/4/(i^1.5))
	dpsi3dr2 = (1/rbub)*(didr2/2/sqrt(i) - (didr)^2/4/(i^1.5))
;
	dpsi3dth = -st*dpsi3dmu
	dpsi3drdth = -st*dpsi3drdmu
	d2psi3dth2 = -mu*dpsi3dmu +(st^2)*d2psi3dmu2
;
	dAdr = dpsi0dr + dpsi1dr + dpsi3dr
	dAdth = dpsi0dth + dpsi1dth + dpsi3dth
	dAdrdth = dpsi0drdth + dpsi1drdth + dpsi3drdth
	dAdr2 = dpsi0dr2 + dpsi1dr2 + dpsi3dr2
	dAdth2 = d2psi0dth2 + d2psi1dth2 + d2psi3dth2
;
;  now calculate field
;
;  eq B1

	brlambout1 =  dAdth/rout/rout/st
	bthlambout1 =  -dAdr/rout/st
	bphlambout1 = 0.

; and calculate currents

	dbth1dr = (dAdr/rout - dAdr2)/rout/st
	dbr1dth= ( (-mu/st)*dAdth+ dAdth2)/rout/rout/st

	jrlambout1 =  0.
	jthlambout1 = 0.
	jphlambout1 = (1./rout)*(bthlambout1 + rout*dbth1dr - dbr1dth)
;
; But now we need to get blittle and derivatives of blittle with respect to
; the physical coord r, which we need for density and pressure  (eq. A18-A19)
;  We do so by going through the cartesian coordinates.
;
		sp = sin(phout)
		cp = cos(phout)
;
;
		zero2tiny,sp
		zero2tiny,cp

		blittleX1 = brlambout1*st*cp + bthlambout1*mu*cp	
		blittleY1 = brlambout1*st*sp + bthlambout1*mu*sp	
		blittleZ1 = brlambout1*mu - bthlambout1*st	
		jlittleX1 = jrlambout1*st*cp + jthlambout1*mu*cp	
		jlittleY1 = jrlambout1*st*sp + jthlambout1*mu*sp	
		jlittleZ1 = jrlambout1*mu - jthlambout1*st	

	jphlambout1 = 0.

;
; but now we have to translate to the xtilde-rotated frame for x and z
;
		blittleY2 = blittleZ1 
		blittleZ = -blittleY1 
		jlittleY2 = jlittleZ1 
		jlittleZ = -jlittleY1 
		jlittleX1 = 0.
		jlittleZ1 = 0.
;
; and now about the z-rotated frame for x and y
;
		blittleX = blittleY2 
		blittleY = -blittleX1 
		jlittleX = jlittleY2 
		jlittleY = -jlittleX1 
		jlittleX1 = 0.
		jlittleZ1 = 0.
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
		jrlambout = jlittleY*streal*spreal + jlittleX*streal*cpreal+ jlittleZ*ctreal
;	
		jthlambout =jlittleY*ctreal*spreal+jlittleX*ctreal*cpreal - jlittleZ*streal
;
		jphlambout = -jlittleX*spreal + jlittleY*cpreal

		jlittleX=0.
		jlittleY=0.
		jlittleZ=0.

;  now calculated tderivout (needed for density  A19)
;
	dbr1dr = (-2.*dAdth/rout+ dAdrdth)/rout/rout/st
;
	tderivout = (brlambout1*dbr1dr + bthlambout1*dbth1dr)/4/double(!dpi)	
;
;  I don't think these next 7 lines are used -- maybe they were diagnostic
;  when I was making sure forces balanced
;
	dbrdr = (dbr1dr*mu-dbth1dr*st)*streal*spreal
	dbrdr = dbrdr + (dbr1dr*st*cp + dbth1dr*mu*cp)*streal*cpreal
	dbrdr = dbrdr -(dbr1dr*st*sp+dbth1dr*mu*sp)*ctreal
	brderivout = brlambout*dbrdr/4.d0/!dpi
	bthphderivout = tderivout - brderivout
;
;   and finally flip the sign about the equator
;
;		south = where(phout gt 0.d0  and phout lt double(!dpi))
		south = where(phout ge double(!dpi)  and phout le 2.d0*double(!dpi))
		if min(south) ne -1 then brlambout[south] = -1*brlambout[south]
		if min(south) ne -1 then bthlambout[south] = -1*bthlambout[south]
		if min(south) ne -1 then bphlambout[south] = -1*bphlambout[south]
test = where(brlambout*0. ne 0. or bthlambout*0. ne 0. or bphlambout*0. ne 0.)
if min(test) ne -1 then begin
;stop
 brlambout[test]=0.
 bthlambout[test]=0.
 bphlambout[test]=0.
 tderivout[test]=0.
 brderivout[test]=0.
 bthphderivout[test]=0.
 Streamout[test]=0.
 nbad=size(test)
 nbad=nbad[1]
 print,'replacing ',strtrim(string(nbad),2),' points with zeros'
endif 
	end

