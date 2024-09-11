	pro fieldcalc,rpB,thetapB,phipB

	common gltranscoord,rlam
	common glshiftcoord,Rcap,Thcap,Phcap
	common glsscoord,rsquig,phiss
	common glprams,apar,xo,rbub,ao,Pio,alpha,eta
	common glprams2,aa,bb,cc,dd,ee,ff,alnot,sig1,out
        common glresults,bstrengthphys,brphys,bthphys,bphphys
        common gljresults,jrphys,jthphys,jphphys
	common glfieldprams,Pi,blittlerlamb,blittlethlamb,blittlephlamb,Stream
	common gloutblock,Streamout,brlambout,bthlambout,bphlambout,tderivout,brderivout,bthphderivout
	common gljoutblock,jrlambout,jthlambout,jphlambout

;
;  CALLED BY GIBLOW
;
;  CALLS GETOUTSIDE, ZERO2TINY
;
;  Feb 2018: Added tests for small numbers (SEG)
;  March 2018: Added call to zero2tiny
;       Jul 2023 -- commented out if NaN stop -- because it can happen
;               for ok reasons-- namely, a USER request for pB
;               and the modsolstruct is evaluated assuming
;               losuse='tau', which evalutates at the thomson sphere
;               and some points in front of the disk could have the thompson
;               sphere inside r=1. Better to pass trhough the nans, which
;               forward can deal with



;  first calculate axisymm. stream function in shifted coordinates
;  SEE APPENDICES B1 and B2
;
		st= sin(Thcap)
		ct= cos(Thcap)
		sp= sin(Phcap)
		cp= cos(Phcap)
;
; problems occur for near-zero theta if the number
;  gets in the noise
;
	zero2tiny,st
	zero2tiny,sp
	zero2tiny,ct
	zero2tiny,cp

;
;  in the region Rcap lt rbub
		
;  eq. B8
		gfun = sin(alnot*Rcap)/alnot/Rcap - cos(alnot*Rcap)
		gfunot = sin(alnot*rbub)/alnot/rbub - cos(alnot*rbub)

;  eq. B7
		Stream = (4.d0*ao*!dpi/alnot/alnot)*(gfun*rbub^2/gfunot-Rcap^2)*st^2

;  in the region Rcap ge rbub
;  call routine that calculates stream function, etc
;  APPENDIX B1

 		outside = where(Rcap ge rbub)
		if min(outside) ge 0 then getoutside,rlam(outside),thetapB(outside),phipB(outside)
;
		if min(outside) ge 0 then Stream(outside) = out*Streamout(*)
;
;  define pressure
;
		Pi = ao*Stream + Pio	
		if min(outside) ge 0 then Pi(outside) = Pio
;
; now calculate little b (blittle), that goes with this Stream function
; and pressure in these bubble coordinates
; To do this, we need to use the derivatives of Stream with respect
; to the cap coords.
;
		dSdT = Stream*2*ct/st
;
		dgfundr = cos(alnot*Rcap)/Rcap - sin(alnot*Rcap)/Rcap/Rcap/alnot + alnot*sin(alnot*Rcap)
;
		dSdR = (4.d0*ao*!dpi/alnot/alnot)*(dgfundr*rbub^2/gfunot-2.d0*Rcap)*st^2
;
;  eq. B6
		blittleR = (1/Rcap/st)*((1/Rcap)*dSdT)
		blittleT = -(1/Rcap/st)*dSdR
;
;  ADD THE PHI FIELD!!!!!
; 
; NOTE! negative sign on BlittleP is to make a left-handed wind
;  about the apple core
;
		Q = alnot*Stream
;
		blittleP = -(1/Rcap/st)*Q
;
; also calculate currents by taking curl
;

		dbPdt=-(1./Rcap/st)*alnot*dSdT
		dbPdt=dbPdt + (ct/Rcap/st/st)*Q

		dbPdR=-(1./Rcap/st)*alnot*dSdR
		dbPdR=dbPdR + (1./Rcap/Rcap/st)*Q
		
		d2gfundr2= 2.*gfun/Rcap/Rcap - alnot*alnot*gfun

		d2SdR2= ((((rbub^2)/gfunot)*d2gfundr2 - 2.)/(((rbub^2)/gfunot)*gfun - Rcap*Rcap))*Stream
		d2SdT2= (4.*((ct/st)^2) - 2./st/st)*Stream

		dbTdR= (1/Rcap/Rcap/st)*dSdR - (1/Rcap/st)*d2SdR2

		dbRdT= -(ct/Rcap/Rcap/st/st)*dSdT + (1/Rcap/Rcap/st)*d2SdT2

	 	jlittleR = (1./Rcap/st)*(ct*blittleP+st*dbPdt)

		jlittleT = -(1./Rcap)*(blittleP + Rcap*dbPdR)

		jlittleP = (1./Rcap)*(blittleT + Rcap*dbTdR - dbRdT)
;
; But now we need to transform to blittle coordinates with respect to
; the physical coord r, which we need for density and pressure (eq. A19)
; (note the derivative calculation for this interior field happens in giblow - tderiv)
;  We do so by going through the cartesian coordinates.
;  note this has also now been rewritten in right-hand coordinates
;
		blittleX = blittleR*st*cp + blittleT*ct*cp - blittleP*sp	
		blittleY1 = blittleR*st*sp + blittleT*ct*sp + blittleP*cp
		blittleZ1 = blittleR*ct - blittleT*st	
		jlittleX = jlittleR*st*cp + jlittleT*ct*cp - jlittleP*sp	
		jlittleY1 = jlittleR*st*sp + jlittleT*ct*sp + jlittleP*cp
		jlittleZ1 = jlittleR*ct - jlittleT*st	
;
; but now we have to translate to the sigma rotated (about x axis) frame for y and z
;  note clockwise rotation-- moves back into physical coordinates
;
		sigma = sig1
;
		blittleY = cos(sigma)*blittleY1 + sin(sigma)*blittleZ1 
		blittleZ = cos(sigma)*blittleZ1 - sin(sigma)*blittleY1 
		jlittleY = cos(sigma)*jlittleY1 + sin(sigma)*jlittleZ1 
		jlittleZ = cos(sigma)*jlittleZ1 - sin(sigma)*jlittleY1 
;
		ctreal = cos(thetapB)
		streal = sin(thetapB)
		cpreal = cos(phipB)
		spreal = sin(phipB)
;
;  these are the "little b" magnetic field components- the little b r
;  is needed in calculating pressure and density in the pB coords
;  see Appendix A, esp. eqs. A18-A19
;
		blittlerlamb = blittleY*streal*spreal + blittleX*streal*cpreal+ blittleZ*ctreal
;	
		blittlethlamb =blittleY*ctreal*spreal+blittleX*ctreal*cpreal - blittleZ*streal
;
		blittlephlamb = -blittleX*spreal + blittleY*cpreal
;
		jlittlerlamb = jlittleY*streal*spreal + jlittleX*streal*cpreal+ jlittleZ*ctreal
;	
		jlittlethlamb =jlittleY*ctreal*spreal+jlittleX*ctreal*cpreal - jlittleZ*streal
;
		jlittlephlamb = -jlittleX*spreal + jlittleY*cpreal
;
;   we need to put in the proper values for the outside field
;
		if min(outside) ge 0 then blittlerlamb(outside)= out*brlambout(*)
		if min(outside) ge 0 then blittlethlamb(outside)= out*bthlambout(*)
		if min(outside) ge 0 then blittlephlamb(outside)= out*bphlambout(*)
		if min(outside) ge 0 then jlittlerlamb(outside)= out*jrlambout(*)
		if min(outside) ge 0 then jlittlethlamb(outside)= out*jthlambout(*)
		if min(outside) ge 0 then jlittlephlamb(outside)= out*jphlambout(*)
;
;  and finally, here are the magnetic field coordinates in the rpB,thetapB
;  phipB coordinate systems !  we have to also get rid of selfsim stuff
;
;  eqs.  A2-A4

;  dlamdr is one, because rlam = rsquig - apar.
;
		dlamdr = 1
;
		brphys = (blittlerlamb*(rlam/rsquig)^2)/(phiss^2)
		bthphys = blittlethlamb*(rlam/rsquig)*dlamdr/(phiss^2)
		bphphys = blittlephlamb*(rlam/rsquig)*dlamdr/(phiss^2)
		jrphys = (jlittlerlamb*(rlam/rsquig)^2)/(phiss^2)
		jthphys = jlittlethlamb*(rlam/rsquig)*dlamdr/(phiss^2)
		jphphys = jlittlephlamb*(rlam/rsquig)*dlamdr/(phiss^2)
;
; check to make sure the geometry is right by comparing field strengths
;
;		bstrength1 = sqrt(blittleR^2+blittleT^2)
;		bstrength2 = sqrt(blittleX^2+blittleY^2+blittleZ^2)
;		bstrength3 = sqrt(blittlerlamb^2 + blittlethlamb^2 + blittlephlamb^2)
;
;  but bstrengthphys is the actual physical field strength in the rpB, 
; thetapB, phipB coords
;
		bstrengthphys = sqrt(brphys^2 + bthphys^2 + bphphys^2)

;	        test=where(bstrengthphys*0. ne 0.)
;		if min(test) ne -1 then stop
		end
