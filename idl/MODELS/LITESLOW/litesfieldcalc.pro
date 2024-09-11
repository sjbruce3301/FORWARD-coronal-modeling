	pro litesfieldcalc,rpB,thetapB,phipB

	common llshiftcoord,Rcap,Thcap,Phcap
	common llprams,xo,rbub,ao
	common llprams2,aa,bb,cc,dd,ee,ff,alnot,sig1,out
        common llresults,bstrengthphys,brphys,bthphys,bphphys
	common lloutblock,Streamout,brlambout,bthlambout,bphlambout

;
;  CALLED BY LITESLOW
;  CALLS LITESGETOUTSIDE
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)



;  first calculate axisymm. stream function in shifted coordinates
;
		st= sin(Thcap)
		ct= cos(Thcap)
		sp= sin(Phcap)
		cp= cos(Phcap)
;
;  in the region Rcap lt rbub
		
;  eq. 5 Lites and Low
;
;  NOTE!! Lites-low is different from Gib-Low alnot is not defined as a root
; of a Bessel function, but a simpler, trig function (gfun)
;  ALSO NOTE: there is a B --> -B when comparing Lites-Low and Gibson-Low
;   because of the choice of alnot root for the two.  The easiest way
; to make them equivalent is to make switch the sign of ao.  We have done
; this in liteslowprams
;
		Stream = ao*(sin(alnot*Rcap)/alnot/Rcap - cos(alnot*Rcap))*st*st

;  in the region Rcap ge rbub
;  call routine that calculates stream function, etc
;  as in Gibson and Low model, APPENDIX B1

 		outside = where(Rcap ge rbub)
		if min(outside) ge 0 then litesgetoutside,rpB[outside],thetapB[outside],phipB[outside]
;
		if min(outside) ge 0 then Stream[outside] = out*Streamout[*]
;
; now calculate little b (blittle), that goes with this Stream function
; in these bubble coordinates
; To do this, we need to use the derivatives of Stream with respect
; to the cap coords.
;
		dSdT = Stream*2*ct/st
;
		dSdR = ao*st*st*(cos(alnot*Rcap)/Rcap - sin(alnot*Rcap)/Rcap/Rcap/alnot + alnot*sin(alnot*Rcap))
;
;  eq. 1 of lites-low

		blittleR = (1/Rcap/st)*((1/Rcap)*dSdT)
		blittleT = -(1/Rcap/st)*dSdR
;
;  ADD THE PHI FIELD!!!!!
; 
;  NOTE!! negative sign is here to make a left-handed rope about the apple core
;
		Q = alnot*Stream
;
		blittleP = -(1/Rcap/st)*Q
;
;
; But now we need to transform to blittle coordinates with respect to
; the physical coord r
;  We do so by going through the cartesian coordinates.
;  note this has also now been rewritten in right-hand coordinates
;
; test see below, comment next if not use
;		blittleX1 = blittleR*st*cp + blittleT*ct*cp - blittleP*sp	
		blittleX = blittleR*st*cp + blittleT*ct*cp - blittleP*sp	
		blittleY1 = blittleR*st*sp + blittleT*ct*sp + blittleP*cp
		blittleZ1 = blittleR*ct - blittleT*st	
;
; but now we have to translate to the sigma rotated (about x axis) frame for y and z
;  note clockwise rotation-- moves back into physical coordinates
;
		sigma = sig1
;
; testing spheromak tilt 
;   -- comment these next two and uncomment two aftr that to fix
;		blittleX = cos(sigma)*blittleX1 + sin(sigma)*blittleZ1 
;		blittleZ = cos(sigma)*blittleZ1 - sin(sigma)*blittleX1 
;		blittleY=blittleY1
		blittleY = cos(sigma)*blittleY1 + sin(sigma)*blittleZ1 
		blittleZ = cos(sigma)*blittleZ1 - sin(sigma)*blittleY1 
;
		ctreal = cos(thetapB)
		streal = sin(thetapB)
		cpreal = cos(phipB)
		spreal = sin(phipB)

;
;  and finally, here are the magnetic field coordinates in the rpB,thetapB
;
		brphys = blittleY*streal*spreal + blittleX*streal*cpreal+ blittleZ*ctreal
		bthphys =blittleY*ctreal*spreal+blittleX*ctreal*cpreal - blittleZ*streal
		bphphys = -blittleX*spreal + blittleY*cpreal

;   we need to put in the proper values for the outside field
;
		if min(outside) ge 0 then brphys[outside]= out*brlambout[*]
		if min(outside) ge 0 then bthphys[outside]= out*bthlambout[*]
		if min(outside) ge 0 then bphphys[outside]= out*bphlambout[*]

		bstrengthphys = sqrt(brphys^2 + bthphys^2 + bphphys^2)

	        test=where(bstrengthphys*0. ne 0.)
		if min(test) ne -1 then stop
		end
