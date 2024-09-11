;************************************************************************
     pro for_pbbcalc,r3D,LosPramsStruct,ModSolStruct,GridPramsStruct,pop2tregime,tau2,r2,denssign,testnearuse,ulimb,sumB,sumpB

;+
; Name: FOR_PBBCALC
;
; Purpose:  determine pB, TB, for points along LOS
;
; INPUTS	r3D is always in [nlos,nactualbite] dimensions
;		LosPramsStruct,ModSolStruct,pop2tregime,
;               r2- pos position but put in [nlos,nactualbite] matrix
;		tau,denssign (note these are in same dimensions
;		as ModSolStruct.Dens, either 1D, or [nlos,nactualbite]
;		depending on whether there are far field points
;		ulimb is limb darkening coefficient
;
; OUTPUTS sumB,sumpB
;		note these are in same dimension as r3D [nlos,nactualbite]
;		they represent 2*I_t and I_t - I_r respectively
;		sumpB is the integrand for pB, but sumB is NOT the integrand for TB
;		TB = I_t + I_r = 2*I_t - (I_t - I_r) = sumB - sumpB
;		this is done in for_integrate.pro
;
; Called by: FOR_INTENSINT
;
; Calls: FOR_CFUN, FOR_BFUN
;
;
; HISTORY: 
;	Written by Sarah Gibson,Terry Kucera 2015
; 	  Feb 2016 edits for pop2 - SEG
;	  Nov 2020 - edited scattering angle chi to account for nonzero elongation
;		(lines of sight not parallel to sun-earth line)
;		also added comments for clarity
;	  Dec 2021 - passed through GridPramsStruct to get distobs
;	  Jan 2022 -- this was actually unnecessary (bringing in distobs)
;		because it was used to calculate elongation (epslon)
;		but the calculations below do not depend on it. 
;		The bug was a line stating
;		   chi = epslon + !dpi/2.d0 - tau2
;		   chi (scattering angle) makes up third angle in
;		   right triangle made by radial vector intersecting
;		   LOS and TS, LOS, and radial vector intersecting 
;		   scattering particle, where other (non-right) angle
;		   is by definition tau.
;	  Mar 2022 -- removed conditional for TAU/XLOS instead
;		in FOR_INTLOS we define tau2 properly for all
;		choices including TLOS, so the same equation
;		(in terms of sin(chi)) works for all
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;	Jul 2024 - passed through ulimb
;		
;-

;
; deal with possibility of 1D array of different dimension that r3D
;

		if min(testnearuse) ne -1 then r3Duse= r3D[testnearuse] else r3Duse=r3D

;
; set up filling factor to be one if not set
; note, FOR_DOPOP2INIT will have ensured pop2 filling factor is set
;

      		fill=1.
      		if tag_exist(ModSolStruct,'FillingFactor') then fill=ModSolStruct.FillingFactor
;
; zero out pop2 density for pop2tregime=3
;
                  if pop2tregime eq 3 then begin
                   if tag_exist(ModSolStruct,'Pop2Dens') then ModSolStruct.Pop2Dens = 1d-5
                  endif

                if pop2tregime ne 0 then $
 		  if tag_exist(ModSolStruct,'Pop2Dens') then pop2dens=ModSolStruct.Pop2Dens else pop2dens=ModSolStruct.Dens

;
; scattering angle chi
;  (be careful here -- if LOSINT=XLOS, this is the angle
;	along parallel lines of sight, FROM THE X=0 plane,
;	because of how TAU defined in for_intlos)
;

		chi =  !dpi/2.d0 - tau2

;  first calculate pB
;
                cff = for_cfun(r3Duse,ulimb=ulimb)   ;scattering function -- note units assume length in solar radii

                sumpB = denssign*fill*cff*sin(chi)*sin(chi)*ModSolStruct.Dens

;  these follow from equation 2.17 or 2.18 b in Guhathakurta thesis
;  note -- pB is the difference between radial and tangential polarized components
;	the tangential polarized component is a function that only depends on r and density
;       the radial polarized component is the difference between that function and the sumpB calculated above
;	so pB is just that sumpB part, depending on r, density, and scattering angle
;	 and we don't even need to calculate the first function here

;Add Pop2 electrons

                if pop2tregime ne 0 then $
                      sumpB = sumpB+denssign*ModSolStruct.Pop2FillingFactor*cff*sin(chi)*sin(chi)*pop2dens

; here we calculate the function that represents the tangential polarized component 
; multiplied by 2 for convenience in the calculation of total brightness which is the sum
;  of the radial and tangential components. It's determined in for_integrate.
;
                bff = for_bfun(r3Duse,ulimb=ulimb)

                sumB =  denssign*fill*bff*2.*ModSolStruct.Dens

;Add Pop2 electrons
                if pop2tregime ne 0 then sumB=sumB+denssign*ModSolStruct.Pop2FillingFactor*bff*2.*pop2dens

;
; make sure sumB and sumpB are in same dimensions as r3D
;

		if min(testnearuse) ne -1 then begin
		   sumBreal=r3D*0.
		   sumpBreal=r3D*0.
		   sumBreal[testnearuse]=sumB
		   sumpBreal[testnearuse]=sumpB
		   sumB=sumBreal
		   sumpB=sumpBreal
		endif
end
