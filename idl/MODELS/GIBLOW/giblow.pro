	pro giblow,rpB,thetapB,phipB,ModPramsStruct,ModSolStruct,insidemask=inside

	common gltranscoord,rlam
	common glshiftcoord,Rcap,Thcap,Phcap
	common glsscoord,rsquig,phiss
	common glprams,apar,xo,rbub,ao,Pio,alpha,eta
	common glprams2,aa,bb,cc,dd,ee,ff,alnot,sig1,out
	common glresults,bstrengthphys,brphys,bthphys,bphphys
        common gljresults,jrphys,jthphys,jphphys
	common glfieldprams,Pi,blittlerlamb,blittlethlamb,blittlephlamb,Stream
	common gloutblock,Streamout,brlambout,bthlambout,bphlambout,tderivout,brderivout,bthphderivout
	common glinblock,Pbackin,Dbackin
	common glbackground,Densback,Presback

;
; Name: GIBLOW
;
; Purpose: Calculate the density, temperature, magnetic field of the
;          Gibson-Low model at a location rpB, thetapB, phipB (true spherical
;          coordinates)
;
; Inputs: 
;	   rpB, thetapB, phipB -- position in 3D space where model is to be evaluated
;				r in units of RSUN, th, ph in RADIANS
;
;          ModPramsStruct - structure associated with model, containing
;                           model name (GIBLOW), model parameters
;			    set up in giblowprams.pro
;
; Outputs: ModSolStruct - Solution of model, containing density,temperature,
;                         pressure, magnetic field (Br, Bth, Bph),current, and velocity
;
; Common Blocks: transcoord, shiftcoord, sscoord, prams, prams2, results, jresults, 
;		  fieldprams, outblock, inblock, background
;
; Calls coords, fieldcalc, getinside,zero2tiny
;
;  Written: S. Gibson 
;
; Version 2.0 July 2014
; Adapted from denscalc in original version of code
; 
; Feb 2018 put in tests for small numbers; fixed bug in constants for calculating temperature
; Mar 2018 added subroutine zero2tiny
; July 2018 -- fixed bug where Bonly was using commented constants-
;	redefined in terms of kboltz
; June 2019 -- added capability for phiss to be entered in same dimension as rpb
;  	note this generally would be done such that 
;	(rpb,thetapb,phipb) and phiss are
;	multidimensional arrays where one (or more) dimension is the spatial
;	variation (which could be a single point) and 
;	the other dimension is temporal (thus, phiss is constant in 
;	the spatial dimension(s) and rpb,thetpb,phipb are constant in the temporal dimension)
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;       Jul 2023 -- commented out if NaN stop -- because it can happen
;               for ok reasons-- namely, a USER request for pB
;               and the modsolstruct is evaluated assuming
;               losuse='tau', which evalutates at the thomson sphere
;               and some points in front of the disk could have the thompson
;               sphere inside r=1. Better to pass trhough the nans, which
;               forward can deal with
;	 

; Read in Parameters:

        xo = ModPramsStruct.Xo
        rbub = ModPramsStruct.Rbub
        apar = ModPramsStruct.Apar
        ao = ModPramsStruct.Ao
        alnot = ModPramsStruct.Alnot
        sig1= ModPramsStruct.Sig1
        out = ModPramsStruct.Out
        Pio = ModPramsStruct.Pio
        aa = ModPramsStruct.AA
        bb = ModPramsStruct.BB
        cc = ModPramsStruct.CC
        dd = ModPramsStruct.DD
        ee = ModPramsStruct.EE
        ff = ModPramsStruct.FF
        alpha = ModPramsStruct.Alpha
        eta = ModPramsStruct.Eta
        if n_elements(ModPramsStruct.Phiss) eq 1 then phiss = rpb*0.+ModPramsStruct.Phiss else phiss=ModPramsStruct.Phiss
	bubbleonly=ModPramsStruct.BubbleOnly

;
;  first shift coordinate systems
;
        coords,rpB,thetapB,phipB
;
;  now calculate the field
;
        fieldcalc,rpB,thetapB,phipB
;
; now calculate pressure from Gibson and Low eq. A18
;
        Pres= (rlam^2/rsquig^2)*(1-(rlam^2/rsquig^2))*(blittlerlamb^2)/8.d0/!dpi $
              + (rlam^2/rsquig^2)*Pi
;
; now we have to match total pressure Pmag+Pgas at the bubble interface
;  note that the INNER bubble solution is defined so that Pmag and Pgas are zero there
;  so, we calculate for each rlam, at the theta of the bubble boundary, the OUTER
;  solution total pressure, and add it to the INNER gas pressure 
;  (for all theta within the bubble at that rlam).  Then we must also
;  add a similar density increment to the INNER density, because the OUTER solution
; is potential and so has density and pressure in HE balance 
;
        Presback=Pres*0.d0
        inside = where(Rcap lt rbub)
        if min(inside) ge 0 then begin
           getinside,rlam[inside],phiss[inside]
           Presback[inside] =  Pbackin*(phiss[inside]^4)
        endif

;
;  now we put in additional hydrostatic background pressure to keep
;  things positive
;
        presout=(aa/(bb+1.))*rsquig^(-bb-1.)+(cc/(dd+1.))*rsquig^(-dd-1.)+ $
                (ee/(ff+1.))*rsquig^(-ff-1.)

        Rsun = 6.9570d10
        GMm = 221.3d0
	presout=presout*GMm/Rsun

;
; see Gibson et al 1998, eqs 3 and 4 - note we are assuming alpha (He) = 0
;

; presin and densin only matter for CME solution
; and are put in as a means of stopping the background density
; from blowing up as the structure self-similarly expands
; since density scales as phiss^-3 and pressure as phiss^-4
; and rsquig = rpB/phiss
; however, note this means that for rsquig less than 1, the
; background density won't fall off as defined by aa,bb,cc,dd,ee,f
; but rather by an r^3 power law.  Thus, if fitting a magnetostatic
; solution it is best to set phiss=1
;
        presin=(1.d0/4.d0)*(aa+cc+ee)*rsquig^(-4.) 
        presin_0=-(1.d0/4.d0)*(aa+cc+ee)
        presin_0=presin_0+(aa/(bb+1.))+(cc/(dd+1.))+(ee/(ff+1.))
        presin=presin+presin_0
	presin=presin*GMm/Rsun

; thus presin and presout will match smoothly at rsquig = 1
;        
        testin=where(rsquig lt 1.)
        testout=where(rsquig ge 1.)

        if min(testin ne -1) then Presback[testin]=Presback[testin]+presin[testin]
        if min(testout ne -1) then Presback[testout]=Presback[testout]+presout[testout]
        
        Pres = Pres + Presback
;
; self-similar transform
;
        Pres = Pres/(phiss^4)

;        test = where(pres*0. ne 0.)
;        if min(test) ne -1 then stop
        test = where(pres lt 0.)
;        if min(test) ne -1 and ModPramsStruct.Bonly eq 0 then  print,'negative pressures'
;
;  now calculate density
;
;  See eq A19
; 
;  first need to calculate tderiv = dT/drlam, where T = Pi + Bstrength (squared)
;  in the Rcap,Thcap system
;
        st = sin(thetapB)
        ct = cos(thetapB)
        sp = sin(phipB)
        cp = cos(phipB)
;
        sTbig = sin(Thcap)
        cTbig = cos(Thcap)
        sPbig = sin(Phcap)
        cPbig = cos(Phcap)
;
; problems occur for near-zero theta if the number
;  gets in the noise
;
	zero2tiny,st
	zero2tiny,sTbig
	zero2tiny,ct
	zero2tiny,cTbig

;
        mu = Rcap*sTbig
;
        gfunot = sin(alnot*rbub)/alnot/rbub - cos(alnot*rbub)
;
        dSdT = Stream*2*cTbig/sTbig
;
        dgfundr = cos(alnot*Rcap)/Rcap - sin(alnot*Rcap)/Rcap/Rcap/alnot + $
                  alnot*sin(alnot*Rcap)
;
        dSdR = (4.d0*ao*!dpi/alnot/alnot)*(dgfundr*rbub^2/gfunot-2*Rcap)*sTbig^2

        d2SdTdR = dSdR*2.d0*cTbig/sTbig
;		
        d2gfundr2 = (-Rcap*Rcap*alnot*sin(alnot*Rcap) - Rcap*cos(alnot*Rcap) + $ 
                     alnot*Rcap*Rcap*Rcap*alnot*cos(alnot*Rcap) - $
                     cos(alnot*Rcap)*Rcap + 2*sin(alnot*Rcap)/alnot)/Rcap/Rcap/Rcap
;
        d2SdR2 = (4.d0*ao*!dpi/alnot/alnot)*(d2gfundr2*rbub^2/gfunot-2)*sTbig^2
;		
        d2SdT2 = Stream*2*(cTbig^2-sTbig^2)/(sTbig^2)
;
        dRdmu = sTbig
        dthetadmu = cTbig/Rcap
;
        dPidR = ao*dSdR
;
        dPidmu=ao*(dRdmu*dSdR + dthetadmu*dSdT)
;
        dTdR = - 4*(dSdT^2)/(Rcap^5)/(sTbig^2)
        dTdR = dTdR +2*dSdT*d2SdTdR/(Rcap^4)/(sTbig^2)
        dTdR = dTdR -2*(dSdR^2)/(Rcap^3)/(sTbig^2)
        dTdR = dTdR +2*dSdR*d2SdR2/(Rcap^2)/(sTbig^2)
;
        dTdR = dTdR -2*alnot*alnot*(Stream^2)/(Rcap^3)/(sTbig^2)
        dTdR = dTdR + 2*alnot*alnot*Stream*dSdR/(Rcap^2)/(sTbig^2)
;
        dTdR = dTdR/8.d0/!dpi
        dTdR = dTdR + dPidR
;
        dTdmu =  -2*((dSdT/Rcap)^2)/(mu^3)
        dTdmu = dTdmu +2*(dSdT/Rcap)*(dRdmu*(-dSdT/(Rcap^2)+ $
                       d2SdTdR/Rcap)+dthetadmu*d2SdT2/Rcap)/(mu^2)
        dTdmu = dTdmu -2*(dSdR^2)/(mu^3)
        dTdmu = dTdmu +2*dSdR*(dRdmu*d2SdR2+dthetadmu*d2SdTdR)/(mu^2)
;
        dTdmu = dTdmu -2*alnot*alnot*(Stream^2)/(mu^3)
        dTdmu = dTdmu +2*alnot*alnot*Stream*(dRdmu*dSdR+dthetadmu*dSdT)/(mu^2)
;
        dTdmu = dTdmu/8.d0/!dpi
        dTdmu = dTdmu + dPidmu

        dRdlam = (rlam - xo*st*cp)/Rcap
;
;  remember we are in a rotated mu frame
;
		
        sigma = sig1
;	
        cs = cos(sigma)
        ss = sin(sigma)
;	
        dmudlam = (rlam*(st^2*(cp^2+sp^2*cs^2) + $
                           ct^2*ss^2 - 2*st*ct*sp*ss*cs) - xo*st*cp)/mu
;	
        tderivR = dTdR*dRdlam
        tderivmu = dTdmu*dmudlam
;
        tderiv = tderivR + tderivmu
;
        outside = where(Rcap ge rbub)
        if min(outside) ge 0 then tderiv[outside] = out*out*tderivout[*]
;
;  the force F could include the effects of the self-similar
;  acceleration (alpha ne 0) - otherwise its just gravity.
; Note that to properly include alpha ne 0, however, phiss needs
; to be defined as a differential equation
;
        mprot = 1.674d-24
        F = GMm/(rsquig^2)/(Rsun^2) + alpha*rsquig*Rsun*mprot
;
        Dens = -((rlam/rsquig)^2)*(1-((rlam/rsquig)^2))*tderiv
        Dens = Dens + 2*(rlam/rsquig)*(apar/(rsquig^2))*Pi
        Dens = Dens + (rlam/rsquig)*(apar/(rsquig^2))* $
               (1-2*((rlam/rsquig)^2))*blittlerlamb^2/4.d/!dpi
        Dens = Dens + ((rlam/rsquig)^2)*((apar^2)/(rsquig^2)+ $
                2*(apar/rsquig))*(blittlethlamb^2+blittlephlamb^2)/4.d/!dpi/rlam
        Dens = Dens/F/Rsun

;  put in background density
;  first radial power law HE
; see Gibson et al 1998, eq 3 
;
        densin=(aa+cc+ee)*rsquig^(-3.)

        densout=(aa*rsquig^(-bb)+cc*rsquig^(-dd)+ee*rsquig^(-ff))

        Densback=Dens*0.d0

        if min(testin ne -1) then Densback[testin]=densin[testin]
        if min(testout ne -1) then Densback[testout]=densout[testout]
;
        DensbackHEonly =  Densback
; 
; now add total pressure continuity part
;
        if min(inside) ge 0 then Densback[inside]=Densback[inside]+Dbackin*(phiss[inside]^3)

        Dens = Dens + Densback
;
;  put dens in ss coords
;
        Dens = Dens/(phiss^3)
        Densback = Densback/(phiss^3)

;        test = where(dens*0. ne 0.)
;        if min(test) ne -1 then stop
        test = where(dens lt 0.)
;        if min(test) ne -1 and ModPramsStruct.Bonly eq 0 then  print,'negative densities'
 
; also velocity (in km/sec)
; note this is radial

        dphidt = sqrt((eta*phiss - 2.d0*alpha)/phiss)
        Vr = (rpB*6.9570d5*dphidt)/phiss

; calculate temperature

 	kboltz=1.3807d-16
        Temp = Pres/2./kboltz/Dens

	; these next few lines are not very accurate, and confusing
        ;mp = 1.6696d-24         ; (in grams)
        ;R = 1./6.02d-9
        ;Temp = 6.02d-9*Pres/Dens/1.6696d-24
        ;Temp = Pres/R/mp/Dens

        
        if ModPramsStruct.Bonly ne 0 then begin
           Temp = Temp*0. + ModPramsStruct.Isothermal
           Dens = DensbackHEonly
           Pres = Temp*2.*kboltz*Dens
;	   Density in bubble is a fraction of outside density
           if min(inside) ne -1 then Dens[inside] = ModPramsStruct.Bonly*Dens[inside]
           cavinside = where(Rcap lt ModPramsStruct.C2Bonly*rbub)
           if min(cavinside) ne -1 then Dens[cavinside] = ModPramsStruct.C1Bonly*Dens[cavinside]
        endif

	test=where(Temp*0. ne 0.)
        if min(Test) ne -1 then Temp[test] = 0.d0
        Bmag = sqrt(brphys*brphys+bthphys*bthphys+bphphys*bphphys)
        Ptot = Pres + Bmag*Bmag/(8.d0*!dPi)

; turn off outside if requested
	if bubbleonly eq 1 then begin
	   dens[outside]=1d-5
	   temp[outside]=1d-5
	   pres[outside]=1d-5
	   ptot[outside]=1d-5
	   brphys[outside]=0.
	   bthphys[outside]=0.
	   bphphys[outside]=0.
	   jrphys[outside]=0.
	   jthphys[outside]=0.
	   jphphys[outside]=0.
	   vr[outside]=0.
	endif
; now output variables requested

	ModSolStruct={Pres:Pres,Dens:Dens,Temp:Temp,Br:brphys,Bth:bthphys,Bph:bphphys,Vr:Vr,Vth:0.*Vr,Vph:0.*Vr,Jr:jrphys,Jth:jthphys,Jph:jphphys}

;
; if force with parameter velimpose
; then make velocity along field
;
	if ModPramsStruct.VelImpose ne 0. then begin
           Vel=ModPramsStruct.VelImpose+Pres*0.
           if bubbleonly eq 1 then vel[outside]=0.

	   ModSolStruct={Pres:Pres,Dens:Dens,Temp:Temp,Br:brphys,Bth:bthphys,Bph:bphphys,Vel:Vel,Jr:jrphys,Jth:jthphys,Jph:jphphys}

	endif


     end

