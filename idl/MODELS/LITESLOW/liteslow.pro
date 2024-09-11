	pro liteslow,rpB,thetapB,phipB,ModPramsStruct,ModSolStruct

	common llshiftcoord,Rcap,Thcap,Phcap
	common llprams,xo,rbub,ao
	common llprams2,aa,bb,cc,dd,ee,ff,alnot,sig1,out
	common llresults,bstrengthphys,brphys,bthphys,bphphys

;
; Name: LITESLOW
;
; Purpose: Calculate the density, temperature, magnetic field of the
;          LITES-Low model at a location rpB, thetapB, phipB (true spherical
;          coordinates)
;
; Inputs: 
;	   rpB, thetapB, phipB -- position in 3D space where model is to be evaluated
;				rpB in units of RSUN, thetapB, phipB in units of RADIANS
;
;          ModPramsStruct - structure associated with model, containing
;                           model name (GIBLOW), model parameters
;			    set up in giblowprams.pro
;
; Outputs: ModSolStruct - Solution of model, containing density,temperature
;                         pressure, magnetic field (Br, Bth, Bph), and velocity
;
; Common Blocks: transcoord, shiftcoord, sscoord, prams, prams2, results, jresults, 
;		  fieldprams, outblock, inblock, background
;
; Calls litescoords, litesfieldcalc
;
;  Written: JD (Adopted from Giblow.pro) 
;
; Version 2.0 July 2014
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)


; Read in Parameters:

        xo = ModPramsStruct.Xo
        rbub = ModPramsStruct.Rbub
        ao = ModPramsStruct.Ao
        alnot = ModPramsStruct.Alnot
        sig1= ModPramsStruct.Sig1
        out = ModPramsStruct.Out
        densscale = ModPramsStruct.Densscale
        aa = ModPramsStruct.AA
        bb = ModPramsStruct.BB
        cc = ModPramsStruct.CC
        dd = ModPramsStruct.DD
        ee = ModPramsStruct.EE
        ff = ModPramsStruct.FF

;
;  first shift coordinate systems
;
        litescoords,rpB,thetapB,phipB
;
;  now calculate the field
;
        litesfieldcalc,rpB,thetapB,phipB

; 
;  for inside the sun (to avoid unrealistic density profile for self-similarly expanding solutions
;  really only used for Gibson-Low
;
        densin=(aa+cc+ee)*rpB^(-3.)

; 
;  outside the sun, r > 1 for a non-time-varying solution
;
        densout=(aa*rpB^(-bb)+cc*rpB^(-dd)+ee*rpB^(-ff))

        Dens=densin*0.d0

        testin=where(rpB lt 1.)
        testout=where(rpB ge 1.)

        if min(testin ne -1) then Dens[testin]=densin[testin]
        if min(testout ne -1) then Dens[testout]=densout[testout]
;
;  allow the option of having a different density
;  outside the bubble
;
	notbub = where(Rcap ge rbub)
	if min(notbub) ne -1 then Dens[notbub]=densscale*dens[notbub]

        mp = 1.6696d-24         ; (in grams)
        R = 1./6.02d-9

        Temp = Dens*0. + 1.d6
        Pres = R*Temp*mp*Dens
        Vel = 0.0*Dens + ModPramsStruct.VelImpose

; now output variables requested

	ModSolStruct={Pres:Pres,Dens:Dens,Temp:Temp,Br:brphys,Bth:bthphys,Bph:bphphys,Vel:Vel}

     end

