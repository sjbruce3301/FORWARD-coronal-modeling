	pro pointval,rpB,thetapB,phipB,ModPramsStruct,ModSolStruct

;
; Name: POINTVAL
;
; Purpose: Spit out the density, temperature, magnetic field sset in POINTVALPRAMS
;
; Inputs: 
;	   rpB, thetapB, phipB -- position in 3D space where model is to be evaluated
;				r in units of RSUN, th, ph in RADIANS
;
;          ModPramsStruct - structure associated with model, containing
;                           model name (POINTVAL), model parameters
;			    set up in giblowprams.pro
;
; Outputs: ModSolStruct - Solution of model, containing density,temperature,
;                         pressure, magnetic field (Br, Bth, Bph),current, and velocity
;
;  Written: S. Gibson 
;

; get dimensions right

  	emptyarr=rpB*0. + 1.

; Read in Parameters:

        brphys=ModPramsStruct.BrVal*emptyarr
        bthphys=ModPramsStruct.BthVal*emptyarr
        bphphys=ModPramsStruct.BphVal*emptyarr
	Dens=ModPramsStruct.Densval*emptyarr
	Temp=ModPramsStruct.Tempval*emptyarr

; calculate Pressure

        mp = 1.6696d-24         ; (in grams)
        R = 1./6.02d-9
        Pres = Temp*R*mp*Dens

        Bmag = sqrt(brphys*brphys+bthphys*bthphys+bphphys*bphphys)
        Ptot = Pres + Bmag*Bmag/(8.d0*!dPi)

; now output variables requested

	ModSolStruct={Pres:Pres,Dens:Dens,Temp:Temp,Br:brphys,Bth:bthphys,Bph:bphphys}

     end

