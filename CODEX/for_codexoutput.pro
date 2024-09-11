
;************************************************************************
     pro for_codexoutput,ObsPramsStruct,ModSolStruct,ModPramsStruct,LosPramsStruct,rmod,thmod,phmod,r3D,theta3D,phi3D,cmer2,intensbite,wlspecbite,wavelengths

;+c
; Name: FOR_CODEXOUTPUT
;
; Purpose:  output codex WL spectral information 
;
; INPUTS ObsPramsStruct,ModSolStruct,ModPramsStruct,LosPramsStruct,r3D,theta3D,phi3D
;	rmod,thmod,phmod,cmer2 
;
;        input dimension [nlos,nposbite]
;
; OUTPUTS 
;	 INTENSBITE -- WL ratio array nposbite
; 	 WLSPECBITE -- array nwavelengths,nposbite
; 	 WAVELENGTHS -- array nwavelengths -- wavelengths in Angstrom
;	
;
; Called by: FOR_INTENSINT
;
; Calls: FOR_FIELDCALLS, FORCODEX
;
; HISTORY: 
;	Written by Sarah Gibson 2024
;-

; calculate line-of-sight velocity from model quantities

	        for_fieldcalls,rmod,thmod,phmod,cmer2,ModSolStruct,ModPramsStruct,LosPramsStruct,Brobs,Bthobs,Bphobs,VelR,VelTh,VelPh
 
;
; For Doppler velocity, need line-of-sight which is Vx

                Vx= VelR*sin(theta3D)*cos(phi3D)+$
                        Velth*cos(theta3D)*cos(phi3D)-$
                        Velph*sin(phi3D)
;
		WL1=ObsPramsStruct.Wavelength_Ang
		WL2=ObsPramsStruct.Wavelength2_Ang
                forcodex,double(r3D),double(ModSolStruct.Dens),double(ModSolStruct.Temp),double(Vx),WL1,WL2,wlratbite,wlspecbite,wavelengths
		intensbite=wlratbite
	        
end
;************************************************************************
;
; inputs to CRAM: [N, T, V, r]  X NBITE 3D array
;  DEFAULT wavelengths for ratio [ratWL1, ratWL2] -- 4000,4100  Angstrom - each wavelength has its own limb darkening coefficient
;   3500-4500A, stpes of 1 angstroms
  ;velocity introduces shift in scattering wavelength.
  ;
; ADD WLRAT to PB Observables
;   if this is requested, triggers calls to this code
;
; Outputs from CRAM:
;  WLwave (spectrum itself) [NPOS dimension array -- integration along NLOS done by CRAM]
; 
;  MAKE in this code:
;   WLratio temperature sensitive ratio of intensities, at ratWL1, ratWL2 [NPOS dimension array]
;   ultimately might also do a velocity senstive ratio
; 
; FORWARD will calculate pB and TB using pBCalc

; make structure eventually?
;      WLSpecStruct = {pB:pB,TB:TB,WLrat:WLrat,WLwave:WLwave}
; note -- maybe only include pB, TB for data CODEX not model
;
; spectrum optional like comp
