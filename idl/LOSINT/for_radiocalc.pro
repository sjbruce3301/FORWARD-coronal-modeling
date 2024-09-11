;************************************************************************
     pro for_radiocalc,ObsPramsStruct,ModSolStruct,ModPramsStruct,LosPramsStruct,rmod,thmod,phmod,r3D,theta3D,phi3D,dlos,cmer2,testnearuse,radiobite

;+
; Name: FOR_RadioCalc
;
; Purpose:  determine Radio Bremstrahllung, with or without gyroresonance, or
;	Faraday rotation  information for points along LOS
;	*****NOTE -- no ray tracing is done in these calculations,
;	*****  at low frequencies, refraction can be important so
;	*****  this needs to be considered.  
;	*****  The opacity calculation begins at the height of
;	*****   the plasma layer associated with a particular frequency
;	*****  Any material along the line of sight with a density larger than
;	***** that associated with the plasma frequency is considered
;	***** to block background emission.
;
; INPUTS  ObsPramsStruct,ModSolStruct,ModPramsStruct,LosPramsStruct,r3D,theta3D,phi3D,testnearuse
;	rmod,thmod,phmod,dlos,cmer2  (note these are in same dimensions
;               as ModSolStruct.Dens, either 1D, or like r3D
;
; OUTPUTS radiobite
; 	note this is in same dimension as r3D, so [nlos,nactualbite]
;
;	If instrument=RADIO, returns Stokes I, V
;	If instrument=FARADAY, returns maps of RM rotation measure (rad/m^2) and
;	FR Faraday rotation of plane of linear polarization (radian, depends on wavelength)
;
;       as 2 X POS array, or (not integrated) 2 X POS,LOS arrays for Faraday
;		(unless testnearuse set, in which case Faraday will be 2D)

; Called by: FOR_INTENSINT
;
; Calls:  FOR_FIELDCALLS, FOR_RADIOMODEL
;
;
; HISTORY: 
;	Written by Sarah Gibson 2015
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;	Oct 2023 -- updated Rsun
;-

;
; deal with possibility of 1D array of different dimension that r3D
;
; note this should not happen for instrument=RADIO, but could FARADAY
;  I have kept it set up for RADIO to work in future, but will require
;  changes to FOR_RADIOMODEL, FOR_INTENSINT, and to FOR_INTLOS 
;

        if min(testnearuse) ne -1 then begin
           r3Duse= r3D[testnearuse]
           theta3Duse= theta3D[testnearuse]
           phi3Duse= phi3D[testnearuse]
        endif else begin
           r3Duse= r3D
           theta3Duse=theta3D
           phi3Duse= phi3D
        endelse

        for_fieldcalls,rmod,thmod,phmod,cmer2,ModSolStruct,ModPramsStruct,LosPramsStruct,Brobs,Bthobs,Bphobs,VelR,VelTh,VelPh

	if strupcase(ObsPramsStruct.Instrument) ne 'FARADAY' then begin
          radiobite=for_radiomodel(ObsPramsStruct,dlos,$
             double(r3Duse),double(theta3Duse),double(phi3Duse),$
             double(Brobs),double(Bthobs),double(Bphobs),$
             double(ModSolStruct.Dens),double(ModSolStruct.Temp))
	endif else begin
	  blos = sin(theta3Duse)*cos(phi3Duse)*brobs+$
       	   cos(theta3Duse)*cos(phi3Duse)*bthobs-$
       	   sin(phi3Duse)*bphobs
; RM -- radians per square meter
	  iv1=ModSolStruct.Dens*blos*2.6e-13
; multiply by wavelength^2 to get FR
; freq in Hz
	  freq=double(ObsPramsStruct.Frequency_MHz)*1d6
	  iv2=iv1*((3.0e8/freq)^2)
	endelse

;
; make sure radiobite are in same dimensions as 2 X r3D
;

	nlos = n_elements(r3D[*,0])
	npos = n_elements(r3D[0,*])
	radiobitereal=dblarr(2,nlos,npos)

        Rsun2cm=6.957e10 
        if min(testnearuse) ne -1 then begin
	 radiobitereal1=r3D*0.
	 radiobitereal2=r3D*0.
	 if strupcase(ObsPramsStruct.Instrument) ne 'FARADAY' then begin
	  radiobitereal1[testnearuse]=radiobite[0,*]
	  radiobitereal2[testnearuse]=radiobite[1,*]
	 endif else begin
	  radiobitereal1[testnearuse]=iv1*Rsun2cm
	  radiobitereal2[testnearuse]=iv2*Rsun2cm
	 endelse
	 radiobitereal[0,*,*]=radiobitereal1
	 radiobitereal[1,*,*]=radiobitereal2
	 radiobite=radiobitereal
	endif else begin
	 if strupcase(ObsPramsStruct.Instrument) eq 'FARADAY' then begin
	  radiobitereal[0,*,*]=iv1*Rsun2cm
	  radiobitereal[1,*,*]=iv2*Rsun2cm
	  radiobite=radiobitereal
	 endif
        endelse
end
