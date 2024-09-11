;************************************************************************
     pro for_polstruc,ObsPramsStruct,LosPramsStruct,notdisk,nunwrapall,CoordSize,NDim,stokesred,StokesStruct

;+
; Name: FOR_POLSTRUC
;
; Purpose:   make Stokes structure for radio and uv 
;
; INPUTS	ObsPramsStruct,LosPramsStruct,notdisk,nunwrapall,CoordSize,NDim,stokesred
;
; OUTPUTS StokesStruct
;
; Called by: FOR_INTENSINT
;
;
; HISTORY: 
;	Written by Sarah Gibson 2015
;	 May 2022 -- updated equivalent width UV
;        July 2022 -- allowed notdisk=-2,-3		
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;	Jan 2023-- now carry UV SPECPOL wavelength in ObsStruct Wavelength_ANG not FREQUENCY_MHZ
;-

         StokesVall=dblarr(nunwrapall)
         StokesIall=dblarr(nunwrapall)
         StokesQall=dblarr(nunwrapall)
         StokesUall=dblarr(nunwrapall)

         stokesI=stokesred[0,*]
         if min(notdisk) ge 0 then stokesIall[notdisk]=stokesI else stokesIall = stokesI
         stokesIall=LosPramsStruct.AxiSymMult*reform(stokesIall,CoordSize[1:NDim],/overwrite)

;
; note here Faraday RM is being stored in stokesI
;       and FR in stokesV
;
         if (strupcase(ObsPramsStruct.Instrument) eq 'RADIO' or $
                strupcase(ObsPramsStruct.Instrument) eq 'FARADAY') then begin
                stokesV=stokesred[1,*]

                if min(notdisk) ge 0 then stokesVall[notdisk]=stokesV else stokesVall = stokesV
                stokesVall=LosPramsStruct.AxiSymMult*reform(stokesVall,CoordSize[1:NDim],/overwrite)
                StokesStruct = {I:StokesIall,V:StokesVall,CentWave:ObsPramsStruct.Frequency_MHz,CentI:StokesIall}


;
; note CentI is the same as I - be careful
;
         endif

         if strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' then begin
                stokesQ=stokesred[1,*]
                stokesU=stokesred[2,*]

                if min(notdisk) ge 0 then stokesQall[notdisk]=stokesQ else stokesQall = stokesQ
                stokesQall=LosPramsStruct.AxiSymMult*reform(stokesQall,CoordSize[1:NDim],/overwrite)
                if min(notdisk) ge 0 then stokesUall[notdisk]=stokesU else stokesUall = stokesU
                stokesUall=LosPramsStruct.AxiSymMult*reform(stokesUall,CoordSize[1:NDim],/overwrite)
		centwave=StokesIall*0.+ObsPramsStruct.Wavelength_Ang

		eqwidth=ObsPramsStruct.SpecPrams.LWidth
		centi=StokesIall/eqwidth
;
; equivalent width of the coronal line is the rectangle to integrate over
; when comparing to the sun central brightness at that wavelength
;
; This is just a convenience to pass line width through
; Thus, CentI is not really meaningful in itself

                StokesStruct = {I:StokesIall,Q:StokesQall,U:StokesUall,CentWave:centwave,CentI:centi}
         endif
end
