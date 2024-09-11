;************************************************************************
     pro for_euvsxrcalc,ObsPramsStruct,ModSolStruct,pop2tregime,r3D,dlos,denssign,testnearuse,reinit,emint,pop2emint,sumB,LyC_reinit=LyC_reinit,nowidgmess=nowidgmess

;+
; Name: FOR_EUVSXRCALC
;
; Purpose:  determine EUV, SXR emission for points along LOS
;
; INPUTS  ObsPramsStruct,ModSolStruct,pop2tregime,r3D,dlos,denssign,testnearuse,reinit,LyC_reinit
;  	 (note these are in same dimensions
;               as ModSolStruct.Dens, either 1D, or like r3D
;
;  Keyword Input	
;	   LyC_reinit- this keeps track of whether Lyman calculation has happened yet
;		if so, goes to zero -- initial (default, set below) value is 1
;
; OUTPUTS  sumB,emint,pop2emint
;               note these are in same dimension as r3D, so [nlos,nactualbite]
;
; Called by: FOR_INTENSINT
;
; Calls: FOR_LINE_MFLUX, FOR_EISRATIO_FORWARD, FOR_EUV_XRAY_IMAGER_FLUX,FOR_LYCONTABS
;
; HISTORY: 
;	Written by Terry Kucera, Sarah Gibson 2015
;-
;  	Feb 2016: Updated for Pop2 consistency  SEG
;	   also reinit consistency
;		set up POP2TREGIME=1 calls
;	Sep 2021: changed inst to instr because of potential for confusion with system var
;		passed through nowidgmess
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
; 	Oct 2023 -- removed NaN dealing points -- put into for_integrate
;		updated Rsun

;
; deal with possibility of 1D array of different dimension that r3D
;

        if min(testnearuse) ne -1 then r3Duse= r3D[testnearuse] else r3Duse=r3D

;      Reinit for Lyman continuum (used if pop2tregime=2). 
;		Should start at 1, and be reset to 0 after first time calculated

	default,LyC_reinit,1

;
; 	But be careful about pop2tregime=1 -- in this case
;		response functions should be calculated every time
;

	usereinit=reinit
	if pop2tregime eq 1 then usereinit=1

;
; set up filling factor to be one if not set
; note, FOR_DOPOP2INIT will have ensured pop2 filling factor is set
;  if operating in pop2tregime other than 0
;

        fill=1.
        if tag_exist(ModSolStruct,'FillingFactor') then fill=ModSolStruct.FillingFactor

;	First calculate emission measure

        RSun_cm = 6.95700d+10  ;solar radius in cm

        emint = ModSolStruct.Dens^2*denssign*fill*RSun_cm

;
; zero out pop2 density for pop2tregime=3
;
        if pop2tregime eq 3 then begin
          if tag_exist(ModSolStruct,'Pop2Dens') then ModSolStruct.Pop2Dens = 1d-5
        endif

        if pop2tregime gt 0 then begin
          if tag_exist(ModSolStruct,'Pop2Dens') then begin
            pop2emint =  ModSolStruct.Pop2Dens^2*denssign*ModSolStruct.Pop2FillingFactor*RSun_cm
          endif else begin
            pop2emint =  ModSolStruct.Dens^2*denssign*ModSolStruct.Pop2FillingFactor*RSun_cm
          endelse
        endif else pop2emint=emint

	sumB=emint*0.d0

        if strupcase(ObsPramsStruct.IClass) ne 'NONE' then instr=ObsPramsStruct.ICLass else instr=ObsPramsStruct.Instrument

; now calculate EUV lines for Population 1

        case strupcase(instr) of
                'UV/EUV SPECTROMETERS': sumB=for_line_mflux(ObsPramsStruct, $
                                alog10(ModSolStruct.Temp),emint,ModSolStruct.Dens,r3Duse,reinit=usereinit,nowidgmess=nowidgmess)
                'EISRATIO': begin
;Don Schmit's Fe XII line ratio calculations
; note for 'fe12de' and 'fe12ratio' eis will be
;a one-d array(nactualbite), and won't need summing below
                   case strupcase(ObsPramsStruct.LineName) of                    
                            'FE12DE' :sumB=for_eisratio_forward(ModSolStruct.dens,dlos,16)
                            'FE12RATIO': sumB=for_eisratio_forward(ModSolStruct.dens,dlos,17)
                                 else: message,'This should not happen'
                   endcase
                 end
                'EUV/XRAY IMAGERS': begin
                        sumB = for_euv_xray_imager_flux(ObsPramsStruct,$
                                alog10(ModSolStruct.Temp),emint,alog10(ModSolStruct.Dens),$
                                                        reinit=usereinit,nowidgmess=nowidgmess)
                end
                'VISIBLE SPECTROMETERS': message,'Spectral Lines including scattering not yet implemented'
                'SXT': message,'SXT not actually implemented. Use XRT for approximation'
                Else:
        endcase

;
; deal with Pop 2
;

; pop2tregime=1 (second coronal population)
; need to pass pop2emint and ModSolStruct.Pop2Temp and pop2abundance into the various emissivity calculations 
; as a second calculation, and then sum the two populations for all points sumB
;
        if pop2tregime eq 1 then if max (ModSolStruct.Pop2FillingFactor) gt 0  then begin
           sumB2=0.
           if tag_exist(ModSolStruct,'POP2TEMP') then pop2temp=ModSolStruct.Pop2Temp else pop2temp=ModSolStruct.Temp 
           if tag_exist(ModSolStruct,'POP2DENS') then pop2dens=ModSolStruct.Pop2Dens else pop2dens=ModSolStruct.Dens
           case strupcase(instr) of
                'UV/EUV SPECTROMETERS': begin
                        sumB2=for_line_mflux(ObsPramsStruct, $
                                alog10(pop2temp),pop2emint,pop2dens,r3Duse,reinit=1,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
                end
                'EUV/XRAY IMAGERS': begin
                        sumB2 = for_euv_xray_imager_flux(ObsPramsStruct,$
                                alog10(pop2temp),pop2emint,alog10(pop2dens),$
                                                        reinit=1,pop2tregime=pop2tregime,nowidgmess=nowidgmess)
                end
                Else:
           endcase
           if exist(sumB) then sumB=sumB+sumB2
          endif

; pop2emint acts as a cool material emission proxy 
; for pop2tregime=2
;
          if pop2tregime eq 2 then if max(ModSolStruct.Pop2Dens) gt 0 and $
                    max (ModSolStruct.Pop2FillingFactor) gt 0  then begin
            if  (strupcase(ObsPramsStruct.IClass) eq 'EUV/XRAY IMAGERS' or strupcase(ObsPramsStruct.IClass) eq 'UV/EUV SPECTROMETERS') then begin

;multiply by Lyman Continuum Absorption
;so far only make happen for EUV imagers and Spectographs
;even then will have to separate out the xray images which don't list numerical wavelengths

                         LC_absorp=$
                            for_LyContAbs(ObsPramsStruct,dlos*ModSolStruct.Pop2FillingFactor,ModSolStruct.Pop2Dens,reinit=LyC_reinit)
                         sumB=sumB*LC_absorp
		         LyC_reinit=0
            endif
          endif

;
; make sure sumB and emint and pop2emint are in same dimensions as r3D
;

                if min(testnearuse) ne -1 then begin
                   sumBreal=r3D*0.
                   emintreal=r3D*0.
                   pop2emintreal=r3D*0.
                   sumBreal[testnearuse]=sumB
                   emintreal[testnearuse]=emint
                   pop2emintreal[testnearuse]=pop2emint
                   sumB=sumBreal
		   emint=emintreal
		   pop2emint=pop2emintreal
                endif

end
