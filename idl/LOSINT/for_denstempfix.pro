;************************************************************************
     pro for_denstempfix,ModPramsStruct,ObsPramsStruct,ModSolStruct,denssign,doprint

;+
; Name: FOR_DENSTEMPFIX
;
; Purpose:  Fix negative densities
;
; INPUTS	ModPramsStruct,ObsPramsStruct,ModSolStruct
;
;
;
; OUTPUTS 	ModSolStruct,denssign 
;
; Called by: FOR_INTENSINT
;
;
; HISTORY: 
;	Written by Sarah Gibson 2015
;
;  Feb 3 2016: got rid of possibility of NaNs from zero density SEG
;  Jun 6 2016; moved correction to PSIMAS transition region values into PSIMAS.PRO
;  October 2021; added doprint 
;-

;
; deal with negative densities
;
          denssign=ModSolStruct.Dens*0.+1.
          testdens = where(ModSolStruct.Dens lt 0.,c)
          if c gt 0 then begin                          ;for anything except one of Don's line ratios or COMP
            IF strupcase(ObsPramsStruct.LineName) ne 'FE12DE' and strupcase(ObsPramsStruct.LineName) ne  'FE12RATIO' $
                and strpos(strupcase(ObsPramsStruct.Instrument),'OMP') lt 0 and strupcase(ObsPramsStruct.Instrument) ne 'CORMAG' and strupcase(ObsPramsStruct.IClass) ne 'UV SPECTROPOLARIMETERS' and strupcase(ObsPramsStruct.IClass) ne 'RADIO' then begin
                                ModSolStruct.Dens[testdens]=-1.*ModSolStruct.Dens[testdens]
                                if doprint eq 1 then message,"WARNING - negative densities -- white light and emission will add negative intensity components to total integral.",/info
                endif else begin                                ;for one of Don's line ratios or COMP
                        ModSolStruct.Dens[testdens]=1d-4
;
; note we do not use exactly zero because otherwise it will be treated as null data in for_pos_map
;
                        message,"WARNING - negative densities -- EIS line ratio using Don's code and polarimetric data  will replace with zeros.",/info
                endelse
            denssign[testdens] = -1.
            endif

end

