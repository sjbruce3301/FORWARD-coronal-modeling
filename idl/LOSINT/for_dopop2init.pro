;************************************************************************
     pro for_dopop2init,ObsPramsStruct,ModSolStruct,pop2tregime,nowidgmess=nowidgmess

;+
; Name: FOR_DOPOP2INIT
;
; Purpose:  
;	Set up so that able to calculate pop2losem,pop2colden, 
;       EUV emission with Lyman continuum absorption and white light images with cool 
;	chromospheric pop2 (pop2tregime=2), 
;       and added emission another coronal pop2 (pop2tregime=1)	
;
; INPUTS
;
;	ObsPramsStruct,ModSolStruct
;
;
; OUTPUTS:  pop2tregime
;
; Called by: FOR_INTENSINT
;
; HISTORY: 
;	Written by Terry Kucera, Sarah Gibson 2015-2016
;	Oct 2020 -- added WL variables XPOLF, XPOLB, XPOLG
;	Sep 2021 -- passed through nowidgmess
; January 2022 -- added TPOLF, TPOLB, TPOLG angular distance from TS functionality
;		fixed a bug in pop2check where condition was "ne" XPOLB instead of "eq"
;	changed to TPOLU, TPOLS
; Jul 2024 - added PR
;-

;Check if POP2 should be applied
;Pop2TRegime = 0 means No
;Pop2TRegime = 1 Yes, use (pop2dens,pop2temp),pop2fillingfactor;pop2abundance in another coronal population
;Pop2TRegime = 2 Yes, use pop2dens,pop2fillingfactor for Lyman Absorption and pB; pop2ionfrac for Lyman.
;Pop2TRegime = 3 Yes, set density in Pop2 points to very small (do not include in integral) 

        if tag_exist(ObsPramsStruct,'Pop2TRegime') then pop2tregime = ObsPramsStruct.Pop2TRegime $
                else pop2tregime =0

;
; check to see if model is forcing one regime or another
;

	if tag_exist(ModSolStruct,'RegimeForce') then begin
         if pop2tregime ne 2 and pop2tregime ne 0 and ModSolStruct.RegimeForce eq 2 then pop2tregime=2
         if pop2tregime ne 1 and pop2tregime ne 0 and ModSolStruct.RegimeForce eq 1 then pop2tregime=1
         if pop2tregime ne 3 and pop2tregime ne 0 and ModSolStruct.RegimeForce eq 3 then pop2tregime=3
        endif

;
; make sure requested observable is consistent
; with Pop2 choice
;
        if strupcase(ObsPramsStruct.Instrument) eq 'POP2LOSEM' or $
                strupcase(ObsPramsStruct.Instrument) eq 'POP2COLDEN' or $
                strupcase(ObsPramsStruct.Instrument) eq 'LOSEM' or $
                strupcase(ObsPramsStruct.Instrument) eq 'COLDEN' then begin
            if strupcase(ObsPramsStruct.Instrument) eq 'POP2LOSEM' or $
                strupcase(ObsPramsStruct.Instrument) eq 'POP2COLDEN'  then if pop2tregime eq 0 then pop2tregime=2
            if pop2tregime gt 0 and not (tag_exist(ModSolStruct,'POP2DENS') and tag_exist(ModSolStruct,'POP2FILLINGFACTOR')) then begin
               if keyword_set(nowidgmess) then message,/info,'Model does not have variables required Pop2: POP2DENS, POP2FILLINGFACTOR. Showing single population results. ' else d=dialog(/warning,'Model does not have variables required Pop2: POP2DENS, POP2FILLINGFACTOR. Showing single population results.')
               pop2tregime=0
            endif
        endif else begin
;
; make sure observable is appropriate to POP2 for other observables

           if pop2tregime ne 0 then begin
             if strupcase(ObsPramsStruct.IClass) ne 'UV/EUV SPECTROMETERS' and $
               strupcase(ObsPramsStruct.IClass) ne 'EUV/XRAY IMAGERS' and $
               strupcase(ObsPramsStruct.LineName) ne 'PB' and $ 
               strupcase(ObsPramsStruct.LineName) ne 'TB' and $ 
               strupcase(ObsPramsStruct.LineName) ne 'XPOLF' and $ 
               strupcase(ObsPramsStruct.LineName) ne 'XPOLB' and $ 
               strupcase(ObsPramsStruct.LineName) ne 'XPOLG' and $ 
               strupcase(ObsPramsStruct.LineName) ne 'PR' and $ 
               strupcase(ObsPramsStruct.LineName) ne 'TPOLU' and $ 
               strupcase(ObsPramsStruct.LineName) ne 'TPOLS' and $ 
               strupcase(ObsPramsStruct.LineName) ne 'TPOLG' and $ 
               strupcase(ObsPramsStruct.LineName) ne 'P' then begin
                 if keyword_set(nowidgmess) then message,/info,'Observable not implemented for second population. Showing single population results ' else d=dialog(/warning,'Observable not implemented for second population. Showing single population results.')
                pop2tregime=0
             endif
           endif
;
; make sure the model has provided what is needed for the requested regime
;

; for second coronal population, really needs a difference in abundance and/or temperature and specification of pop2 filling factor
;
           pop2check=0
           if pop2tregime eq 1 then begin
             abundance = ObsPramsStruct.SpecPrams.Abundance
             if tag_exist(ObsPramsStruct.SpecPrams,'Pop2Abundance') then begin
                pop2abundance = ObsPramsStruct.SpecPrams.Pop2Abundance 
             endif else begin
                ObsPramsStruct.SpecPrams=add_tag(ObsPramsStruct.SpecPrams,abundance,'Pop2Abundance')
                pop2abundance = ObsPramsStruct.SpecPrams.Pop2Abundance 
             endelse
             if pop2abundance ne abundance then pop2check=1
             if tag_exist(ModSolStruct,'POP2TEMP') then pop2check=1
             if strupcase(ObsPramsStruct.LineName) eq 'PB' or $ 
               strupcase(ObsPramsStruct.LineName) eq 'TB' or $ 
               strupcase(ObsPramsStruct.LineName) eq 'XPOLF' or $ 
               strupcase(ObsPramsStruct.LineName) eq 'XPOLB' and $ 
               strupcase(ObsPramsStruct.LineName) eq 'XPOLG' or $ 
               strupcase(ObsPramsStruct.LineName) eq 'PR' or $ 
               strupcase(ObsPramsStruct.LineName) eq 'TPOLU' or $ 
               strupcase(ObsPramsStruct.LineName) eq 'TPOLS' and $ 
               strupcase(ObsPramsStruct.LineName) eq 'TPOLG' or $ 
               strupcase(ObsPramsStruct.LineName) eq 'P' then $
                  if tag_exist(ModSolStruct,'POP2DENS') then pop2check=1
             if not tag_exist(ModSolStruct,'POP2FILLINGFACTOR') then pop2check=0
             if pop2check eq 0 then begin
                if keyword_set(nowidgmess) then message,/info,'Model does not have different temperature or abundance or filling factor as required Pop2TRegime 1: Coronal. Showing single population results. ' else d=dialog(/warning,'Model does not have different temperature or abundance or filling factor as required Pop2TRegime 1: Coronal. Showing single population results.')
                pop2tregime=0
             endif
           endif

; for second chromospheric population, needs a specification of pop2 density and filling factor
;  same for third zeroed out population
;
           if pop2tregime eq 2 or pop2tregime eq 3 then begin
             if not (tag_exist(ModSolStruct,'POP2DENS') and tag_exist(ModSolStruct,'POP2FILLINGFACTOR')) then begin
                if keyword_set(nowidgmess) then message,/info,'Model does not have variables required for Pop2TRegime 2 or 3: POP2DENS, POP2FILLINGFACTOR. Showing single population results' else d=dialog(/warning,'Model does not have variables required for Pop2TRegime 2 or 3: POP2DENS, POP2FILLINGFACTOR. Showing single population results.')
                pop2tregime=0
             endif
           endif
	endelse

;
; special case model forcing pop2tregime -- change throughout
; note, dont change throughout if one of the other things above changed
; value of pop2tregime -- these all come with their own warnings
; and will continue to do so unless the user changes pop2tregime themselves
; but the RegimeForce requires no user intervention, so needs to be reported
;
       if tag_exist(ObsPramsStruct,'Pop2TRegime') then begin
         if tag_exist(ModSolStruct,'RegimeForce') then begin
          if ObsPramsStruct.Pop2TRegime ne 0 and pop2tregime ne 0 then begin
            if ObsPramsStruct.Pop2TRegime ne ModSolStruct.RegimeForce then begin
             if keyword_set(nowidgmess) then message,/info,'POP2TREGIME changed to '+strtrim(string(ModSolStruct.RegimeForce),2)+' as required by model.' else d=dialog(/warning,'POP2TREGIME changed to '+strtrim(string(ModSolStruct.RegimeForce),2)+' as required by model.')
             ObsPramsStruct.Pop2TRegime = ModSolStruct.RegimeForce
           endif
          endif
         endif
       endif
end
