pro for_forwardmap,Quantmap,ModPramsStruct,GridPramsStruct,ObsPramsStruct,LosPramsStruct,$
        bg_input=bg_input,bg_output=bg_output,$
	StokesStruct,WLStruct,ModSolStruct,nreinit,reinit,memory,datadump,extratitle,working_dir,parallel=parallel,verbose=verbose,nowidgmess=nowidgmess,mapname=mapname

;+
;  Name:  FOR_FORWARDMAP
;
;  Program to create map and associated structure for forward calculation
;
;	INPUTS
;		GridPramsStruct,ModPramsStruct,ObsPramsStruct,LosPramsstruct
;		nreinit, reinit,memory,datadump,working_dir (see for_settingdefaults.pro for descriptions)
;		extratitle (see for_outputdefaults)
;
;       INPUT Keywords
;		parallel (see for_settingdefaults)
;
;	OUTPUTS
;		QuantMap,ModSolStruct, StokesStuct (for integrated case, comp and fe11comp,si9comp, si10comp,cormag, and wavelength resolved case wavecomp)
;
;  see FOR_SETTINGDEFAULT for description of other keywords
;
; Called by FOR_DRIVE 
; 
; Calls FOR_POSNOINT, FOR_INTENSINT, FOR_POS_MAP
; 
; Written by Sarah Gibson, Terry Kucera, 2010-2014
; Version 2.0 July 2014
;	November 2021 -- passed mapname through to for_intensint
;
;-
; March 2016 -- updated NINST bug fix - now pass through deltar to FOR_POSNOINT and let
;		it calculate spherical coordinates.
;
; June 2019 -- updated NINST bug fix -- now field is BHOR
;		sqrt(Bth^2+Bph^2)
; Sept 2021  - passed through nowidgmess
; Oct 2021 - allowed user inputted losint < .001 for POS
; Feb 2022 -- passed through bg_input,bg_output
; Mar 2022 -- allowed for TLOS
; Apr 2022 -- didn't limit user inputted losint below .001 Rsun
;			(tlos might want bigger)
;		passed working_dir through to for_posnoint
; Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
; Jul 2023 -- added IONDENS
; Sep 2023 -- added AWSOM hooks
; Aug 2024 -- added WLRAT hooks


;
; special case, line=NINST, requires derivative
; and model='PFSSMOD' or 'NUMCUBE' or 'ADAPTCUBE'
; 
;  NOTE - the derivative must take place in spherical coords
;  and should be independent of observer position
;

    if strupcase(ObsPramsStruct.LineName) eq 'NINST' then begin
     if strupcase(ModPramsStruct.Name) eq 'PFSSMOD' or strupcase(ModPramsStruct.Name) eq 'NUMCUBE' or strupcase(ModPramsStruct.Name) eq 'ADAPTCUBE' or strupcase(ModPramsStruct.Name) eq 'PSIMAS' or strupcase(ModPramsStruct.Name) eq 'AWSOM' then begin
       ObsPramsStruct.LineName = 'BHOR'
       for_posnoint,quantity,GridPramsStruct.Rpos,GridPramsSTruct.THpos,GridPramsStruct.PHpos,$
		LosPramsStruct,ObsPramsStruct,ModPramsStruct,GridPramsStruct,ModSolStruct,$
		nreinit=nreinit,deltar=ModPramsStruct.Deltar,rdriv=rdriv,nowidgmess=nowidgmess,working_dir=working_dir
       ndriv1=quantity
       rdriv1=rdriv

       for_posnoint,quantity,GridPramsStruct.Rpos,GridPramsSTruct.THpos,GridPramsStruct.PHpos,$
		LosPramsStruct,ObsPramsStruct,ModPramsStruct,GridPramsStruct,ModSolStruct,$
		nreinit=nreinit,deltar=-ModPramsStruct.Deltar,rdriv=rdriv,working_dir=working_dir

       rdriv2=rdriv 
;
; not sure this next abovedisk test is necessary since quantity
; is only calculated if it passes test for rdriv2 gt 0 below
;
       abovedisk=where(rdriv2 ge 0.d0)
       if min(abovedisk) ne -1 then ndriv2=quantity else ndriv2 = 0.d0*GridPramsStruct.Rpos

       quantity=quantity*0.d0
       if strupcase(ModPramsStruct.Name) eq 'NUMCUBE' or strupcase(ModPramsStruct.Name) eq 'ADAPTCUBE' or strupcase(ModPramsStruct.Name) eq 'PSIMAS' or strupcase(ModPramsStruct.Name) eq 'AWSOM' then test=where(ndriv1 ne 0. and ndriv2 ne 0. and rdriv1 gt 0. and rdriv2 gt 0.)
       if strupcase(ModPramsStruct.Name) eq 'PFSSMOD' then begin
  	  rtop = abs(ModPramsStruct.RTop)-1.d0 
	  test=where(ndriv1 ne 0. and ndriv2 ne 0. and rdriv1 gt 0. and rdriv2 gt 0. and rdriv1 le rtop and rdriv2 le rtop)
       endif
       if min(test) ne -1 then quantity[test] = -1.d0*(alog(ndriv1[test])-alog(ndriv2[test]))/(alog(rdriv1[test])-alog(rdriv2[test]))

       ObsPramsStruct.LineName = 'NINST'
       quantsave=quantity

     endif else print,'must use PFSSMOD or NUMCUBE/ADAPTCUBE (or PSIMAS or AWSOM) model'
    endif 

;
; calculate plane of sky even if doing full integral, so can
; keep a copy of the POS ModSolStruct with dimensions as notdisk in for_posnoint
;
; if integral or 'ninst', though, pass in dummy 'dens' for linename
; unless 'iondens'

    linesave=ObsPramsStruct.LineName
    if (ObsPramsStruct.LineNum ne -99 or strupcase(ObsPramsStruct.LineName) eq 'NINST') and (strupcase(ObsPramsStruct.Instrument) ne 'IONDENS') then ObsPramsStruct.LineName='dens'

    for_posnoint,quantity,GridPramsStruct.Rpos,GridPramsSTruct.THpos,GridPramsStruct.PHpos,$
		LosPramsStruct,ObsPramsStruct,ModPramsStruct,GridPramsStruct,ModSolStruct,$
		nreinit=nreinit,working_dir=working_dir

;
; return original linename 
    ObsPramsStruct.LineName=linesave

;
; replace quantity for special case 'NINST'
;

    if strupcase(ObsPramsStruct.LineName) eq 'NINST' then quantity=quantsave

;
; Run the forward model for integral cases
;

if ObsPramsStruct.LineNum ne -99 and ObsPramsStruct.Instrument ne 'IONDENS' then begin

; special case, observable in plane of sky (or Thomson Sphere)

	memuse=memory
  	if abs(ObsPramsStruct.Pos) gt 0 then begin
	   if strupcase(LosPramsStruct.LosUse) eq 'TAU' $
	     or strupcase(LosPramsStruct.LosUse) eq 'NULL' $
             then LosPramsStruct.LosUse = 'TLOS'
           LosPramsStruct.NLos = 2
;
; allowing user full control over step size but making default .001 in for_losdefaults
;
;           LosPramsStruct.LosInt = LosPramsStruct.LosInt < 0.001d0
           LosPramsStruct.LosMin = -1.*LosPramsStruct.LosInt/2.d0
	   memuse=0
	endif

     	for_intensint,quantity,GridPramsStruct.Rpos,GridPramsStruct.THpos,GridPramsStruct.PHpos,$
      	        LosPramsStruct,ObsPramsStruct,ModPramsStruct,GridPramsStruct,datadump,StokesStruct,WLStruct,$
                bg_input=bg_input,bg_output=bg_output,$
		reinit=reinit,nreinit=nreinit,memory=memuse,verbose=verbose,working_dir=working_dir,parallel=parallel,nowidgmess=nowidgmess,mapname=mapname

endif
;
; Make map
  
	QuantMap=for_pos_map(quantity,ModPramsStruct,ObsPramsStruct,LosPramsStruct,GridPramsStruct,StokesStruct=StokesStruct,$
                      extratitle)
  
    
end
