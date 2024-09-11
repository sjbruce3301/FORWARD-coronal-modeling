pro for_iondens,ObsPramsStruct,ModSolStruct,ModPramsStruct,r3D,r,iondens,nowidgmess=nowidgmess

;+                   
; Name: FOR_IONDENS
;                
; Purpose:  calculate ion density from line information and
;	 	electron density
;                
; INPUT    
;           
;      ObsPramsStruct (with line info)
;	ModSolStruct (with density,temperature info for electonr)
;	ModPramsStruct (for model name)
;	r3D (for test of transition region)
;	r (for adding chromodens only on disk)
;      
;	Nowidgmess-- no widget text output
;
; OUTPUT
;	Ion density
;
; Calls FOR_IDSET
; Called by FOR_POSNOINT
;
; Oct 2023 -- added check for PSI threshold temperatures and ModPramsStruct pass through
;		changed below transition region points to NaNs
;		and passed through r3D and r 
;		and chromodens keyword to add chromodens 
; Feb 2024 -- fixed bug where testtrans could be -1


 logT=alog10(ModSolStruct.Temp)
 Dens=ModSolStruct.Dens

 for_idset,ObsPramsStruct,LogT,dens,iondens,chromodens,nowidgmess=nowidgmess

 if strupcase(ModPramsStruct.name) eq 'PSIMAS' or strupcase(ModPramsStruct.name) eq 'AWSOM' then begin
;
; deal with overbroad transition region in PSI MAS Model
; points less than 500000K should be ignored
;  IN THE NEAR FIELD!!! not the far field
; we will accomplish this by setting density zero there
; note that chromodens will be added on the disk

  testtrans=where(ModSolStruct.Temp lt 5d5 and r3D lt 5.,c)
  if c ne 0 then begin
   iondens[testtrans]=0.
   if ObsPramsStruct.SpecPrams.ChromoRad lt 0. then iondens[testtrans]=sqrt(-1.)
  endif
 endif

 testdisk=where(r le 1)
 iondens[testdisk]=iondens[testdisk]+chromodens

end
