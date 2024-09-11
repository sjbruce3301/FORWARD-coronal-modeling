PRO PSIMAS,r,theta,phi,ModPramsStruct,ModSolStruct,nreinit=nreinit

;+
; Name:
;      PSIMAS
;
; Calls NUMCUBE for PSI MAS datacube
;
; Written Sarah Gibson
; Version 2.0 July 2014
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;	Nov 2022 -- fixed bug where density set to 1d-4 in far
;		field because of temperature dropoff
;	Oct 2023 -- replaced small densities in the transition region
;		with NaNs so that in plots it is clear they are unused points
;       Jan 2024 -- fixed bug in where statement - last density element always made into a NAN

      NUMCUBE,r,theta,phi,ModPramsStruct,ModSolStruct,nreinit=nreinit 

;
; deal with overbroad transition region in PSI MAS Model
; points less than 500000K should be ignored
;  IN THE NEAR FIELD!!! not the far field
; we will accomplish this by setting density small there
;

      testtrans=where(ModSolStruct.Temp lt 5d5 and r lt 5.)
;      ModSolStruct.Dens[testtrans]=1d-4
      if min(testtrans) ne -1 then ModSolStruct.Dens[testtrans]=sqrt(-1.)
;      if tag_exist(ModSolStruct,'Pop1Dens') then ModSolStruct.Pop1Dens[testtrans]=1d-4
;      if tag_exist(ModSolStruct,'Pop2Dens') then ModSolStruct.Pop2Dens[testtrans]=1d-4
      if tag_exist(ModSolStruct,'Pop1Dens') then ModSolStruct.Pop1Dens[testtrans]=sqrt(-1.)
      if tag_exist(ModSolStruct,'Pop2Dens') then ModSolStruct.Pop2Dens[testtrans]=sqrt(-1.)

END
