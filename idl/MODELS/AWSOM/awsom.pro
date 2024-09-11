PRO AWSOM,r,theta,phi,ModPramsStruct,ModSolStruct,nreinit=nreinit

;+
; Name:
;      AWSOM
;
; Calls NUMCUBE for AWSOM datacube
;
; Written Sarah Gibson and Judit Szente
;	Sep 2023
;       Jan 2024 -- fixed bug in where statement - last density element always made into a NAN

      NUMCUBE,r,theta,phi,ModPramsStruct,ModSolStruct,nreinit=nreinit 

;
; deal with overbroad transition region in AWSOM Model
; points less than 230000K should be ignored
;  IN THE NEAR FIELD!!! not the far field
; we will accomplish this by setting density small there
; or better still - to NaN which will be ignored in integral
; but allow easy identification when plotting shells
;

;      testtrans=where(ModSolStruct.Temp lt 230000. and r lt 5.)
      testtrans=where(ModSolStruct.Temp lt 500000. and r lt 5.)
;      ModSolStruct.Dens[testtrans]=1d-4
      if min(testtrans) ne -1 then ModSolStruct.Dens[testtrans]=sqrt(-1.)
;      if tag_exist(ModSolStruct,'Pop1Dens') then ModSolStruct.Pop1Dens[testtrans]=1d-4
;      if tag_exist(ModSolStruct,'Pop2Dens') then ModSolStruct.Pop2Dens[testtrans]=1d-4
      if tag_exist(ModSolStruct,'Pop1Dens') then ModSolStruct.Pop1Dens[testtrans]=sqrt(-1.)
      if tag_exist(ModSolStruct,'Pop2Dens') then ModSolStruct.Pop2Dens[testtrans]=sqrt(-1.)


END
