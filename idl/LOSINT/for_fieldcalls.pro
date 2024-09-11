pro for_fieldcalls,rmod,thmod,phmod,phbitenorm,ModSolStruct,ModPramsStruct,LosPramsStruct,Brobs,Bthobs,Bphobs,VelR,VelTh,VelPh

;
; program to put vector fields into correct coordinate system
; from model frame to observer
;
; 
; Called by FOR_DUMPCUBE, FOR_UVCALC, FOR_RADIOCALC, and FOR_COMPOUTPUT
;
; Calls FOR_VECTUNROT
;
; Written by Sarah Gibson 2012-2013
; Version 2.0 July 2014
; Bug fix- velocities not projected for VelImpose case full vector velocity in cube
;	June 2016 SEG
;
;       October 2017 -- SEG -- made adjustments to work with UVCODES
;	June 2019 -- bug fix: Velr/th/ph(test)=0. -- added if min(test) ne -1
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)



  if tag_exist(ModSolStruct,'Br') eq 1 then begin
	for_vectunrot,ModSolStruct.Br,ModSolStruct.Bth,$
                 ModSolStruct.Bph,$
                 Brobs,Bthobs,Bphobs,rmod,thmod,phmod,$
                 phbitenorm,LosPramsStruct.Bang,LosPramsStruct.Thetao
  endif else begin
		for_vectunrot,rmod*0.d0,rmod*0.d0,rmod*0.d0,$
                 Brobs,Bthobs,Bphobs,rmod,thmod,phmod,$
                 phbitenorm,LosPramsStruct.Bang,LosPramsStruct.Thetao
  endelse

  Bmag=sqrt(Brobs*Brobs+Bthobs*Bthobs+Bphobs*Bphobs)

  Brobs=double(Brobs)
  Bthobs=double(Bthobs)
  Bphobs=double(Bphobs)

;
; now velocities
;

  if tag_exist(ModPramsStruct,'VelImpose') then velimpose=ModPramsStruct.VelImpose else velimpose=0.d0
  if tag_exist(ModSolStruct,'VR') and tag_exist(ModSolStruct,'VTH') and tag_exist(ModSolStruct,'VPH') and velimpose eq 0. then begin
                     for_vectunrot,ModSolStruct.Vr,ModSolStruct.Vth,ModSolStruct.Vph,$
                       VelR,VelTh,VelPh,rmod,thmod,phmod,$
                       phbitenorm,LosPramsStruct.Bang,LosPramsStruct.Thetao

  endif else begin
                     if tag_exist(ModSolStruct,'Vel') or velimpose ne 0. then begin
                       test=where(Bmag eq 0.d0)
                       if min(test) ne -1 then Bmag[test]=1.d0

                       if velimpose ne 0. then begin
                             usevel=ModPramsStruct.Velimpose
;
; if topology set -- only put velocity on open field lines
;
                             if tag_exist(ModSolStruct,'open') then begin
                                usevel=ModSolStruct.Open*velimpose
                             endif
                       endif else usevel=ModSolStruct.Vel

                       if min(usevel) eq 0. and max(usevel) eq 0. then begin
                           Velr=0.d0*rmod
                           Velth=0.d0*rmod
                           Velph=0.d0*rmod
                       endif else for_vectunrot,usevel*ModSolStruct.Br/Bmag,$
                           usevel*ModSolStruct.Bth/Bmag,$
                           usevel*ModSolStruct.Bph/Bmag,$
                           VelR,VelTh,VelPh,rmod,thmod,phmod,$
                           phbitenorm,LosPramsStruct.Bang,LosPramsStruct.Thetao      
                       if min(test) ne -1 then Velr[test]=0.d0 
                       if min(test) ne -1 then Velth[test]=0.d0 
                       if min(test) ne -1 then Velph[test]=0.d0 
                     endif else begin
                       Velr=0.d0*rmod
                       Velth=0.d0*rmod
                       Velph=0.d0*rmod
                     endelse
  endelse

  Velr=double(Velr)
  Velth=double(Velth)
  Velph=double(Velph)

end
