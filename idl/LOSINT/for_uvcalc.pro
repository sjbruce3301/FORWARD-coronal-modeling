;************************************************************************
     pro for_uvcalc,ObsPramsStruct,ModSolStruct,ModPramsStruct,LosPramsStruct,pop2tregime,rmod,thmod,phmod,r3D,theta3D,phi3D,cmer2,testnearuse,StokesUV_I,StokesUV_Q,StokesUV_U

;+
; Name: FOR_UVCalc
;
; Purpose:  determine UV spectropolarimetric information for points along LOS
;
; INPUTS  ObsPramsStruct,ModSolStruct,ModPramsStruct,LosPramsStruct,pop2tregime
;	r3D,theta3D,phi3D,testnearuse
;	rmod,thmod,phmod,cmer2  (note these are in same dimensions
;               as ModSolStruct.Dens, either 1D, or like r3D
;
; OUTPUTS StokesUV_I,StokesUV_Q,StokesUV_U
;        note these are in same dimension as r3D, so [nlos,nactualbite]
;
; Called by: FOR_INTENSINT
;
; Calls:  FOR_FIELDCALLS, FOR_UVMODEL
;
;
; HISTORY: 
;	Written by Sarah Gibson 2015
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;-

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

          for_uvmodel,ObsPramsStruct,ModSolStruct,pop2tregime,double(r3Duse),double(theta3Duse),double(phi3Duse),double(Brobs),double(Bthobs),double(Bphobs),VelR,VelTh,VelPh,StokesUV_I,StokesUV_Q,StokesUV_U

          if min(testnearuse) ne -1 then begin
                   StokesUV_Ireal=r3D*0.
                   StokesUV_Qreal=r3D*0.
                   StokesUV_Ureal=r3D*0.
                   StokesUV_Ireal[testnearuse]=StokesUV_I
                   StokesUV_Qreal[testnearuse]=StokesUV_Q
                   StokesUV_Ureal[testnearuse]=StokesUV_U
                   StokesUV_I=StokesUV_Ireal
                   StokesUV_Q=StokesUV_Qreal
                   StokesUV_U=StokesUV_Ureal
          endif

end
