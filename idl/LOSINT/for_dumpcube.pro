;************************************************************************
     pro for_dumpcube,ModSolStruct,ModPramsStruct,LosPramsStruct,cmer2,rmod,thmod,phmod,testnearuse,r3D,theta3D,phi3D,datadump

;+
; Name: FOR_DUMPCUBE
;
; Purpose:   write out cube (will be done with verbose set)
;
; INPUTS ModSolStruct,LosPramsStruct,ModPramsStruct,testnearuse,r3D,theta3D,phi3D,datadump (name of output file)	
; 	cmer2,rmod,thmod,phmod (note these are in same dimensions
;               as ModSolStruct.Dens, either 1D, or like r3D
;
; OUTPUTS  saved file datadump
;
; Called by: FOR_INTENSINT
;
; Calls FOR_FIELDCALLS
;
; HISTORY: 
;	Written by Sarah Gibson 2015
;	October 2020  added presobs to dump 
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

          Densobs=ModSolStruct.Dens
          Tempobs=ModSolStruct.Temp
          Presobs=ModSolStruct.Pres
          r3Duse=double(r3Duse)
          theta3Duse=double(theta3Duse)
          phi3Duse=double(phi3Duse)
          if exist(Brobs) eq 0 then begin
           Brobs=r3Duse*0.d0
           Bthobs=r3Duse*0.d0
           Bphobs=r3Duse*0.d0
          endif else begin
           Brobs=double(Brobs)
           Bthobs=double(Bthobs)
           Bphobs=double(Bphobs)
          endelse
          if exist(VelR) eq 0 then begin
            VelR=Brobs*0.d0
            VelTh=Brobs*0.d0
            VelPh=Brobs*0.d0
          endif else begin
            VelR=double(VelR)
            VelTh=double(VelTh)
            VelPh=double(VelPh)
          endelse
          Densobs=double(Densobs)
          Tempobs=double(Tempobs)

          save,filename=datadump,testnearuse,double(r3D),$
			  double(r3Duse),double(theta3Duse),double(phi3Duse),$
                          double(Brobs),double(Bthobs),double(Bphobs),$
                          double(VelR),double(VelTh),double(VelPh),$
                          double(Densobs),double(Tempobs),double(Presobs)

end
