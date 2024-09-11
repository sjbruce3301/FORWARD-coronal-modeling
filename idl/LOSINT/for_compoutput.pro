;************************************************************************
     pro for_compoutput,ObsPramsStruct,ModSolStruct,ModPramsStruct,LosPramsStruct,rmod,thmod,phmod,r3D,theta3D,phi3D,cmer2,nfin,nst,lun,compbite,complbite,wavelengths,velbite,centbite,linebite,lchisqbite

;+c
; Name: FOR_COMPOUTPUT
;
; Purpose:  output comp information -- either as output file to be read by forcomp.f, or various *bite with 
;		los-integrated spectral line information
;
; INPUTS ObsPramsStruct,ModSolStruct,ModPramsStruct,LosPramsStruct,r3D,theta3D,phi3D,nfin,nst
;	rmod,thmod,phmod,cmer2 
;
; OUTPUTS (only if NOFTRAN set, otherwise output is written file) 
;		compbite,complbite,wavelengths,velbite,centbite,linebite,lchisqbite
;
;
; Called by: FOR_INTENSINT
;
; Calls: FOR_FIELDCALLS
;
;
; HISTORY: 
;	Written by Sarah Gibson 2015
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;-

	        for_fieldcalls,rmod,thmod,phmod,cmer2,ModSolStruct,ModPramsStruct,LosPramsStruct,Brobs,Bthobs,Bphobs,VelR,VelTh,VelPh
 
;
; For Doppler velocity, need line-of-sight which is Vx

                Vx= VelR*sin(theta3D)*cos(phi3D)+$
                        Velth*cos(theta3D)*cos(phi3D)-$
                        Velph*sin(phi3D)
;
; now write out data unles NOFTRAN
;
                if ObsPramsStruct.FCompPrams.noftran ne 1 then begin

                 nwrap = nfin-nst+1
                 for j = 0.d0,double(nwrap)-1.d0 do begin

; POS
                  for i = 0.d0,double(LosPramsStruct.NLos)-1.d0 do begin
; LOS
                   writeu,lun,double(r3D[i,j]),double(theta3D[i,j]),double(phi3D[i,j]),$
                          double(ModSolStruct.Dens[i,j]),$
                          double(ModSolStruct.Temp[i,j]),$
                          double(Brobs[i,j]),double(Bthobs[i,j]),double(Bphobs[i,j]),double(Vx[i,j])

                  endfor
                 endfor
                endif else forcomp,ObsPramsStruct.Instrument,ObsPramsStruct.LineName,double(r3D),double(theta3D),double(phi3D),double(ModSolStruct.Dens),double(ModSolStruct.Temp),double(Brobs),double(Bthobs),double(Bphobs),double(Vx),compbite,complbite,wavelengths,velbite,centbite,linebite,lchisqbite


end
