pro for_makeplasma,r,th,ph,dens,pres,temp,hydro,isothermal=isothermal,densprof=densprof

;
; sets up background hydrostatic cube
;
; Called by MAKE_MY_CUBE
; Calls FOR_HYDROCALC
;
; Written by Laurel Rachmeler, Sarah Gibson 2012
; Version 2.0 July 2014
;   Oct 2018 -- updated Hydrostatic
;   Sep 2019 -- added hydro=4
;   Feb 2022 -- updated to call for_hydrodefaults
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;		fixed bug where vcdensprof and vodensprof were not in call to for_hydrodefaults


if hydro ne 4 then begin
  for_hydrodefaults,$
        hydro=hydro,$
        cdensprof=densprof,ct0=isothermal,$
        vcdensprof=vcdensprof
  densprof=vcdensprof
endif else begin
 for_hydrodefaults,$
        hydro=hydro,$
        odensprof=densprof,ot0=isothermal,$
        vodensprof=vodensprof
  densprof=vodensprof
endelse

;
;Make 3D r cube for hydro

r3d=fltarr(n_elements(r),n_elements(th),n_elements(ph))
for i=0,n_elements(th)-1 do begin
     for j=0,n_elements(ph)-1 do begin
      r3d[*,i,j]=r[*]
     endfor  
endfor   
     
for_hydrocalc,r3d,dens,pres,temp,hydro=hydro,isothermal=isothermal,densprof=densprof

end
