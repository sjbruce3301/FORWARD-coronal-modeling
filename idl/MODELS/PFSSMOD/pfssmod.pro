PRO pfssmod,r,theta_for,phi_for,ModPramsStruct,ModSolStruct

;+
; Name:
;      PFSSMOD
;
; Purpose:
;      Main wrapper program to call the PFSS model and get back out the
;      parameters: pressure, density, temp, br, bth, bphi
;
; Calling sequence:
;      PFSSMOD,r,theta_for,phi_for,ModPramsStruct,ModSolStruct
;
; Input:
;
;      r: scalar x1 (r) position of the desired interpolation point
;      in units of solar radii
;
;      theta_for : scalar x2 (theta) position of the desired interpolation
;	(named theta_for to avoid conflict with PFSS_DATA_BLOCK)
;      point in radians
;
;      phi_for : scalar x3 (phi) position of the desired interpolation
;	(named phi_for to avoid conflict with PFSS_DATA_BLOCK)
;      point in radians
;
;      ModPramsStruct : Structure created further up the chain by
;      pfssmodprams.pro 
;
; Output:
;      ModSolStruct : interpolated value of the model parameters at
;      the given r,th,phi
;
; Called by FOR_INTENSINT and FOR_POSNOINT (via call_procedure)
; Calls: FOR_HYDROCALC
;
; Common blocks:
;	PFSS_DATA_BLOCK -- part of DeRosa PFSS software
;	   stores datacube information
;
; ; Author and history:
;      Written by  Laurel Rachmeler, Sarah Gibson
;	LR: original version
;       modified for hydrostatic model consistency SEG March 2011
;       modified for forward-backward compatibility LR June 2013
;
; Version 2.0 July 2014
;
;	Feb 2016 - commented L0,B0,
;		added negative rtop to signify
;		source surface at infinity but rtop size of box
;	Nov 2016 -- removed common block PFSSSTUFF -- unneeded becasue PFSS package
;		codes avoid unnecessary re-restoring of files
;		Also used simpler link to PFSS_DATA_BLOCK
;		Also changed FLTARR to DBLARR
;		Also fixed bug where field above SS was not scaled by Rss*Rss
;	Feb 2017 - topology stuff, open field, mag strength at photo ftpt, expansion factor at point
;	Sep 2018 -- adjusted hydro strategy so densprof and odensprof are usually scalars
;	   that are multiplied by standard profiles. However left it possibility of vector line command input.
; 	Oct 2018 -- added Vasquez hydrostatic
;	Sep 2019 -- added Cranmer empirical
;	Jan 2024 -- updated open topology info to include sign
;-

COMPILE_OPT IDL2 ;default long and square brackets for array subscripts

@pfss_data_block

;extract parameter variables from params structure

 t=tag_names(ModPramsStruct)
 for i=0,n_elements(t)-1 do void=execute(t[i]+'=ModPramsStruct.(i)')

 CoordSize=SIZE(r)	
 NDim=CoordSize[0]
 nunwrapall=PRODUCT(CoordSize[1:NDim]) ;map input onto 1-D 
                                ;arrays of dimension 
                                ;nunwrap
;unwrap the positions into a 1d array
 nunwrapall=LONG(nunwrapall)
 p1=REFORM(r,nunwrapall)
 p2=REFORM(theta_for,nunwrapall)
 p3=REFORM(phi_for,nunwrapall)

 irc=DBLARR(N_ELEMENTS(p1))
 ithc=DBLARR(N_ELEMENTS(p1))
 iphc=DBLARR(N_ELEMENTS(p1))
 b1=DBLARR(N_ELEMENTS(p1))
 b2=DBLARR(N_ELEMENTS(p1))
 b3=DBLARR(N_ELEMENTS(p1))

;find where the r is above/below the top of box (source surface unless negative)
 low_index=WHERE(p1 LE abs(rtop),countl);below
 IF countl LE 0 THEN print,'Warning: none of your points are below the source surface'
 high_index=WHERE(p1 GT abs(rtop),counth);above

 almost_rtop=abs(rtop)*0.999999

 IF countl GT 0 THEN BEGIN
;find spherical coords for below source surface
    irc[low_index]=get_interpolation_index(rix,p1[low_index])
    ithc[low_index]=get_interpolation_index(lat,90-p2[low_index]*!radeg)
    iphc[low_index]=get_interpolation_index(lon,(p3[low_index]*!radeg+360) mod 360)
 ENDIF

 IF counth GT 0 THEN BEGIN
;find spherical coords for above source surface, basically calculate
;Br at rtop and use that with bth=bph=0
    high_rs=dblarr(counth)+almost_rtop
    irc[high_index]=get_interpolation_index(rix,high_rs)
    ithc[high_index]=get_interpolation_index(lat,90-p2[high_index]*!radeg)
    iphc[high_index]=get_interpolation_index(lon,(p3[high_index]*!radeg+360) mod 360)
 ENDIF

 b1=interpolate(br,iphc,ithc,irc)
 b2=interpolate(bth,iphc,ithc,irc)
 b3=interpolate(bph,iphc,ithc,irc)

 IF counth GT 0 THEN BEGIN
;force outer radial field if it isn't already
;impose r^2 dropoff (taking into account that Br is evaluated at SS
; so need to scale from there).
    b1[high_index]=rtop*rtop*b1[high_index]/(p1[high_index]*p1[high_index])
;
; test for non-radiality -- this will happen for example if source surface
; is at infinity (rtop lt 0) but points are outside the box (top=abs(rtop))
;
    btest=b2[high_index]*b2[high_index] + b3[high_index]*b3[high_index]
    test=where(btest gt 1d-4)
    if min(test) ne -1 then print,'field above box assuming to be radial, but nonradial fields as high as ',sqrt(max(btest[test])),' Gauss'
    b2[high_index]=0
    b3[high_index]=0
 ENDIF

; now figure out plasma
;  note densprof, odensprof are set up as a vector in for_hydrodefaults
;   for all HYDRO>1
;  and is a tag on ModPramsStruct

 hydrosave=hydro
 hydrouse=hydro
 if hydro eq 4 then hydrouse=3
 for_hydrocalc,p1,dens_he,pres_he,temp_he,hydro=hydrouse,isothermal=t0,densprof=densprof
 pres=pres_he
 temp=temp_he
 dens=dens_he
 hydro=hydrosave

; topology stuff - open field marker(op), mag strength at photo ftpt (bmf), 
;  expansion factor at point (expf)
; note -- need to load the cube because it is not in the COMMON block
;

 if tag_exist(ModPramsStruct,'topology') then begin
  if topology eq 1 then begin
   restore,filename
   op=DBLARR(N_ELEMENTS(p1))
   bmf=DBLARR(N_ELEMENTS(p1))
   expf=DBLARR(N_ELEMENTS(p1))
   op=interpolate(open,iphc,ithc,irc)
;
;   NOTE USING FIRST FOOTPOINT INFO ONLY -- appropriate for open
;
   if exist(brphot) eq 0 then brphot=brphot1
   if exist(bthphot) eq 0 then bthphot=bthphot1
   if exist(bphphot) eq 0 then bphphot=bphphot1
   bphotmag=sqrt(brphot^2+bthphot^2+bphphot^2)
   bmag=sqrt(br^2+bth^2+bph^2)
; NOTE this may be poorly interpolated
; FOR_POSNOINT will calculated fieldlines on the fly instead
   expfac=bphotmag*open/bmag
   for ic = 0,nr-1 do expfac[*,*,ic]=expfac[*,*,ic]/rix[ic]/rix[ic]
   bmf=interpolate(bphotmag,iphc,ithc,irc)
   expf=interpolate(expfac,iphc,ithc,irc)
   brf=interpolate(br,iphc,ithc,irc)
;
; overwrite plasma for open 
;  and add sign
;
   for_hydrocalc,p1,odens_he,opres_he,otemp_he,hydro=hydro,isothermal=ot0,densprof=odensprof
   opencheck=where(op ne 0)
   signopen=abs(brf)/brf
   if min(opencheck) ne -1 then begin
    pres[opencheck]=opres_he[opencheck]
    temp[opencheck]=otemp_he[opencheck]
    dens[opencheck]=odens_he[opencheck]
;    print,minmax(op[opencheck])
    op[opencheck]=op[opencheck]*signopen[opencheck]
   endif
   closedcheck=where(op eq 0)
   if min(closedcheck) ne -1 then op[closedcheck]=.1*signopen[closedcheck]
  endif
 endif

;reform all of the solutions into whatever the initial r,theta,phi
;form was
 pres=REFORM(pres,CoordSize[1:NDim],/overwrite)
 dens=REFORM(dens,CoordSize[1:NDim],/overwrite)
 temp=REFORM(temp,CoordSize[1:NDim],/overwrite)
 b1=REFORM(b1,CoordSize[1:NDim],/overwrite)
 b2=REFORM(b2,CoordSize[1:NDim],/overwrite)
 b3=REFORM(b3,CoordSize[1:NDim],/overwrite)
 if tag_exist(ModPramsStruct,'topology') then begin
  if topology eq 1 then begin
   op=REFORM(op,CoordSize[1:NDim],/overwrite)
   bmf=REFORM(bmf,CoordSize[1:NDim],/overwrite)
   expf=REFORM(expf,CoordSize[1:NDim],/overwrite)
  endif
 endif
        

;reform r,th,phi. DO need to do these, because they got reset with the
;restore. 
 r=REFORM(p1,CoordSize[1:NDim])
 theta_for=REFORM(p2,CoordSize[1:NDim])
 phi_for=REFORM(p3,CoordSize[1:NDim])

 Vel=Pres*0.d0+velimpose
 
;put solutions into the correct structure

 if tag_exist(ModPramsStruct,'topology') then begin
  if topology eq 1 then begin
   ModSolStruct={Pres:pres,Dens:dens,Temp:temp,Vel:vel,$
              Br:b1,Bth:b2,Bph:b3,BPhotMag:bmf,ExpFac:expf,Open:op}
  endif else $
   ModSolStruct={Pres:pres,Dens:dens,Temp:temp,Vel:vel,$
              Br:b1,Bth:b2,Bph:b3}
 endif else $
  ModSolStruct={Pres:pres,Dens:dens,Temp:temp,Vel:vel,$
              Br:b1,Bth:b2,Bph:b3}

END
