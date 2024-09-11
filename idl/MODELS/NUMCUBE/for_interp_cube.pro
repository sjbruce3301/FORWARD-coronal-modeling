PRO FOR_INTERP_CUBE, ModPramsStruct,cube,x1,x2,x3,pres,dens,temp,br,bth,bph,vr,vth,vph,pop2dens,pop2temp,fillfact,pop2fillfact,bmf,expf,op,iondens=iondens

;+
; Name:
;      CUBE
; Purpose:
;      Take the data cube and get a value of all of the model
;      paramters at an arbitrary position in space. Does this
;      with a 3D linear interpolation of a value on a grid. The 
;      grid does not need to be regular, 
;
; Calling sequence:
;      INTERP_CUBE,ModPramsStruct,cube,x1,x2,x3,pres,dens,temp,br,bth,bph
;		(iondens=iondens if set)
;
; Input:
;      Cube : the data cube structure within which you want to
;      interpolate. Must be in the correct format
;
;      x1 : scalar x1 (r) position of the desired interpolation point
;      in units of solar radii VECTOR IN THIS VERSION
;
;      x2 : scalar x2 (theta) position of the desired interpolation
;      point in radians  VECTOR IN THIS VERSION
;
;      x3 : scalar x3 (phi) position of the desired interpolation
;      point in radians  VECTOR IN THIS VERSION
;
;
;      ModPramsStruct : Structure created in NUMCUBEPRAMS
;       that includes all the necessary information such as
;       what to do outide the cube.
;      Also includes these tags:
;       VelImpose : if nonzero, fills or overwrites velocity to be parallel to B
;		with magnitude VelImpose
;       CubeRot : if nonzero, rotates cube
;
; Output:
;      pres,temp,dens,br,bth,bph,vr,vth,vph: interpolated value of the model parameters at
;      r,th.phi. All are scalars
;      also pop2dens,pop2temp,fillfact,pop2fillfact if passed through
;	if velocity is passed as scalar magnitude (assuming direction along field)
;	then vr will contain this ('vel') and vth and vph will be empty
;	 (direction projection along field will happen in FOR_FIELDCALLS)
;
; Called by NUMCUBE
; Calls FOR_CARTTOSPHERE, FOR_CUBEROT, FOR_HYDROCALC, FOR_SPHERETOCART
;	INTERPOL, INTERPOLATE
;
; Author and history:
;      Written by Yuhong Fan, Laurel Rachmeler, Terry Kucera, Sarah Gibson
;		LR: modified to add comments, and adapt to the
;                        data cube instead of an array
;               YF : created oridinal interpolation program, interp3d.pro
;       modified for hydrostatic model consistency SEG March 2011
;
;       Sarah Gibson: added one degree pixel padding for global models
;                       March 2012
;       Vectorized. TAK 7-Feb-2013
;       Fixed bug dividing by Rsun and in const SEG Sept 2013
;       Added velocity capability SEG 24-0ct-24
;       Moved cube rotation into this routine, since otherwise the outside was also rotated Jan 2014 SEG
; Version 2.0 July 2014
;       Added Pop2 capability; fixed possible bug where "if cube" instead of "if cube ne 0" Jan 2015 SEG
;		Other Pop2 related changes, Jan 29, 2016 TAK
;		Added check for Vel scalar (new implementation make_my_cube allows that) before interpolation
;			Feb 2015 SEG
;
;	Added topology tag capability Apr 2017 SEG
;	Removed pixel padding because it no longer applied July 2017 SEG
;       Sep 2018 -- adjusted hydro strategy so densprof and odensprof are usually scalars
;          that are multiplied by standard profiles. However left it possibility of vector line command input.
;	   also made it no longer forced that hydro=1 for the odens/cdens profiles.
;	Oct 2018 -- added Vasquez hydrostatic
;	Sep 2019 -- added Cranmer empirical hydro=4
;	Aug 2020 -- SEG -- added iondens checks
;	Feb 2022 -- SEG -- updated treatment of plasma using for_hydrodefaults
;		simpliefied parameters
;		fixed bug where B0 was written Bo, and replaced with Bnorm
;		  to avoid conflict with solar B angle 
;	Nov 2022 -- SEG -- fixed bug where In/out of bounds didn't
;		account properly for top radial point (out=ge, in = lt)
;	    Also added negative hydro to allow r**2 dropoff in/out
;		Also fixed bug where inbounds was failing becuase
;			phi close to pi/2
;	    Also put in check for cube.global because default NUMCUBE doesn't have it
;	Apr 2024 -- SEG -- added detached FL = -1, and changed outofbounds op=-10
;-

;extract parameter variables from params structure

t=tag_names(ModPramsStruct)
for i=0,n_elements(t)-1 do void=execute(t[i]+'=ModPramsStruct.(i)')

x1v=cube.r ;grid positions in r (R_sun)
x2v=cube.th ;grid positions in theta (radians)
x3v=cube.ph ;grid positions in phi (radians)
n1=N_ELEMENTS(x1v)
n2=N_ELEMENTS(x2v)
n3=n_ELEMENTS(x3v)

Br=x1*0.
Bth=x1*0.
Bph=x1*0.
Vr=x1*0.
Vth=x1*0.
Vph=x1*0.
pres=x1*0.
dens=x1*0.
temp=x1*0.

ioncheck=0
if keyword_set(iondens) then ioncheck=1
if ioncheck eq 1 then iondens=x1*0.

pop2dens=x1*0.
pop2temp=x1*0.
fillfact=x1*0.
pop2fillfact=x1*0.


;at this point
;x1=r
;x2=theta
;x3=phi
;in original passed in coordinates

;
; cube rotation
;

p1=x1
p2=x2
p3=x3
IF ModPramsStruct.CubeRot ne 0 THEN BEGIN
;***TRANSFORM TO CARTESIAN, ORIGINAL COORDINATES, POSITION ONLY***
            FOR_SPHERETOCART, p1, p2, p3
            ;p1=x
            ;p2=y
            ;p3=z
            ;in original passed in coordinates

;***DO THE CUBE ROTATION***
            FOR_CUBEROT, ModPramsStruct.CubeRot, p1, p2, p3
            ;p1=x
            ;p2=y
            ;p3=z
            ;in radially-rotated coordinates

;***TRANSFORM BACK TO SPHERICAL, ROTATED COORDINATES***
            FOR_CARTTOSPHERE, p1, p2, p3
            ;p1=r
            ;p2=theta
            ;p3=phi
            ;in radially-rotated coordinates

ENDIF

;p1=r
;p2=theta
;p3=phi
;in radial-rotated coordinates

;Change angle ranges

tmp=where(p3 GT (!dPI),c) & if c gt 0 then p3[tmp]=p3[tmp]-2.d0*!DPI
tmp=where(p3 LT (-!dPI),c) & if c gt 0 then p3[tmp]=p3[tmp]+2.d0*!DPI

r_sun=6.9570d10 ; cm solar radii

topyes=0
if tag_exist(cube,'topology') then begin
 if ModPramsStruct.topology eq 1 then begin 
    topyes=1
    op=DBLARR(N_ELEMENTS(x1))-10
    bmf=DBLARR(N_ELEMENTS(x1))
    expf=DBLARR(N_ELEMENTS(x1))
;
; outside the box (OutofBounds) will have default op=-10
;  and bmf=expf=0.
;
; detached field lines will have open=-1 and result in
; expf=negative
; and be filled in with plasma like closed
;
    bphotmag=sqrt(cube.brphot1^2+cube.bthphot1^2+cube.bphphot1^2)
    bmag=sqrt(cube.br^2+cube.bth^2+cube.bph^2)
    expfac=bphotmag*double(cube.open)/bmag
    for ic = 0,n1-1 do expfac[*,*,ic]=expfac[*,*,ic]/x1v[ic]/x1v[ic]
;
; closed plasma
;
    if (hydro ne 0 and hydro ne 4) then tophydro=hydro else tophydro=3
    for_hydrocalc,p1,cdens_he,cpres_he,ctemp_he,hydro=tophydro,isothermal=cT0,densprof=cdensprof

;
; open plasma
;
    for_hydrocalc,p1,odens_he,opres_he,otemp_he,hydro=tophydro,isothermal=oT0,densprof=odensprof

 endif
endif

IF n_elements(cube.axisym) EQ 0 THEN axisym=0 ELSE axisym=cube.axisym ;in case you forgot to set this parameter

;first check if it is in range. 
; need to compare rotated coordinates to cube gridpoints;
; but, if out of range these will be evaluated in non-rotated coordinates
; That last line of the if statement
;means that the phi range only counts if the cube isn't 2d axisymmetric
;or global

if tag_exist(cube,'global') then global=cube.global else global=0

OutofBounds=where( ((p1 LT x1v[0]) OR (p1 GT x1v[n1-1]) $
		OR (p1 GE 1000.) $
		OR ((p2 LT x2v[0]) OR (p2 GT x2v[n2-1]) $
		OR ((p3 LT x3v[0]) OR (p3 GT x3v[n3-1]) AND (axisym EQ 0)) $
                    AND (global NE 1) )),Cout)

if Cout gt 0 THEN BEGIN
	;deal with magnetic field
   CASE Bout of
   		1: begin
				;small external field
			  br[OutofBounds]=0  	;default so not really needed
			  bth[OutofBounds]=0	;default so not really needed
			  bph[OutofBounds]=0	;default so not really needed
		  end
		2: begin		
			  ;split radial field, gauss
			  br[OutofBounds]=Bnorm/(x1[OutofBounds]*x1[OutofBounds])
			  ;IF (x2[OutofBounds] GT !DPI/2.d0) THEN br=-1.*br ;negative on the bottom half     
			  tmp2=where(x2[OutofBounds] GT !DPI/2.d0,c2) 
			  if c2 gt 0 then br[OutofBounds[tmp2]]=-1.*br[OutofBounds[tmp2]]
			  bth[OutofBounds]=0	;default so not really needed
			  bph[OutofBounds]=0	;default so not really needed
          end
   		ELSE: begin
      ;dipole field, gauss
		  const=Bnorm/(2*x1[OutofBounds]*x1[OutofBounds]*x1[OutofBounds])
		  br[OutofBounds]=const*2.0*COS(x2[OutofBounds])
		  bth[OutofBounds]=const*SIN(x2[OutofBounds])
		  bph[OutofBounds]=0
		  end
	ENDCASE
   ;deal with plasma
   IF (hydro LE 0) THEN BEGIN
    IF (hydro EQ 0) THEN BEGIN
      pres[OutofBounds]=1d-5
      dens[OutofBounds]=1d-5
      temp[OutofBounds]=1d-5     
      if ioncheck eq 1 then iondens[OutofBounds]=1d-5
    ENDIF ELSE BEGIN
;
; NEW! negative hydro means use the upper/lower boundary value
; for a given (potentially rotated) theta,phi and assume r**2 falloff. 
;  this will actually be applied later (since interpolation needed to
;	determine theta,phi) 
;
; for now zero out ions outside of box -- may return to this later
      if ioncheck eq 1 then iondens[OutofBounds]=1d-5
    ENDELSE

   ENDIF ELSE BEGIN
      ;hydrostatic, calculate with HYDROCALC
      for_hydrocalc,x1[OutofBounds],dens_he,pres_he,temp_he,hydro=hydro,isothermal=te,densprof=densprof
      pres[OutofBounds]=pres_he
      temp[OutofBounds]=temp_he
      dens[OutofBounds]=dens_he
;
; we don't want to put ions outside box even if hydro model being assumed 
; for rest of plasma
;
      if ioncheck eq 1 then iondens[OutofBounds]=1d-5
   ENDELSE
; out of bounds should not be affected by filling factor
   fillfact[OutofBounds]=1.
; pop2 should always be in bounds
   pop2dens[OutofBounds]=1d-5
   pop2temp[OutofBounds]=1d-5
   pop2fillfact[OutofBounds]=1d-5

;	out of bounds, velocity will be 0
   vr[OutofBounds]=0.d0  
   vth[OutofBounds]=0.d0
   vph[OutofBounds]=0.d0

ENDIF ;end out of bounds

InBounds=where((((p1 GE x1v[0]) AND (p1 LE x1v[n1-1])) $
		AND (p1 LT 1000.) $
		AND (((p2 GE x2v[0]) AND (p2 LE x2v[n2-1])) $
		AND (((p3 GE x3v[0]) AND (p3 LE x3v[n3-1])) OR (axisym EQ 1)) $
                    OR (global EQ 1) )),Cin)

If CIn gt 0 or hydro lt 0 THEN BEGIN
   ;find r position in the grid
   j1=interpol(indgen(n_elements(x1v)),x1v,p1)

   ;find theta position in the grid
    j2=interpol(indgen(n_elements(x2v)),x2v,p2)

   ;find phi position in the grid. If the grid is axisymmetric, then
   ;this is just zero.
   
  IF axisym THEN BEGIN
 	  j3=0
  ENDIF ELSE BEGIN
      j3=interpol(indgen(n_elements(x3v)),x3v,p3)
  ENDELSE

  mult=p1*0.+1.
  JAbove=0
  JBelow=0
; if doing r**2 falloff for out of box points
  IF hydro lt 0 and Cout gt 0 THEN BEGIN
; check to see if below or above
    BelowBounds = where (p1 lt x1v[0],JBelow)
    AboveBounds = where (p1 gt x1v[n1-1],JAbove)
    if JBelow gt 0 then begin
      j1[BelowBounds]=interpol(indgen(n_elements(x1v)),x1v,x1v[0])
      mult[BelowBounds]=(x1v[0]*x1v[0])/p1[BelowBounds]/p1[BelowBounds]
    endif
    if JAbove gt 0 then begin
      j1[AboveBounds]=interpol(indgen(n_elements(x1v)),x1v,x1v[n1-1])
      mult[AboveBounds]=(x1v[n1-1]*x1v[n1-1])/p1[AboveBounds]/p1[AboveBounds]
    endif
  ENDIF

   ;find the interpolated value. This is a linear weighted
   ;interpolation. Take the value at one of the grid points, and weight
   ;it by the product of the distances b/w the target location and the 
   ;opposite grid walls. Sum the 8 possible combinations (one for each
   ;neighboring grid point 

  IF axisym THEN BEGIN

;
; if topology tag is set, use open/closed info to determine density, pres, temp
; also expansion factor
;
; this should never be used for iondens - will result in zero values if done
; at some point may want to adjust for_makeplasma to allow for closed/open ion values
;
   if topyes eq 1 then begin
    op[InBounds]= interpolate(double(cube.open),j1[inbounds],j2[inbounds])
    bmf[InBounds]= interpolate(bphotmag,j1[inbounds],j2[inbounds])
    expf[InBounds]= interpolate(expfac,j1[inbounds],j2[inbounds])
;
; overwrite plasma for closed
;
    closedcheck=where(op eq 0 or op eq -1)
    if min(closedcheck) ne -1 then begin
     pres[closedcheck]=cpres_he[closedcheck]
     temp[closedcheck]=ctemp_he[closedcheck]
     dens[closedcheck]=cdens_he[closedcheck]
    endif
;
; overwrite plasma for open
;
    opencheck=where(op gt 0)
    if min(opencheck) ne -1 then begin
     pres[opencheck]=opres_he[opencheck]
     temp[opencheck]=otemp_he[opencheck]
     dens[opencheck]=odens_he[opencheck]
    endif

   endif else begin
    pres[InBounds]= interpolate(double(cube.pres),j1[inbounds],j2[inbounds])
    dens[InBounds]= interpolate(double(cube.dens),j1[inbounds],j2[inbounds])
    temp[InBounds]=interpolate(double(cube.temp),j1[inbounds],j2[inbounds])
    if JAbove gt 0 then begin
     pres[AboveBounds]= mult[abovebounds]*interpolate(double(cube.pres),j1[abovebounds],j2[abovebounds])
     dens[AboveBounds]= mult[abovebounds]*interpolate(double(cube.dens),j1[abovebounds],j2[abovebounds])
     temp[AboveBounds]=interpolate(double(cube.temp),j1[abovebounds],j2[abovebounds])
    endif
    if JBelow gt 0 then begin
     pres[BelowBounds]= mult[belowbounds]*interpolate(double(cube.pres),j1[belowbounds],j2[belowbounds])
     dens[BelowBounds]= mult[belowbounds]*interpolate(double(cube.dens),j1[belowbounds],j2[belowbounds])
     temp[BelowBounds]=interpolate(double(cube.temp),j1[belowbounds],j2[belowbounds])
    endif
    if ioncheck eq 1 then iondens[InBounds]= interpolate(double(cube.iondens),j1[inbounds],j2[inbounds])
   endelse


   if tag_exist(cube,'Pop2Dens') && n_elements(cube.Pop2Dens) gt 1 $
         then pop2dens[InBounds]= interpolate(double(cube.pop2dens),j1[inbounds],j2[inbounds])
   if tag_exist(cube,'Pop2Temp') && n_elements(cube.pop2Temp) gt 1 $
         then pop2temp[InBounds]= interpolate(double(cube.pop2temp),j1[inbounds],j2[inbounds])
   if tag_exist(cube,'Pop2FillingFactor') && n_elements(cube.Pop2FillingFactor) gt 1 $
         then pop2fillfact[InBounds]= interpolate(double(cube.pop2fillingfactor),j1[inbounds],j2[inbounds])
   if tag_exist(cube,'FillingFactor') && n_elements(cube.FillingFactor) gt 1 $
         then fillfact[InBounds]= interpolate(double(cube.fillingfactor),j1[inbounds],j2[inbounds])
   br[InBounds]=interpolate(double(cube.br),j1[inbounds],j2[inbounds])
   bth[InBounds]= interpolate(double(cube.bth),j1[inbounds],j2[inbounds])
   bph[InBounds]= interpolate(double(cube.bph),j1[inbounds],j2[inbounds])
   if ModPramsStruct.VelImpose eq 0. then begin
     if tag_exist(cube,'Vr') then begin
      vr[InBounds]=interpolate(double(cube.vr),j1[inbounds],j2[inbounds])
      vth[InBounds]= interpolate(double(cube.vth),j1[inbounds],j2[inbounds])
      vph[InBounds]= interpolate(double(cube.vph),j1[inbounds],j2[inbounds])
     endif else begin
      if tag_exist(cube,'Vel') then begin
        if min(cube.Vel) eq 0. and max(cube.Vel) eq 0. then vr[InBounds]=0.d0 else $
          vr[InBounds]=interpolate(double(cube.vel),j1[inbounds],j2[inbounds]) 
      endif else vr[InBounds]=0.d0
     endelse
   endif else vr[InBounds]=interpolate(double(ModPramsStruct.VelImpose)+cube.dens*0.d0,j1[inbounds],j2[inbounds]) 
 
  ENDIF ELSE BEGIN ;not axysimmetric

   if topyes eq 1 then begin
    op[InBounds]= interpolate(double(cube.open),j1[inbounds],j2[inbounds],j3[inbounds])
    bmf[InBounds]= interpolate(bphotmag,j1[inbounds],j2[inbounds],j3[inbounds])
    expf[InBounds]= interpolate(expfac,j1[inbounds],j2[inbounds],j3[inbounds])
;
; overwrite plasma for closed or detached
;
    closedcheck=where(op eq 0 or op eq -1)
    if min(closedcheck) ne -1 then begin
      pres[closedcheck]=cpres_he[closedcheck]
      temp[closedcheck]=ctemp_he[closedcheck]
      dens[closedcheck]=cdens_he[closedcheck]
    endif
;
; overwrite plasma for open
;
    opencheck=where(op gt 0)
    if min(opencheck) ne -1 then begin
      pres[opencheck]=opres_he[opencheck]
      temp[opencheck]=otemp_he[opencheck]
      dens[opencheck]=odens_he[opencheck]
    endif

   endif else begin

    pres[InBounds]= interpolate(double(cube.pres),j1[inbounds],j2[inbounds],j3[inbounds])
    dens[InBounds]= interpolate(double(cube.dens),j1[inbounds],j2[inbounds],j3[inbounds])
    temp[InBounds]=interpolate(double(cube.temp),j1[inbounds],j2[inbounds],j3[inbounds])
    if JAbove gt 0 then begin
     pres[AboveBounds]= mult[abovebounds]*interpolate(double(cube.pres),j1[abovebounds],j2[abovebounds],j3[abovebounds])
     dens[AboveBounds]= mult[abovebounds]*interpolate(double(cube.dens),j1[abovebounds],j2[abovebounds],j3[abovebounds])
     temp[AboveBounds]=interpolate(double(cube.temp),j1[abovebounds],j2[abovebounds],j3[abovebounds])
    endif
    if JBelow gt 0 then begin
     pres[BelowBounds]= mult[belowbounds]*interpolate(double(cube.pres),j1[belowbounds],j2[belowbounds],j3[belowbounds])
     dens[BelowBounds]= mult[belowbounds]*interpolate(double(cube.dens),j1[belowbounds],j2[belowbounds],j3[belowbounds])
     temp[BelowBounds]=interpolate(double(cube.temp),j1[belowbounds],j2[belowbounds],j3[belowbounds])
    endif
    if ioncheck eq 1 then iondens[InBounds]=interpolate(double(cube.iondens),j1[inbounds],j2[inbounds],j3[inbounds])
   endelse

   if tag_exist(cube,'Pop2Dens') && n_elements(cube.pop2Dens) gt 1 $
         then pop2dens[InBounds]= $
            interpolate(double(cube.pop2dens),j1[inbounds],j2[inbounds],j3[inbounds])
   if tag_exist(cube,'Pop2Temp') && n_elements(cube.pop2Temp) gt 1 $
        then pop2temp[InBounds]= $
           interpolate(double(cube.pop2temp),j1[inbounds],j2[inbounds],j3[inbounds])
   if tag_exist(cube,'Pop2FillingFactor') && n_elements(cube.Pop2FillingFactor) gt 1 $
         then pop2fillfact[InBounds]= $
           interpolate(double(cube.pop2fillingfactor),j1[inbounds],j2[inbounds],j3[inbounds])
   if tag_exist(cube,'FillingFactor') && n_elements(cube.FillingFactor) gt 1 $
         then fillfact[InBounds]= $
           interpolate(double(cube.fillingfactor),j1[inbounds],j2[inbounds],j3[inbounds])
   br[InBounds]=interpolate(double(cube.br),j1[inbounds],j2[inbounds],j3[inbounds])
   bth[InBounds]= interpolate(double(cube.bth),j1[inbounds],j2[inbounds],j3[inbounds])
   bph[InBounds]= interpolate(double(cube.bph),j1[inbounds],j2[inbounds],j3[inbounds])
   if ModPramsStruct.VelImpose eq 0. then begin
     if tag_exist(cube,'Vr') then begin
      vr[InBounds]=interpolate(double(cube.vr),j1[inbounds],j2[inbounds],j3[inbounds])
      vth[InBounds]= interpolate(double(cube.vth),j1[inbounds],j2[inbounds],j3[inbounds])
      vph[InBounds]= interpolate(double(cube.vph),j1[inbounds],j2[inbounds],j3[inbounds])
     endif else begin
      if tag_exist(cube,'Vel') && n_elements(cube.Vel) gt 1 then begin
        if min(cube.Vel) eq 0. and max(cube.Vel) eq 0. then vr[InBounds]=0.d0 else $
          vr[InBounds]=interpolate(double(cube.vel),j1[inbounds],j2[inbounds],j3[inbounds]) 
      endif else vr[InBounds]=0.d0
     endelse
   endif else vr[InBounds]=interpolate(double(ModPramsStruct.VelImpose)+cube.dens*0.d0,j1[inbounds],j2[inbounds],j3[inbounds]) 

  ENDELSE 

;
; ;***TRANSFORM POSITION AND VECTOR TO CARTESIAN, ROTATED COORDINATES***
  IF ModPramsStruct.CubeRot ne 0 THEN BEGIN
            p1use=p1[InBounds]
            p2use=p2[InBounds]
            p3use=p3[InBounds]
;
; need to be careful we dont transform the position vector twice
;
            p1use2=p1[InBounds]
            p2use2=p2[InBounds]
            p3use2=p3[InBounds]
            b1use=br[InBounds]
            b2use=bth[InBounds]
            b3use=bph[InBounds]
            v1use=vr[InBounds]
            v2use=vth[InBounds]
            v3use=vph[InBounds]
            FOR_SPHERETOCART, p1use, p2use, p3use,b1use,b2use,b3use
            if tag_exist(cube,'Vr') then begin
               FOR_SPHERETOCART,p1use2,p2use2,p3use2,v1use,v2use,v3use
	    endif
; coordinates are:
;p1=x
;p2=y
;p3=z
;b1=bx
;b2=by
;b3=bz
;v1=vx
;v2=vy
;v3=vz
;in radially-rotated coordinates
;***DO THE DE-ROTATION ABOUT THE RADIAL-AXIS***

            FOR_CUBEROT, -1*ModPramsStruct.CubeRot,p1use,p2use,p3use,b1use,b2use,b3use 
            if tag_exist(cube,'Vr') then  $
             FOR_CUBEROT, -1*ModPramsStruct.CubeRot,p1use2,p2use2,p3use2,v1use,v2use,v3use 
; coordinates are:
;p1=x
;p2=y
;p3=z
;b1=bx
;b2=by
;b3=bz
;v1=vx
;v2=vy
;v3=vz
;in original orientation cartesian coordinates

;***TRANSFORM POSITION AND VECTOR TO SPHERICAL, ORIGINAL COORDINATES***

            FOR_CARTTOSPHERE,p1use,p2use,p3use,b1use,b2use,b3use
            if tag_exist(cube,'Vr') then  $
             FOR_CARTTOSPHERE,p1use2,p2use2,p3use2,v1use,v2use,v3use
; coordinates are:
;p1=r
;p2=theta
;p3=phi
;b1=br
;b2=btheta
;b3=bphi
;v1=vr
;v2=vtheta
;v3=vphi
;in original passed in spherical coordinates
;
; now put into arrays
;
            br[InBounds]=b1use
            bth[InBounds]=b2use
            bph[InBounds]=b3use
            vr[InBounds]=v1use
            vth[InBounds]=v2use
            vph[InBounds]=v3use
  ENDIF ; end if cuberot

ENDIF ;end in bounds

END

