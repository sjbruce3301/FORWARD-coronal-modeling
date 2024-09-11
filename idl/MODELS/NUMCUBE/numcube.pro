PRO NUMCUBE,r,theta,phi,ModPramsStruct,ModSolStruct,nreinit=nreinit,nowidgmess=nowidgmess

;+
; Name:
;      NUMCUBE
;
; Purpose:
;      Main wrapper program to call the data cube and get back out the
;      parameters of the cube: pressure, density, temp, br, bth, bphi
;
; Calling sequence:
;      NUMCUBE,r,theta,phi,ModPramsStruct, ModSolStruct 
;
; Input:
;
;      r : scalar x1 (r) position of the desired interpolation point
;      in units of solar radii
;
;      theta : scalar x2 (theta) position of the desired interpolation
;      point in radians
;
;      phi : scalar x3 (phi) position of the desired interpolation
;      point in radians
;
;      ModPramsStruct : Structure created further up the chain by
;      numcubeprams.pro that includes all the necessary modifications to
;      the cube like rotating the central meridian and latitude, and
;      what to do outside the cube.
;
; Output:
;      ModSolStruct : interpolated value of the model parameters at
;      the given r,theta,phi
;
; Called by FOR_INTENSINT and FOR_POSNOINT (via call_procedure), PSIMAS, AWSOM
; Calls FOR_INTERP_CUBE
;
; Author and history:
;      Written by Laurel Rachmeler, Terry Kucera, Sarah Gibson
;	LR: original version
;       modified for hydrostatic model consistency SEG March 2011
;               Vectorized, TAK 8-Feb-2013
;               Added velocity capability SEG Oct 2013 - Jan 2014
;               Fixed rotation problem (was rotating outside too) Jan 2014 SEG
; Version 2.0 July 2014
;		Added POP2 capablity Jan 2015 SEG
;	Aug-2020 -- SEG -- added IONDENS tag compatibility
;	Sep 2021 - passed through nowidgmess
;	Sep 2023 -- passed through hooks for AWSOM
;-

COMPILE_OPT IDL2 ;default long and square brackets for array subscripts


;**warn if giving default

if ModPramsStruct.Label eq 'Radial Field' and nreinit eq 1 then $
     if keyword_set(nowidgmess) then message,/info,'If you have a specific CUBENAME datacube, you need to explicitly set it as a model parameter, otherwise default is $FORWARD/MODELS/NUMCUBE/cube1.dat, but it is probably not what you want! If you are running PSIMAS or AWSOM from date try changing the date' else d=dialog(/WARNING,'If you have a specific CUBENAME datacube, you need to explicitly set it as a model parameter, otherwise default is $FORWARD/MODELS/NUMCUBE/cube1.dat, but it is probably not what you want! If you are running PSIMAS from date try changing the date')

;***UNWRAP THE POSITION DATA***
;The position input might be a point, a line, or a
;cube. I want them to be a long line, so unwrap if it isn't

;If input arrays have mare than 1D, reform them into 1D arrays

	CoordSize=SIZE(r)	
	NDim=CoordSize[0]
	nunwrapall=PRODUCT(CoordSize[1:NDim]) ;map input onto 1-D 
                                              ;arrays of dimension 
                                              ;nunwrap
        nunwrapall=LONG(nunwrapall)
	p1=REFORM(r,nunwrapall)
	p2=REFORM(theta,nunwrapall)
	p3=REFORM(phi,nunwrapall)
 
   COMMON THECUBE, cube

; We used to have a limit to avoid overloading IDL memory
; but this should be taken care of at the top level via the memory keyword

a_pres=p1*0d0
a_dens=p1*0d0
a_temp=p1*0d0
a_b1=p1*0d0
a_b2=p1*0d0
a_b3=p1*0d0

if tag_exist(cube,'Vr') then begin
  a_v1=p1*0.d0
  a_v2=p1*0.d0
  a_v3=p1*0.d0
endif else a_vel=p1*0.d0

if tag_exist(cube,'Pop2Dens') then a_pop2dens=p1*0d0
if tag_exist(cube,'Pop2Temp') then a_pop2temp=p1*0d0
if tag_exist(cube,'FillingFactor') then a_fillingfactor=p1*0d0
if tag_exist(cube,'Pop2FillingFactor') then a_pop2fillingfactor=p1*0d0

dopop2=0
if (tag_exist(cube,'Pop2Dens') or tag_exist(cube,'Pop2Temp') or tag_exist(cube,'FillingFactor') or tag_exist(cube,'Pop2FillingFactor')) and (ModPramsStruct.DThres eq 0 and ModPramsStruct.ColdPts eq 0) then dopop2=1

;***DO THE INTERPOLATION FROM THE DATA CUBE***
;the actual cube is passed in with ModPramsStruct.cube
        
ioncheck=0
if tag_exist(cube,'IonDens') then ioncheck = 1

if ioncheck eq 1 then begin
 iondens=1
 FOR_INTERP_CUBE, ModPramsStruct,cube,$
                p1,p2,p3,$
                pres,dens,temp,br,bth,bph,vr,vth,vph,$
                pop2dens,pop2temp,fillfact,pop2fillfact,bphotmag,expfac,open,iondens=iondens
endif else begin
 FOR_INTERP_CUBE, ModPramsStruct,cube,$
                p1,p2,p3,$
                pres,dens,temp,br,bth,bph,vr,vth,vph,$
                pop2dens,pop2temp,fillfact,pop2fillfact,bphotmag,expfac,open
endelse

a_pres=pres
a_dens=dens
a_temp=temp
if ioncheck eq 1 then a_iondens=iondens
if dopop2 eq 1 then begin
     if tag_exist(cube,'Pop2Dens') then a_pop2dens=pop2dens
     if tag_exist(cube,'Pop2Temp') then a_pop2temp=pop2temp
     if tag_exist(cube,'FillingFactor') then a_fillingfactor=fillfact
     if tag_exist(cube,'Pop2FillingFactor') then a_pop2fillingfactor=pop2fillfact
endif
topyes=0
if tag_exist(cube,'topology') then begin
 if ModPramsStruct.topology eq 1 then begin
    topyes=1
    a_bphotmag=bphotmag
    a_expfac=expfac
    a_open=open
 endif
endif
a_b1=br
a_b2=bth
a_b3=bph
if tag_exist(cube,'Vr') then begin
      a_v1=vr
      a_v2=vth
      a_v3=vph
endif else a_vel=vr

;***RE-WRAP THE POSITION AND VECTOR DATA***

a_pres=REFORM(a_pres,CoordSize[1:NDim],/overwrite)
a_dens=REFORM(a_dens,CoordSize[1:NDim],/overwrite)
a_temp=REFORM(a_temp,CoordSize[1:NDim],/overwrite)
if ioncheck eq 1 then a_iondens=REFORM(a_iondens,CoordSize[1:NDim],/overwrite)
if dopop2 eq 1 then begin
     if tag_exist(cube,'Pop2Dens') then a_pop2dens=REFORM(a_pop2dens,CoordSize[1:NDim],/overwrite)
     if tag_exist(cube,'Pop2Temp') then a_pop2temp=REFORM(a_pop2temp,CoordSize[1:NDim],/overwrite)
     if tag_exist(cube,'Pop2FillingFactor') then a_pop2fillingfactor=REFORM(a_pop2fillingfactor,CoordSize[1:NDim],/overwrite)
     if tag_exist(cube,'FillingFactor') then a_fillingfactor=REFORM(a_fillingfactor,CoordSize[1:NDim],/overwrite)
endif
if topyes eq 1 then begin
 a_bphotmag=reform(a_bphotmag,coordsize[1:ndim],/overwrite)
 a_expfac=reform(a_expfac,coordsize[1:ndim],/overwrite)
 a_open=reform(a_open,coordsize[1:ndim],/overwrite)
endif
a_b1=REFORM(a_b1,CoordSize[1:NDim],/overwrite)
a_b2=REFORM(a_b2,CoordSize[1:NDim],/overwrite)
a_b3=REFORM(a_b3,CoordSize[1:NDim],/overwrite)
if tag_exist(cube,'Vr') then begin
   	 a_v1=REFORM(a_v1,CoordSize[1:NDim],/overwrite)
  	 a_v2=REFORM(a_v2,CoordSize[1:NDim],/overwrite)
  	 a_v3=REFORM(a_v3,CoordSize[1:NDim],/overwrite)
endif else a_vel=REFORM(a_vel,CoordSize[1:NDim],/overwrite)

        ;don't need to reform positions because I copied the 
        ;original positions from r, theta, phi to p1,p2,p3
        ;r=REFORM(r,CoordSize[1:NDim],/overwrite)
   	;theta=REFORM(theta,CoordSize[1:NDim],/overwrite)
   	;phi=REFORM(phi,CoordSize[1:NDim],/overwrite)

;zero out B-field if NOFIELD is applied
; OR if not 0 or 1, can act as a multiplier on the field
if tag_exist(ModPramsStruct,'nofield') then begin
	if ModPramsStruct.nofield eq 1 then begin
		a_b1 = 0.d0 *a_b1
		a_b2 = 0.d0 *a_b2
		a_b3 = 0.d0 *a_b3
	endif
	if ModPramsStruct.nofield ne 0 and ModPramsStruct.nofield ne 1  then begin
		a_b1 = ModPramsStruct.nofield*a_b1
		a_b2 = ModPramsStruct.nofield*a_b2
		a_b3 = ModPramsStruct.nofield*a_b3
        endif
endif

if tag_exist(cube,'Vr') then $
          ModSolStruct={Pres:a_pres,Dens:a_dens,Temp:a_temp,$
                      Br:a_b1,Bth:a_b2,Bph:a_b3,Vr:a_v1,Vth:a_v2,Vph:a_v3} $
else $
          ModSolStruct={Pres:a_pres,Dens:a_dens,Temp:a_temp,Vel:a_vel,$
                      Br:a_b1,Bth:a_b2,Bph:a_b3} 
;
; deal with various POP2 possibilities
;

if dopop2 eq 1 then begin
     if tag_exist(cube,'Pop2Dens') then ModSolStruct=add_tag(ModSolStruct,a_pop2dens,'Pop2Dens')
     if tag_exist(cube,'Pop2Temp') then ModSolStruct=add_tag(ModSolStruct,a_pop2temp,'Pop2Temp')
;
; allow setting or overwriting of filling factor if P2FILL set to other than one
;  -- assume FILL=1-P2FILL everywhere, P2FILL=P2FILL everywhere
;
     if ModPramsStruct.P2Fill ne 1 then begin
      FillFact=1.-ModPramsStruct.P2Fill
      Pop2FillFact=ModPramsStruct.P2Fill
     endif else begin
      if tag_exist(cube,'Pop2FillingFactor') then Pop2FillFact=a_pop2fillingfactor else Pop2FillFact=1
      if tag_exist(cube,'FillingFactor') then FillFact=a_fillingfactor else FillFact=1
     endelse
     ModSolStruct=add_tag(ModSolStruct,FillFact,'FillingFactor')
     ModSolStruct=add_tag(ModSolStruct,Pop2FillFact,'Pop2FillingFactor')
     MaxFillFact=max(FillFact+Pop2FillFact)
             ;Maybe this should result in a full stop, or a reduction in FillFact, but for now it is just a warning.
     if MaxFillFact gt 1 then message,/info,'The maximum combined filling factor, '+trim(MaxFillFact)+', >1!'
endif

; utilization of COLDPTS or DTHRES
; dopop2 should be 0 in this case, and any pop2 information
; in the cube ignored

if ModPramsStruct.DThres ne 0 or ModPramsStruct.ColdPts ne 0 then begin

  if ModPramsStruct.DThres ne 0 then begin
    a_pop1dens=a_dens
    a_pop2dens=a_dens
    thresD=ModPramsStruct.DThres
    if thresD eq 1 then thresD=5d8
    testpop1=where(a_dens lt thresD)
    testpop2=where(a_dens ge thresD)
  endif 

  if ModPramsStruct.ColdPts ne 0 then begin
    a_pop1dens=a_dens
    a_pop2dens=a_dens
    thresT=ModPramsStruct.ColdPts
    if thresT eq 1 then thresT=3d4
    testpop1=where(a_temp gt thresT)
    testpop2=where(a_temp le thresT)
  endif 

  if ModPramsStruct.P2Fill eq 1 then begin
     if min(testpop2) ne -1 then a_pop1dens[testpop2]=0.
     if min(testpop1) ne -1 then a_pop2dens[testpop1]=0.
     FillFact=1.
     Pop2FillFact=1.
  endif else begin
     FillFact=a_dens*0.
     Pop2FillFact=a_dens*0.
     if min(testpop1) ne -1 then FillFact[testpop1]=1.
     if min(testpop2) ne -1 then FillFact[testpop2]=1.-ModPramsStruct.P2Fill
     if min(testpop1) ne -1 then Pop2FillFact[testpop1]=0.
     if min(testpop2) ne -1 then Pop2FillFact[testpop2]=ModPramsStruct.P2Fill
  endelse

  ModSolStruct=add_tag(ModSolStruct,a_pop1dens,'Pop1Dens')
  ModSolStruct=add_tag(ModSolStruct,a_pop2dens,'Pop2Dens')
  ModSolStruct=add_tag(ModSolStruct,FillFact,'FillingFactor')
  ModSolStruct=add_tag(ModSolStruct,Pop2FillFact,'Pop2FillingFactor')

endif

if tag_exist(cube,'RegimeForce') then ModSolStruct=add_tag(ModSolStruct,cube.RegimeForce,'RegimeForce')

if topyes eq 1 then begin
  ModSolStruct=add_tag(ModSolStruct,a_bphotmag,'BPhotMag')
  ModSolStruct=add_tag(ModSolStruct,a_expfac,'ExpFac')
  ModSolStruct=add_tag(ModSolStruct,a_open,'Open')
endif

if ioncheck eq 1 then ModSolStruct=add_tag(ModSolStruct,a_iondens,'IonDens')

END
