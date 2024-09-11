;************************************************************************
;+
function for_get_grid,gridtype,ngrid,ngy,rheight,$
                      limb,cmer,phio,bang,ruser,thuser,phuser,coorduser,$
                      xxmin,xxmax,yymin,yymax,losoffset,azequi,distobs,losuse
;
;Name: FOR_GET_GRID
;
;Purpose: To create structure containing the grid of points
;          at which observables will be forward-modeled along a line of sight
;
;  Inputs:
;
;       GRIDTYPE - PlaneOfSky or CarrMap or UserInput: default PlaneOfSky
;		testing new grid: AzEqui -- that will show even steps in elongation
;		will piggy back on PlaneofSky for now
;		by setting a keyword "azequi" 
;
;       RUSER,THUSER,PHUSER -- if UserInput then need these in SPHERICAL COORDS!
;               two choices: heliographic (coorduser='helio')
;                            heliocentric (coorduser='observer')
;               see full description in FOR_GRIDDDEFAULTS.PRO
;		They also need to have the same dimension(s) 
;       NGRID-   PlaneOfSky default 256 (makes NGrid,NGrid grid
;                 unless Limb is set as below, then NGrid/2,Ngrid)
;                 CarrMap default 180 (makes 2*NGrid,NGrid+1./dang
;                 (phi,theta) grid -- dang degree resolution)
;               NGY- if set, overrides NGRID so that DX does not have to
;                               equal DY
;       RHEIGHT - only needed for CarrMap, default 1.2 Rsun
;       LIMB -  tells which limb to plot
;               if CarrMap, default West
;               if set for PlaneOfSky, only plots that half Sun at
;               that Limb
;       CMER - central meridian longitude of observer
;                       (e.g. Carrington coordinates)
;               IN DEGREES -- default -90 for PLANEOFSKY and CARRMAP
;                             default 0 for USER
;
;       PHIO - shift to impose on model structure -- note this
;               allows us to make a Carrington map in fixed
;               Carrington coordinates and move the structure around, or
;               fit its location to data
;
;       XXMIN,XXMAX,YYMIN,YYMAX - min and max values of the grid
;         NOTE evaluate at center of pixel
;         so begin/end +/- resolution element/2 from min max point
;
;
;       LOSOFFSET - offset from center point of LOS integration
;                       (X=0 plane if LOSUSE=XLOS
;                         Thomson Sphere if LOSUSE=TAU or LOSUSE=TLOS)
;	  can be scalar or (for gridtype='USER') same dimensions as RUSER, THUSER, PHUSER
;         NOTE: FOR_GRIDDEFAULTS WILL ENSURE THAT LOSOFFSET='NULL' if POS UNSET
;		(except for Physical Diagnostics)

;
; Output outarray - structure containing:
;
;	GRIDTYPE, RHEIGHT, LIMB, PHIO (as inputted but degrees to radians)
;	CMER -- converted to radians
; 	
;	coordinates in (Nx,Ny) grid:
;       	RPOS - r in plane of the sky
;       	THPOS - theta (polar angle, 0 - 2 pi, counterclockwise from north) in plane of the sky
;       	PHPOS - central meridian longitude
;
;       DX, DY - pixel width of grid (based on range and Ngrid)
;	XRANGE,YRANGE -- arrays containg XXMIN,XXMAX; YYMIN,YYMAX
;       XCENTER, YCENTER - location of the center of the grid (to be
;                          used by for_pos_map in making map.)
;	LOSOFFSET - as inputted or default 0.
;	    unless USER set in which case will be adjusted to indicate position along LOS to
;	    USER point(s) from X=0 (XLOS) or TS (TLOS or TAU)
;		dimensions (Nx, Ny) grid
;	AZEQUI, DISTOBS (as inputted)
;
;  Called by FOR_GETSTRUCTS
;
;Common Blocks: None
;
; written by Jim Dove, Sarah Gibson, Terry Kucera 2010-2014
;
; HISTORY - Modified how the range of the grid is determined. One
;           option is that xxmin, xxmax, yymin, yymax are passed
;           directly. Other option is that maxhweight and limb are
;           passed. Output structure now contains dx, dy, xcen, and
;           ycen to be used by for_pos_map.
;         - 11 Feb 2010 JBD
;       Added UserInput options May 4 SEG
;               Changed cmer=cmer0 to cmer=cmer0[0] to prevent accidental 
;                       conversion of ph to 1 element array. 9-Nov-2010 TAK
;               Added warning if thuser gt 180. Dec-2012 SEG
;       Moved defaults to for_defaults Jan 2013 SEG
;       Gave option of 0/1 for COORD; got rid of maxheight; moved
;               limb defaults to for_griddefaults  Mar 2013 SEG
;       Removed COORD - absorbed in OCCULT, now in LOS
;
; Version 2.0 July 2014
;
;	Added choices of COORDUSER and made revisions to USER
;	  (choice COORDUSER='observer' should be backward compatible)
;		SEG March 2016
;  	Added compatibility for xoffset as number vs array SEG Jan 2018
;
;	Fix(round so that nx doesnt end up long
;	        SEG Sep 2021 
;
; 	Testing AZEQUI -- sep 2021
;	Oct 2021 -- put in placeholder xoffuse for xoffset so it doesn't change dimension
;	Dec 2021 -- passing through distobs
;		also moved distobs and azequi to end of GridPramsStruct so order doesn't 
;			 trigger rerun of old files (although probably that is more of an 
;			 issue for gridops, as dealt with in for_griddefaults.pro)
;	Mar 2022 -- changed xoffset to losoffset
;			passed losuse through from for_getstructs
;			edited USER losoffset determination for clarity 
;			and removed some remaining is_number(cmer) checks
;	Jun 2022 -- added check for distobs ne 'NULL' before making it double
; 	Jul 2022 -- fixed bug rprof-->rproj, loffuse-->losoffuse
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;			added some clarifying comments to USER
;			ensured losoffset same dimension as rpos,thpos,phpos
;				and changed 'NULL' values (for LOS integral or Physical Diagnostic
;				to value 0 in GridPramsStruct
;		    also put in stop if ruser, thuser, phuser not in the same dimensions
;	Feb 2023 -- changed print statement for change in xxmin etc
;		to 1d-7 not 1d-2
;		also changed print statements to identify any changes in ngrid,ngy
;- 

mdtor=!dpi/180d0

; Plane of Sky grid

  if strupcase(gridtype) eq 'PLANEOFSKY' then begin

     xxr=xxmax-xxmin
     yyr=yymax-yymin

     if ngy eq ngrid then begin
       rrr=max([xxr,yyr])
;       dx=rrr/double(ngrid-1)
       dx=rrr/double(ngrid)
       dy=dx
     endif
     if ngy ne ngrid then begin
;       dx=xxr/double(ngrid-1)
;       dy=yyr/double(ngy-1)
       dx=xxr/double(ngrid)
       dy=yyr/double(ngy)
     endif

;     nx=round(xxr/dx) + 1
;     ny=round(yyr/dy) + 1
     nx=fix(round(xxr/dx)) 
     ny=fix(round(yyr/dy)) 

     if nx ne ngrid then begin
	print,'NX changed',nx,ngrid
     endif
     if ny ne ngy then begin
	 print,'NY changed',ny,ngy
     endif

     xxmaxin=xxmax
     yymaxin=yymax

;     xxmax = xxmin + double(nx-1)*dx
;     yymax = yymin + double(ny-1)*dy
     xxmax = xxmin + double(nx)*dx
     yymax = yymin + double(ny)*dy

;
; make sure we are not trying to evaluate right at origin
;
     if abs(xxmin) lt 1d-8 then xxmin=1d-8
     if abs(yymin) lt 1d-8 then yymin=1d-8
     if abs(xxmax) lt 1d-8 then xxmax=1d-8
     if abs(yymax) lt 1d-8 then yymax=1d-8

     if abs(xxmaxin-xxmax) gt 1d-7 then begin
	print,'XMAX changed',xxmaxin,xxmax
     endif
     if abs(yymaxin-yymax) gt 1d-7 then begin
	print,'YMAX changed',yymaxin,yymax
     endif

;     x1d=xxmin + dindgen(nx)*dx
;     y1d=yymin + dindgen(ny)*dy
     x1d=xxmin + dindgen(nx)*dx + dx/2.d0
     y1d=yymin + dindgen(ny)*dy + dy/2.d0

     xcenter=(xxmax+xxmin)/2.d0
     ycenter=(yymax+yymin)/2.d0

     x=x1d#replicate(1.d0,ny)
     y=replicate(1.d0,nx)#y1d
        
     r = sqrt(x^2+y^2)
     th = acos(y/r)
     west = where(x gt 0.)
     if min(west) ne -1 then th[west]=2.d0*!dpi-th[west]
     ph=x*0.d0 + (cmer-phio)*mdtor

     rheight=0.d0

;
; special case AZEQUI set
;
     if azequi eq 1 then begin
;
; step in even elongation
;
        elong=r*!dtor
        r=distobs*tan(elong)
;
; the purpose of this is just to pick plane of sky intersections for the lines of sight
; that are equally spaced in elongation rather than r_pos. The points in 3D space will be 
; calculated the same as usual, dealing with non-parallel lines of sight in for_intlos
;
; alpha = th
;  --- remember though this is a polar angle, 0 to 360 counterclockwise
;
; e_y = -elong*sin(alpha)
; e_z = elong*cos(alpha)
;
; to be sorted out in for_plot.pro
;
     endif 
;
; put losoffset into same dimensions as r, th, ph
;  and get rid of 'NULL' for LOS integral/Physical Diagnostic (replace with 0.)
;
     if is_number(losoffset[0]) then begin
	losoffuse=double(losoffset) + r*0.d0
     endif else losoffuse = r*0.d0

  endif 

  if strupcase(gridtype) eq 'USERINPUT' or strupcase(gridtype) eq 'USER' then begin

;
; ruser, thuser, phuser (in DEGREES)  
;  are spherical coords
;  r should be radius of point
;  th should be colatitude of point
;  ph should be longitude of point 
; colatitude/longitude are in two frames defined by COORDUSER
; and described in detail in FOR_GRIDDEFAULTS
;
; First we will get arrays in the right dimension and change to radians
;
   test_r=size(ruser)
   test_th=size(thuser)
   test_ph=size(phuser)
   if test_r[0] ne test_th[0] or test_r[0] ne test_ph[0] then begin
     print,'ruser, thuser, phuser must have same dimensions'
     print,'stopping in for_get_grid -- reset and try again.'
     stop
   endif
   if test_r[0] eq  0 then begin
       r3D=dblarr(1)
       th3D=dblarr(1)
       ph3D=dblarr(1)
       r3D[0]=ruser
       th3D[0]=thuser*mdtor
       ph3D[0]=phuser*mdtor
       if thuser gt 180 then print,'thuser should be 0-180 degrees (colatitude)'
   endif else begin
       r3D=ruser
       th3D=thuser*mdtor
       ph3D=phuser*mdtor
       test = where(thuser gt 180)
       if min(test) ne -1 then print,'thuser should be 0-180 degrees (colatitude)'
   endelse

; Now, if COORDUSER='helio',
; we have to convert to 'observer' spherical frame
; because later r3D,theta3D,phi3D will be calculated assuming that
;
;  This requires knowledge of BANG, which should be already set, with default = zero.
;  (if BANG=0, 'observer' is the same as 'helio')
;
;  But we need first to center our frame on CMER, however, since the BANG tilt
;  will be with respect to that.
;
; CMER will also default to zero.

   if strupcase(coorduser) eq 'HELIO' and bang ne 0 then begin
    thtilt=th3D
    phtilt=ph3D-cmer*mdtor
    for_throt,r3D,thtilt,phtilt,bang*mdtor
    th3D=thtilt
    ph3D=phtilt+cmer*mdtor
   endif

;
; Now we can proceed in converting to (observer frame) plane-of-sky coords
;  r=POS radius (<1 on disk),th=POS polar angle,ph=POS cmer
;	(POS = X=0 plane)
;  also, losoffset=displacement of point from central point of LOS integration (X=0 plane or Thomson Sphere)
;	stepping along LOS
;
; Note: if one wants to determine integrand observable quantity at points 
; along the LOS, this can be done either by fixing CMER and defining USER points
; along the LOS, or by defining ruser,thuser,phuser at intersection of LOS and plane of sky/TS,
; then using LOSOFFSET to move along the LOS - in both cases setting /POS
;  (otherwise, will integrate as if at projected point in POS)
; These two approaches should be equivalent
;

   phidif=ph3D-cmer*mdtor
   test=where(phidif lt 0.)
   if min(test) ne -1 then phidif=phidif+2.*!dpi
   test=where(phidif gt 2.*!dpi)
   if min(test) ne -1 then phidif=phidif-2.*!dpi

;
; find POS radius at intersection of LOS and POS
;
; start by projecting point (r3d,theta3d,phidif) onto POS
;
   xx=r3D*sin(th3D)*cos(phidif)
   yy=r3D*sin(th3D)*sin(phidif)
   zz=r3D*cos(th3D)

   rproj = sqrt(yy^2+zz^2)

;
; correct for radial position of observer
;
   r = (rproj*distobs)/(distobs-xx)

;
; but force lines of sight parallel if LOSUSE=XLOS
;
   if strupcase(losuse) eq 'XLOS' then r=rproj

;
; theta in POS is the same at projected and intersection point
;
   th = acos(zz/rproj)

;
; phi is by convention defined as central meridian
; theta is polar angle, increasing counterclockwise from north
;
   west = where(yy gt 0.)
   if min(west) ne -1 then th[west]=2.d0*!dpi-th[west]
   ph=ph3D*0.d0 + cmer*mdtor

;
;  need to correct the out of plane dimension 
;  for any user-inputted LOSOFFSET
;
; first put user defined losoffset into same dimensions as r, th, ph
;  and get rid of 'NULL' for LOS integral/Physical Diagnostic (replace with 0.)
;
   if is_number(losoffset[0]) then begin
       test_los=size(losoffset)
       if test_los[0] ne 0 then begin
         if test_r ne test_los then begin
          print,'losoffset must be scalar or same dimensions as ruser, thuser, phuser'
          print,'stopping in for_get_grid -- reset and try again.'
          stop
         endif
       endif
       losoffuse=double(losoffset) + r*0.d0
   endif else losoffuse = r*0.d0

; now add in the out of plane shift for the USER point(s) location(s)
;  -- easy for XLOS
   if strupcase(losuse) eq 'XLOS' then begin
    losoffuse=losoffuse+xx
;  radial distance to offset point
;  (will just be r3D if loffset not set)
    roffset=sqrt(rproj*rproj + losoffuse*losoffuse)
;
; now for TLOS/TAU
   endif else begin
    elong=atan(r/distobs)
    alpha=asin(xx/r3D)
    tau=alpha-elong
    toffset=r3D*sin(tau)
    losoffuse=losoffuse+toffset
; closest intersection with TS
    rp2=sqrt(r3D*r3D - toffset*toffset)
; radial distance to offset point
    roffset=sqrt(rp2*rp2 + losoffuse*losoffuse)
   endelse
;
; check to be sure not inside the sun
;
   test=where(roffset lt 1.d0)
   if min(test) ne -1 then begin
     print,'points inside sun will be ignored (maybe reconsider losoffset)' 
   endif

;
; we can also rotate the model structure relative
; to phi(model)=0  position 
;

     ph=ph -phio*mdtor
; 
     dx=0.
     dy=0.
     xcenter=0
     ycenter=0
     xxmin=0.
     xxmax=0.
     yymin=0.
     yymax=0.
     rheight=0.

;     print,ruser,thuser,phuser
;     print,r,th/mdtor,ph/mdtor
;     print,losoffuse
;     print,cmer,bang

  endif

  if strupcase(gridtype) eq 'CARRMAP' then begin

;   Carrington Map grid

     dang=180.d0/double(ngrid) 
     nx=2.d0*ngrid                 ; phi dimension -- 360 degrees
     rheight=double(rheight)

;
; theta dimension -- 181 degrees (north to south pole inclusive of zero lat)
;  	unless dang > 1 degree, then 180 degrees (last point left off)
;

     ny=ngrid + fix(1.d0/dang)     

     r=dblarr(nx,ny) + rheight
     th=dblarr(nx,ny) 
     ph=dblarr(nx,ny) 
;
;  ph is the central meridian, but note the maps
;  will need to be shift +/- 90 in make_carr_map
;  to yield standard Carrington map output 
;  that is, the data are extracted from the limbs,
;  but the map plot has central meridian for the X axis
;
     
     for i = 0,nx-1 do ph[i,*]= double(i)*dang  + (cmer-phio) 
     if dang lt 1. then dstop=fix(1.d0/dang)
     if dang ge 1. then dstop=1.d0
     if strupcase(limb) eq 'WEST' then begin
        for j = 0,ny-dstop do th[*,j]= 180.d0+j*dang
     endif
     if strupcase(limb) eq 'EAST' then begin
        for j = 0,ny-dstop do th[*,j]= 180.d0-j*dang
     endif
     if strupcase(limb) eq 'CMER' then begin
        for j = 0,ny-dstop do begin
	  th[*,j]= 0.
	  r[*,j]=rheight*(cos((180.d0-j*dang)*mdtor))
; 
; needs to be negative to maintain the information about which hemisphere
;
	endfor
     endif
;
; make sure phi is within bounds
;
     big=where(ph gt 360.d0)
     if min(big) ne -1 then ph[big]=ph[big]-360.d0
     small=where(ph lt 0.d0)
     if min(small) ne -1 then ph[small]=360.d0+ph[small]
;
; degrees to radii
;
     th=th*mdtor
     ph=ph*mdtor

;     dx = (xxmax-xxmin)/double(nx-1)
;     dy = (yymax-yymin)/double(ny-1)
     dx = 360.d0/double(nx-1) ; pixel size in degrees, longitude
     dy = 180.d0/double(ny-1) ; pixel size in degrees, latitude

;     xcenter=(xxmax+xxmin)/2.d0
;     ycenter=(yymax+yymin)/2.d0
      xcenter=180.d0
      ycenter=90.d0

      xxmin=0.d0
      xxmax=360.d0
      yymin=0.d0
      yymax=180.d0

;
; put losoffset into same dimensions as r, th, ph
;  and get rid of 'NULL' for LOS integral/Physical Diagnostic (replace with 0.)
;
      if is_number(losoffset[0]) then begin
	losoffuse=double(losoffset) + r*0.d0
      endif else losoffuse = r*0.d0

  endif

  if is_number(rheight) then rheight=double(rheight)
  cmeruse=double(cmer*mdtor)
  if is_number(phio) then phiouse=double(phio*mdtor) else phiouse=phio
  if is_number(distobs) then distobs=double(distobs)

  outarray={GridType:gridtype,RHeight:rheight,Limb:limb,Rpos:double(r),THpos:double(th),PHpos:double(ph),Cmer:cmeruse,Phio:phiouse,$
           Dx:double(dx),Dy:double(dy),XRange:[double(xxmin), double(xxmax)],YRange:[double(yymin), double(yymax)],XCenter:double(xcenter),YCenter:double(ycenter),LOSoffset:losoffuse,$
	   AzEqui:double(azequi),DistObs:distobs}

   return,outarray

end

;**********************************************************************
