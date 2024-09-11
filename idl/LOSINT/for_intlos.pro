;************************************************************************
     pro for_intlos,rbite,thbite,phbite,alfabite,losoffbite,LosPramsStruct,ObsPramsStruct,$
GridPramsStruct,r3D,theta3D,phi3D,r2,cmer2,x2,tau2,dlos,doprint,verbose=verbose

;+
; Name: FOR_INTLOS
;
; Purpose:  	DETERMINE spherical coords along LOS in viewer's coordinate system, also determine line of sight vector and differential line of sight dlos

;
; INPUTS
;
;    rbite,thbite,phbite - reduced POS array
;	  (rbite is radial position at the intersection of the LOS and the X=0 plane)
;	  (remember thbite goes 0 to 360 counterclockwise)
;    alfabite - limb
;    LosPramsStruct,ObsPramsStruct,GridPramsStruct
;    doprint = whether to print verbose stuff
;    losoffbite -- GridPramsStruct.LOSoffset -- if an array, put into bite size
;     for_get_grid will store one of four things here:
;      1) if USER and POS set and LOSOFFSET not inputted as keyword, will be the distance of the user points
;	(inputted in spherical geometry r3D,theta3D,phi3D)
;	along the LOS from the intersection of the LOS with either X=0 or TS (XLOS, or TLOS/TAU)
;      2) if USER and POS set and LOSOFFSET inputted by user, will be the sum of that input 
;	and the LOS distance as in 1
;      3) if USER and POS not set, all LOSOFFSET information will be ignored
;		because integration will be centered on X=0 (XLOS) or TS (TLOS, TAU)
;	*note -- I had thought to center integrals on the USER point, but that way madness lies
;	 the  integral should be over infinite LOS, and if not, optimized to includes most emission/brightness
;	  -- which is why we center on X=0 or TS
;	 however, we will need to be careful to zero out LOSOFFSET and to deal with above solar disk
;	 as we would for nonuser Grids
;      4) if USER not set, and POS set, and LOSOFFSET inputted
;	will be the user input value
;      5) if USER not set, and POS not set, and LOSOFFSET inputted
;	 will be ignored (should in fact be forced to 0)
;
; KEYWORD INPUT
;	verbose
;
; OUTPUTS
;
;	r3D,theta3D,phi3D - spherical coords along LOS, in viewer's system of dimension [nlos,nactualbite]
;		(note nlos can be reduced from original input if OBSLOSLIMIT is set to a maximum radius)
;	r2- pos position of LOS intersection but put in [nlos,nactualbite] matrix 
;	cmer2 - central meridian in [nlos,nactualbite] matrix
;	tau2 LOS angle along line of sight, dimension [nlos,nactualbite]
;	x2 - line of sight position vector, dimension [nlos,nactualbite]
;		for both XLOS and TLOS - XLOS forces parallel lines of sight
;	dlos - differential line of sight, dimension [nlos,nactualbite]
;		this is dx -- even if we use TAU it is converted
;		used in for_euvsxrcalc and for_radiocalc although no longer explicitly
;		used in for_integrate (int_tabulated function doesn't need as such - implicit in array)
;
; Called by: FOR_INTENSINT
;
; History: Written by Sarah Gibson 2015
;-
;  	Feb 5 2016: changed definition of Tau so
;		mimics XLOS in that it is always negative
;		behind POS independent of limb -- see
;		comments below
;	Feb 2018: added Elongation capability
;		for lines of sight not close to sun
;		applications e.g. to heliospheric imaging
;       July 2018: added test for POS before elongation problem
;	April 2020: BUG FIX -- inconsistency between tau2 and dlos
;	Nov 2020: BUG FIX -- asymmetric integrations in tau were being thwarted
;		by recalculation associated with obsloslimit
;	Jan 2021: edited VERBOSE stuff
;	Sep 2021: fix(round so that nlos didn't end up long
;	Oct 2021: added fix for USER LOS=tau integration 
;		LOSoffset needs to be taken into account in determiing POS intersection
;		of line of sight
;	Dec 2021 - Jan 2022: passed through distobs, defined tau2 for xlos case
;		added comments
;	March 2022 -- fixed bug where dlos didn't have LOS dimension
;		added TLOS capability
;		also introduced RBITEUSE so that RBITE not changed
;		also changed xoffset --> losoffset
;               removed unnecessary rloslimit change of nlos for tau -- after all
;                those points removed later for all choices LOSUSE
;		 and dealt with in for_intensint by reducing to *rmoduse* etc
;	April 2022 -- added keyword nostretch
;	July 2022 - fixed problem with USER losoffset
;	Sept 2022 -- added back revised conditional for POS ne 0 TLOS
;		(search on shouldnt)
;            was printing unnecessary error	
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;		added some clarifying comments about how LOSOFFSET used
;		fixed bug where stretch was multiplying by < 1 everywhere
;		updated integrals to ignore losoff unless POS set
;		and fixed bug where TLOS was not centering on POS shifted by LOSOFFSET
;		and simplified TLOS vs TAU formulation and overal structure of code
;	Nov 2022 -- moved all points below r3d=1  to 1000 to ignore
;	Apr 2023 -- commented out NEVIII blocking disk
;       May 2022 -- gave same capability for other UV specpol lines
;                       (OVI, LYA-- be careful esp. for LYA 
;                       -- just for tests because 
;                       generally chromo not treated in FORWARD)
;
;  useful variables

pid2= !dpi/2.d0
nactualbite=n_elements(rbite)

;
; distance from observer
; usually 1 AU 215 Rsun
; but e.g. Probe, Orbiter will change this
; so in future this can be a parameter
;  note if one goes too close in, the Thomson Sphere shrinks to be partly inside the Sun.

distobs=GridPramsStruct.DistObs

;
; check elongation --
; if rbite greater than about 5, print warning
; better to step constant angle
;

elongbite=atan(rbite/distobs)

if max(elongbite) gt 0.02 and strupcase(LosPramsStruct.LosUse) eq 'XLOS' and ObsPramsStruct.Pos eq 0  then begin
 print,'WARNING: max elongation=',max(elongbite)
 print,'Consider switching LOSUSE to TAU or TLOS for nonparallel lines of sight.'
endif

;
; figure out starting point on LOS (losmin)
;	 and number of points along LOS (nlos)
 
losmin=LosPramsStruct.LosMin
nlos=LosPramsStruct.NLos
losint=LosPramsStruct.LosInt

;
; scale by elongation if LOSUSE=TLOS
;    UNLESS keyword "nostretch" is set
;   note this will give LOSMIN, LOSINT dimension of rbite (POS)
;

if tag_exist(LosPramsStruct,'NoStretch') then begin
  if is_number(LosPramsStruct.NoStretch) then nostretch=LosPramsStruct.NoStretch $
    else nostretch=0
endif else nostretch=0

if strupcase(LosPramsStruct.LosUse) eq 'TLOS' and nostretch eq 0 then begin
 if verbose eq 1 then begin
    print,'losmin orig',minmax(losmin)
    print,'losint orig',minmax(losint)
 endif
 if ObsPramsStruct.Pos eq 0 then begin
  losmaxorig=losmin+losint*(nlos-1)
; this was a bug
;  losmin=losmin*distobs*sin(elongbite)
;  losmaxnew=losmaxorig*distobs*sin(elongbite)
  losmin=losmin/cos(elongbite)
  losmaxnew=losmaxorig/cos(elongbite)
  losint=(losmaxnew-losmin)/(nlos-1)  
 endif
endif

; POS dimension matrix
bitenorm = replicate(1.,(nactualbite))
; LOS dimension matrix
losnorm = replicate(1.,nlos)

losintbite= losint*bitenorm

cmer2=losnorm#phbite
r2 = losnorm#rbite
th2=losnorm#thbite
elong2=losnorm#elongbite
alfa2=losnorm#alfabite
losoff2=losnorm#losoffbite
tau2=r2*0.d0

;  IF INTEGRATING OVER TAU -- LOS ANGLE
;    or TLOS -- constant distance along (not parallel) LOS

if strupcase(LosPramsStruct.LosUse) eq 'TAU' $
  or strupcase(LosPramsStruct.LosUse) eq 'TLOS' then begin

       if ObsPramsStruct.Pos ne 0 then begin
           if strupcase(LosPramsStruct.LosUse) eq 'TAU' then print,'for_intlos,losuse=tau,pos=',ObsPramsStruct.Pos,': shouldnt happen'
           if strupcase(LosPramsStruct.LosUse) eq 'TLOS' and ObsPramsStruct.Pos ne 2 $
		then print,'for_intlos, losuse=tlos, pos=',ObsPramsStruct.Pos,': shouldnt happen'
	endif
;
; calculate spherical coords along lines of sight
;

; commented out below -- the old way assuming zero elongation
;   (new version should reduce to this for elong=0)
;
;        dlos = losint*r2/cos(tau2)/cos(tau2)
;        r3D=(1/cos(tau2))*r2
;
;        theta3D = acos(cos(tau2)*cos(th2))
;
; changed below to abs(sin(th2)))
;   (remember th2 is 0 to 360)
;  so that now tau negative is always behind plane of sky
;  so integration along LOS occurs in same order as for XLOS
;  matters for lyman absorption, or integration asymmetric about POS
;
;  (alfa identifies which limb we are looking at)
;
;        phi2=atan(tan(tau2)*(1/abs(sin(th2))))
;        phi3D = alfa2*(pid2 - phi2)
;

;
; now recalculate more generally
;
; it's helpful to first calculate
; 3D coordinates of point along (no longer necessarily parallel) lines of sight
; the loci of intersections of LOS with its closest appoach to sun
; make up the Thomson sphere
;
; note r2 and th2 are still plane of sky (X=0 plane) intersecting radius and polar angle
; can convert these to spherical coordinates of point of intersection of LOS with Thomson Sphere 
; rp2, thp2, php2
;
; essentially we are making our LOS spacing cluster at the TS. This makes sense for white light
; but not other observables -- but, only WL will be out so far as for this to matter
; and it is not wrong, just perhaps not optimal LOS spacing for other wavelengths, anyway
; (it is optimal for WL, which is the point)

	rp2=r2*cos(elong2)
	rpbite=rbite*cos(elongbite)

; don't actually use these next, but useful for reference
;
;	thp2=acos(cos(elong2)*cos(th2))
;
;  elong between 0 and pi/2, so cos(elong) always positive
;
;  cos(th2),cos(thp2) positive in north, negative in south as they should be
;     thp2 has lost information about east west, but that is ok, it is retained in alfa
;
;	php2 = asin(sin(elong2)/sin(thp2))
;  which is equivalent to
;	php2 = atan(tan(elong2)*(1/abs(sin(th2))))

; now make sure tau is within -pi/2 to pi/2 limits
; and if elong significant make sure in front of the observer
; so maximum positive angle is pi/2-elong
;
; note if we put a value for taumin close to -1.57 we can get into the
; situation of all bite points needing to be changed because elongbite is > 0
;
; in this case we will keep the number of points and change the resolution
; because this is a hard cutoff and we need to apply it for all
; cases (including CoMP, etc. which has problems with matrices where NLOS varies from point to point)
;
        low=-1.57d0
        highbite=1.57d0-elongbite
;
; ALSO we need to check if there is a LOS offset 
; either because of user keyword input LOSOFFSET and POS slice
; or because doing gridtype='USER' and POS slice (both of which will only happen for TLOS and POS=2),
; and we need to adjust our center for POS integral accordingly

        if strupcase(LosPramsStruct.LosUse) eq 'TLOS' then begin
	 if ObsPramsStruct.Pos eq 2 then begin
; this is the angle from the TS, just for reference
;	  gamma2 = atan(losoffbite,rpbite)
; and this is the radial distance to it
;  but this is also for reference because we are going to measure
;   tau from the TS still, since that is where teh right angle is
;	  rpbitep=sqrt(rpbite*rpbite + losoffbite*losoffbite)
          losoffbiteuse=losoffbite
	 endif else begin
	  losoffbiteuse=0.
         endelse
; this next includes losoffbiteuse explicitly and is in radians
         tauminbite=atan(losmin+losoffbiteuse,rpbite)>low
; note this next does not include losoffbiteuse explicitly (although an adjustment
;	has been made to aovid going below -pid2 that takes it into account) 
;	and it is in distance units
         losminbite=tan(tauminbite)*rpbite-losoffbiteuse
; this nextdoes include losoffbiteuse explicitly
         losmaxbite=losminbite+losoffbiteuse+losintbite*(nlos-1)
; as does this
         taumaxbite=atan(losmaxbite,rpbite)
         losmaxbitenew=tan(taumaxbite<highbite)*rpbite
         losintbitenew=(losmaxbitenew-losminbite-losoffbiteuse)/(nlos-1)  
; and this next is where we shift the start point losmin to include losoffbiteuse
;  thus centering the integral on the position losoffbite - IF POS slice
         x2 = (losnorm#(losminbite+losoffbiteuse) + findgen(nlos)#losintbitenew)
	 tau2=atan(x2,rp2)
	 if verbose eq 1 then begin
            print,'losmin update',minmax(losminbite)
            print,'losmax update',minmax(losmaxbitenew)
         endif
        endif else begin
; now doing LOS = TAU (constant angle in radians)
; we ignore losoffbite for TAU because always an integral
;  but add it *0 to get dimensions right
         tauminbite=losmin + losoffbite*0. >low 
         taumaxbite=tauminbite+losintbite*(nlos-1)
	 losintbitenew=(taumaxbite<highbite-tauminbite)/(nlos-1)  
         tau2 = (losnorm#tauminbite + findgen(nlos)#losintbitenew)
	 x2=rp2*tan(tau2)
	 if verbose eq 1 then begin
            print,'taumin update',minmax(tauminbite)
            print,'taumax update',minmax(taumaxbite)
         endif
	endelse
	if verbose eq 1 then begin
            print,'losint update',minmax(losintbitenew)
	endif
        testmax=where(taumaxbite ge highbite,tmx) 
	if tmx gt 0 then begin
         if verbose ne 0 and doprint eq 1 then begin
	  print,"**********"
	  print,"**********"
	  print,"**********"
	  print,"Adjusting spacing on "+strtrim(string(tmx),2)+" points so that no points are behind observer"
	  print,"pid2 minus elong minmax="
	  print,strtrim(string(minmax(highbite)),2)
	  print,"**********"
	  print,"**********"
	  print,"**********"
  	 endif
  	endif

;
; relative to rp2, dlos and r3D are same as parallel LOS case above
;

        r3D=(1/cos(tau2))*rp2

;        dlos = losint*rp2/cos(tau2)/cos(tau2)
; this was a bug -- need to use losintbite to be consistent with tau2
;
;   	note -- for TAU, losintbite is by definition dtau and constant
;		and has units of radians so needs to be converted
;		to units of length
;
        if strupcase(LosPramsStruct.LosUse) eq 'TAU' then $
	   dlos = (losnorm#losintbitenew)*rp2/cos(tau2)/cos(tau2) $
;
; (i.e., los = rp2*tan(tau), so dlos= rp2*sec^2(tau) dtau
;
 	else dlos = losnorm#losintbitenew

;
; theta3D reduces to parallel case for elong2=0
;  and to thp2 for tau2=0
; (note tau+elong=out of sky plane angle xi)
; 

        argument=cos(tau2+elong2)*cos(th2)
        theta3D = acos(argument)

;
;  tau2+elong2 is constrained to be always less than  pi/2
;     so cos(theta3D) is positive in the north, negative in the south
;	and does not know about east/west

	phi2 = asin(sin(tau2+elong2)/sin(theta3D))

;  which is equivalent to
;	phi2 = atan(tan(tau2+elong2)*(1/abs(sin(th2))))
;
; for elong2=0 this reduces to old version

;
; this next takes care of eastern/western hemisphere
;  (remember, phbite info is central merdian and is preserved in cmer2;
;	so here we just need to know +/- from central meridian)
;

        phi3D = alfa2*(pid2 - phi2)

;
; Sanity check:
;
; theta3D unchanged at equator (theta3D=thp2=th2=90.)
;  php2=elong2; phi2=tau2+elong2
;
; th2=0 (POS polar angle 0) thp2=elong2; theta3D = tau2+elong2
;  (should never be exactly 0 because we removed
;  these points in for_intpos.pro)
;  php2=pi/2; phi2=pi/2
;
;    at tau2 = 0 
;	cos(theta3D) = cos(elong2)*cos(th2) = cos(thp2)
;	phi3D=pi/2 - php2
;
;    foreground (relative to Thomson sphere)
;	at tau2=pi/2-elong2; cos(theta3d)=0; theta3d=90 
;	 sin(phi2)=1., phi2=pi/2, phi3D=0
;
;    background 
;	at tau2 = -elong2 
;	  cos(theta3D) = cos(th2)  
;	  phi3D=pi/2
;	   (intersection wit POS)
;	at tau2 = -pi/2 
;	  cos(theta3D) = sin(elong2)*cos(th2)
;	  sin(phi2)= cos(elong2)/sin(theta3D)
;	  (for elong2=0 symmetric with foreground) 

endif

;  IF INTEGRATING OVER X -- LOS DISTANCE
;
; Note by definition -- these are parallel lines of sight
; if far out, there will be a warning suggesting switch to TAU
;

if strupcase(LosPramsStruct.LosUse) eq 'XLOS' then begin
     th2 = losnorm#acos(cos(thbite))
     ph2 = losnorm#alfabite*pid2
     y2 = r2*sin(th2)*sin(ph2)
     z2 = r2*cos(th2)
     if strupcase(GridPramsStruct.GridType) eq 'CARRMAP' and strupcase(GridPramsStruct.Limb) eq 'CMER' then begin
       y2=r2*0.d0
       z2=r2
       r2=abs(r2)
     endif
     xlos = losmin + losint*findgen(nlos)
; losoffset -- only should be used for /POS, see above
     if ObsPramsStruct.Pos ne 0 then begin
      if ObsPramsStruct.Pos eq 2 then print,'for_intlos, losuse=xlos, pos=2: shouldnt happen'
      x2 = xlos#bitenorm + losoff2
     endif else x2 = xlos#bitenorm
     dlos = losint*r2/r2

    if is_number(LosPramsStruct.DoDisk) then if LosPramsStruct.DoDisk ne 0. then if strpos(strupcase(ObsPramsStruct.Instrument),'OMP') lt 0 and $
         strupcase(ObsPramsStruct.Instrument) ne 'CORMAG' and $
;         strupcase(ObsPramsStruct.IClass) ne 'UV SPECTROPOLARIMETERS' and $
;         strpos(strupcase(ObsPramsStruct.Instrument),'OVI') lt 0 and $
;         strpos(strupcase(ObsPramsStruct.Instrument),'NEVIII') lt 0 and $
;         strpos(strupcase(ObsPramsStruct.Instrument),'MGIX') lt 0 and $
;         strupcase(ObsPramsStruct.Instrument) ne 'LYA' and $
         strupcase(ObsPramsStruct.Instrument) ne 'WL' and $
         strupcase(ObsPramsStruct.Instrument) ne 'KCOR' then begin

     	      incres=1
     	      if tag_exist(LosPramsStruct,'IncRes') then if is_number(LosPramsStruct.IncRes) then incres=LosPramsStruct.IncRes
     	      if strupcase(ObsPramsStruct.Instrument) eq 'RADIO' then incres=1

	      if incres ne 0 then begin
;
;  use higher resolution above disk
;  first calculate how fine a step to take if taking nlos steps above (positive x) the x=0 plane of sky
;       to finish at the closest point towards the Earth in the original LOS integral (thus, putting all
;       the points that otherwise lie behind the Sun to use in increasing (doubling if symmetric) the resolution in front of it)
;
;  be careful with POS slices: if losoffset is set it can throw things off here
;
                    if ObsPramsStruct.Pos eq 0 then losintfine=abs(losmin+losint*double(nlos-1))/double(nlos-1) else losintfine=losint/2.d0
                    xlos = losintfine*findgen(nlos)
;		print,'losint=',losint,' losintfine=',losintfine
                    x2disk = xlos#bitenorm
                    shiftdisk=(LosPramsStruct.DoDisk^2-y2^2-z2^2)
                    frontbelowdisk = where(r2 le LosPramsStruct.DoDisk,dfront)
                    if dfront gt 0 then begin
;
; there is a possibility of sqrt(neg) number here if rounding error on number close to zero
;
			if min(shiftdisk[frontbelowdisk]) lt -1d-8 then stop
                        x2[frontbelowdisk]=x2disk[frontbelowdisk]+sqrt(abs(shiftdisk[frontbelowdisk]))
                        dlos[frontbelowdisk] = losintfine
	 	    endif
              endif else begin
;
; may eventaully add incres=2 option to have half the points in the first
; scale height - for now the other option is zero, throw away points
;
                    shiftdisk=(LosPramsStruct.DoDisk^2-y2^2-z2^2)
                    backbelowdisk = where(r2 le LosPramsStruct.DoDisk and x2 lt 0.,c)
                    frontbelowdisk = where(r2 le LosPramsStruct.DoDisk and x2 gt 0.,d)
                    if d gt 0 then x2[frontbelowdisk]=x2[frontbelowdisk]+sqrt(shiftdisk[frontbelowdisk])
;
; set points behind sun to so far away signal should be negligable
;
                    if c gt 0 then x2[backbelowdisk]=-1000.d0
              endelse
    endif

    r3D=sqrt(r2*r2+x2*x2)
    theta3D = acos(z2/r3D)
    phi3D = atan(y2,x2)
;
; should really define tau2 otherwise it will be zero and do weird
; things to XPOLG calculation in for_pinpoint
; but be careful because this implicitly assumes parallel lines of sight
;  AND it is the angular distance from the X=0 plane
;   NOT the Thomson Sphere
;
    tau2=asin(x2/r3D)
endif

; if r is ge rloslimit  set to far away
;

rloslimit=ObsPramsStruct.ObsLosLimit
if rloslimit le 1. then rloslimit=1000.

test = where(r3D ge rloslimit,p)
if p gt 0 and strupcase(ObsPramsStruct.Instrument) ne 'RADIO' then r3D[test]=1000.d0

;
; it is possible for r3D to be less than one
;  if distobs is pushed very close to the Sun
;  of if user points are combined with an losoffset
;  this will be dealt with by setting to NaN which
;  will result in NaNs for all LOS-integrated quantities
; note this can't happen for XLOS except for
;  throwaway points below disk that will be sent to r3D>1000
;  as defined above; these individual points along the LOS will
;  be ignored in for_intlos, but the entire LOS does not need to be 
;  thrown away since the foreground points are good.
;
; be careful though because sometimes r3d=1.0 exactly will trigger
; the NaN -- might be better to just throw all of them to r3D>1000
; -- trying that now

test = where(r3D lt 1.0,p)
;if p gt 0 and strupcase(ObsPramsStruct.Instrument) ne 'RADIO' and $
;   strupcase(GridPramsStruct.GridType) ne 'USER' $
;   then r3D[test]=sqrt(-1)
;if p gt 0 and $
;   strupcase(GridPramsStruct.GridType) eq 'USER' $
;   then r3D[test]=1000.d0
if p gt 0 then $
    r3D[test]=1000.d0


end
