;************************************************************************
      pro for_posnoint,qntall,r,theta,phi,LosPramsStruct,ObsPramsStruct,ModPramsStruct,GridPramsStruct,$
                ModSolStruct,nreinit=nreinit,norunmodel=norunmodel,deltar=deltar,rdriv=rdriv,nowidgmess=nowidgmess,distobs=distobs,working_dir=working_dir

;+
; Name: FOR_POSNOINT
;
; Purpose:  calculates 2D map of plasma property (e.g., N, T, B)
;			defined by model (ModPramsStruct.Name) 
;			in plane of sky of plane offset from that along LOS
;
; INPUTS
;
;	r - r in plane of the sky
;	theta - theta (polar angle, 0 - 2 pi, counterclockwise from north) in plane of the sky (radians)
;	phi - central meridian longitude  (radians)
; 	
;	LosPramsStruct - Structure containing parameters that determine viewing angle and so POS
;               BANG -- bang solar B angle IN RADIANS, default 0 -- nonzero requires call to procedure FOR_THROT via FOR_DOROTS 
;               THETAO -- thetao latitude rotation IN RADIANS, default 90 -- thetao ne 90 requires call to procedure FOR_THROT  via FOR_DOROTS
;
;	ObsPramsStruct - Structure containing description of the observable to be modeled
;		(see OBS_NAME for descriptions of the possible observation types)
;               INSTRUMENT, (unless LABELONLY set)
;               LINENAME, (unless LABELONLY set)
;               LINENUM (number with one to one correspondence to LINENAME), (unless LABELONLY set)
;               LABEL text lable for plots
;
;	ModPramsStruct - Structure containing name and model parameters
;		NAME
;		model parameters as specified in MOD_NAME function
;
;	GridPramsStruct - Structure containing grid paramers
;		LIMB
;		LOSOFFSET -- line-of-sight offset for plane relative to plane-of-sky or Thomson Sphere
;
; Keyword inputs
;
;	NREINIT -- avoid loading numerical datacubes more than once
; 	NORUNMODEL -- use ModSolStruct passed through
;	DELTAR -- used for NINST calculation: add to RMOD
;
; OUTPUT:  Array qntall, having same dimensions as r and theta
; 
;	ModSolStruct - Structure containing model output, e.g. DENS, PRES, VEL, BR, BTH, BPH
;			Data in same dimensions as notdisk
;			OUTPUT - or INPUT if NORUNMODEL set
;
;	RDRIV -- return information about RMOD for derivative NINST calculation
;
; Calls: MODPRAMSSTRUCT.NAME,
;		FOR_DOROTS -- which calls FOR_THROT as needed
;		FOR_VECTUNROT
;
; Called by: FOR_DRIVE, FOR_PLOTFIELDLINES, FOR_FORWARDMAP 
;
; Written by Sarah Gibson, Terry Kucera
;
; HISTORY: 
;        Split off from FOR_INTENSINT Mar 1 2010, SEG
;        Modified to not do calculation under solar disk Mar 8 2010, SEG
;         defined model-frame coordinate system (rmod,thmod,phmod)
;               and explictly redefined vector B to be projected on observer's
;               frame coordinates (r3D,theta3D,phi3D)   Mar 8 2010 SEG
;       Added FF for Filling Factor Oct. 27, 2011 TAK
;       No call to model for cases where ModSolStruct already calculated Jan 18 2012 SEG
;       Changed response if density is negative, 1-Aug-2012, SEG, TAK
;       Fixed bug where line='phi' always returned limb, added central
;       meridian to get phi in Carrington coordinates
;       Got rid of COORD, used OCCULT
;       Adjusted for limb = CMER option for CarrMap Sept 2013, SEG
;
; Version 2.0 July 2014
;
; 	Changed FF to FILLFACT and added POP2FILLFACT
;		also add POP2DENS, TEMP
;	Added options THMOD,PHMOD,THOBS,PHOBS instead of THETA, PHI
;	  these allow seeing angles in observer's frame (for POS
;	  plot, these should always look like standard theta phi)
;	  vs model, or absolute heliographic coordinate system
;	  (these are rotated by Bang,Cmer,Thetao)
;	     March 9 2016 SEG
;
; 	Added DELTAR and RDRIV keywords to enable NINST calculation
;	   also don't do magnetic vector rotation if NINST
;	Added IONDENS June 2016 SEG
;	Added EXPFAC Dec 2016 SEG
;	Fixed bug where EXPFAC was calculated with Br everywhere 
;		when it should be Bmag Jan 2017 SEG
;	Turned off using Bfield*topo.sav stored expansion factor
;	  because resolution not good
;
;       April 2017 - SEG - added fix for RDRIV
;		case quantity=NINST, occulter on
;
;	May 2017 - SEG -- updated to work with revised FOR_TRACEBACK
;
;       October 2017 -- SEG -- made adjustments to work with UVCODES
;	June 2018 --- SEG -- replaced zeros in EXPFAC with -9999
;	July 2018 -- SEG -- made Vr absolute value when field-aligned
;	June 2019 -- added BHOR
;	August 2020 -- SEG -- update IONDENS to show DENS if not set
;		also fixed unassigned velimpose
;	Sept 2021 -- SEG -- changed condition if ne CARRMAP 
;		 because messign up USER
;	        and did some other edits to clarify notdisk for PLANEOFSKY
;		and to fix bugs associated with USER
;		Also moved/adjusted code mapping CarrMap CMER up to r=height
;		Generally decided that dodisk should only impact gridtype PLANEOFSKY
;		passed through nowidgmesss
;	March 2022 -- changed xoffset --> losoffset
;		updated for TLOS 
;	April 2022 -- passed through working_dir and changed to it
;		before running model -- because TURBHY needed it
;	July 2022 -- fixed bug losffset -->losoffset
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;	Jul 2023 -- added call to for_iondens
;	Aug 2023 -- moved diagnostics higher up so can diagnose issues before model call
;	Sep 2023 -- added hooks for AWSOM
;		added hooks for iondens
;	Oct 2023-- added ModPramsStruct to call to FOR_IONDENS
;			passed r3D and r through to FOR_IONDENS
;	Jan 2024 -- added possibility of extracting topology open=1, closed=0
;		note the sign is also included even for closed (br)
;	Jun 2024 -- commented redundant filees
;	                commented out multipliction of EXPFAC by OPEN
;			so now CLOSED field lines show EXPFAC too
;-

	default,norunmodel,0

	mdtor=!dpi/180d0
;  pi/2
	pid2= !dpi/2.d0


; both limbs will be represented in the plane of the sky:
;  the angle phi is the central meridian angle
; 
; alfa +1 for West (right) limb
; alfa -1 for East (left) limb
;
;
; make sure that theta is not too close to the poles or alfa will blow up
;
	test=where(abs(theta) lt 1d-5)
	if min(test) ne -1 then theta[test]=1d-5
	test=where(abs(theta-double(!dpi)) lt 1d-5)
	if min(test) ne -1 then begin
		thetafix=theta[test]
		test2=where(thetafix gt double(!dpi))
		if min(test2) ne -1 then thetafix[test2]=double(!dpi)+1d-5
		test3=where(thetafix le double(!dpi))
		if min(test3) ne -1 then thetafix[test3]=double(!dpi)-1d-5
		theta[test]=thetafix
	endif
	test=where(abs(theta-2.d0*double(!dpi)) lt 1d-5)
	if min(test) ne -1 then theta[test]=2.d0*1d-5

;
;  establish points not under solar disk (or occulters)
;  for POS we will assume Cartesian is desired
;  unless occult/upoccult set to negative (dashed line, no data removed)
;
        odid=0
        udid=0
        ddid=0
        if is_number(LosPramsStruct.Occult) then if LosPramsStruct.Occult ge 1. then odid=1
        if is_number(LosPramsStruct.UpOccult) then if LosPramsStruct.UpOccult gt 1. and LosPramsStruct.UpOccult lt 1000.d0 then udid=1
        if is_number(LosPramsStruct.DoDisk) then if LosPramsStruct.DoDisk ne 0. then ddid=1

        if odid eq 1 and udid eq 1 and ddid eq 0 then notdisk=where(r ge LosPramsStruct.Occult and r le LosPramsStruct.UpOccult)
        if odid eq 1 and udid eq 0 and ddid eq 0 then notdisk=where(r ge LosPramsStruct.Occult)
        if odid eq 0 and udid eq 1 and ddid eq 0 then notdisk=where(r ge 1. and r le LosPramsStruct.UpOccult)
        if odid eq 0 and udid eq 0 and ddid eq 0 then notdisk=where(r ge 1.)
        if odid eq 0 and udid eq 1 and ddid eq 1 then notdisk=where(r le LosPramsStruct.UpOccult)
        if odid eq 0 and udid eq 0 and ddid eq 1 then notdisk=-2

;   -- Neither CARRMAP nor USER should worry about notdisk for plane of sky quantities
;      and note that for_get_grid takes user inputted spherical points and determines
;	 POS coords (r,theta,phi) and losoffset for third dimension.
; 	 So should be dealt with basically like PlaneOfSky or CarrMap (other than CMER) below
;
	losoffset=GridPramsStruct.LosOffset
 	if n_elements(losoffset) eq 1 then begin
         if is_number(losoffset) eq 0 then losoffset = 0.0
        endif else begin	
         checkoffset=where(is_number(losoffset) eq 0)
	 if min(checkoffset) ne -1 then losoffset[checkoffset]=0.0
	endelse

        if strupcase(GridPramsStruct.GridType) ne 'PLANEOFSKY' then begin
         ruse=r
         thuse=theta
	 losoffuse=losoffset
	 notdisk=-3
;
; so notdisk=-1 happens if PLANEOFSKY and no data with rpos within limits set by occult,upoccult,dodisk
;  notdisk=-2 happens if PLANEOFSKY and dodisk set and upoccult not
;  notdisk=-3 is USER or CARRMAP
;
        endif else begin
         if min(notdisk) ge 0 then begin
;
;  don't bother doing calculation below disk 
;
          ruse=r[notdisk]
          thuse=theta[notdisk]  
	  losoffuse=losoffset[notdisk]
	 endif
;
; check to be sure there are any points worth calculating at all
;
         if min(notdisk) eq -1 then begin 
	  ruse=-9999
         endif 
;
; and keep all points if dodisk is set (forcing occult=0) and upoccult not set
;
         if min(notdisk) eq -2 then begin 
          ruse=r
          thuse=theta
	  losoffuse=losoffset
         endif 
        endelse

        if min(ruse) ne -9999 then begin
;
; if CMER CarrMap
; map points up to sphere r=rheight
;
	 if strupcase(GridPramsStruct.Limb) eq 'CMER' $
	   and strupcase(GridPramsStruct.GridType) eq 'CARRMAP' then begin 	
            r3D=GridPramsStruct.RHeight + ruse*0.
  	    theta3D=acos(ruse/r3D)
 	    phi3D = 0.d0
	 endif else begin
; 
;  now determine 3D points. This will depend on whether
;  the POS map is centered on the X=0 plane (XLOS) or the Thomson Sphere (TLOS, TAU)
;
;  note: these are now 1D arrays if notdisk
; 
	  alfa = -sin(thuse)/abs(sin(thuse))   ;theta  is polar angle 0 - 2*pi - note counterclockwise!
;
  	  if strupcase(LosPramsStruct.LosUse) eq 'XLOS' then begin 
	   theta3D = acos(cos(thuse))
 	   phi3D = alfa*pid2 
	   y = ruse*sin(theta3D)*sin(phi3D)
	   z = ruse*cos(theta3D)
	   r3D = sqrt(ruse*ruse+losoffuse*losoffuse)
	   theta3D = acos(z/r3D)
	   phi3D = atan(y,losoffuse)
;
; deal with on disk if necessary for PlaneOfSky
;  (Planeofsky disk points up to the r=dodisk sphere)
;
           if LosPramsStruct.DoDisk ne 0. and strupcase(GridPramsStruct.GridType) eq 'PLANEOFSKY' then begin
	    y3d = r3D*sin(theta3D)*sin(phi3D)
	    z3d = r3D*cos(theta3D)
	    ondisk3D = where(r3D le LosPramsStruct.DoDisk,c)
	    if c gt 0. then begin
             r3D[ondisk3d]=LosPramsStruct.DoDisk
  	     theta3D[ondisk3d]=acos(z3d[ondisk3d]/r3D[ondisk3d])
 	     phi3D[ondisk3d] = atan(y3d[ondisk3d],sqrt(abs(r3D[ondisk3d]^2-(z3D[ondisk3d]^2+y3D[ondisk3d]^2))))
	    endif
	   endif

	  endif else begin ; end check for XLOS
	   distobs=GridPramsStruct.DistObs
  	   elonguse=atan(ruse/distobs) 
; spherical coordinates of intersection with Thomson Sphere
    	   rpuse=ruse*cos(elonguse)
	   thpuse=acos(cos(elonguse)*cos(thuse))
	   tauuse=atan(losoffuse,rpuse)
	   r3D=rpuse/cos(tauuse)
           argument=cos(tauuse+elonguse)*cos(thuse)
           theta3D = acos(argument)
	   phuse=asin(sin(tauuse+elonguse)/sin(theta3D))
	   phi3D=alfa*(pid2-phuse)
	  endelse

	 endelse

;
; do coordinate rotations to model coordinate frame
;

	 for_dorots,thmod,phmod,r3D,theta3D,phi3D,phi,LosPramsStruct.Bang,LosPramsStruct.Thetao
;
; make sure r3D < 1 results in NaN
;
	 test = where(r3D lt 1.0,p)
	 if p gt 0 then r3D[test]=sqrt(-1)

	 rmod=r3D

;
;  Special case NINST
;
	 rdriv=rmod*0.
 	 if keyword_set(deltar) eq 1 then begin
          rmod=rmod+deltar
          rdriv=rmod-1.d0
         endif

;
; change directory to working_directory where I/O will occur
; otherwise I/O will occur in directory for_drive is called from
;   (needed for TURBHY)
;

        if working_dir ne '' then cd,working_dir,current=old_dir

; diagnostics
;        phobs=(phi3D+phi)/mdtor
;        test=where(phobs lt 0.d0)
;        if min(test) ne -1 then phobs[test]=phobs[test]+360.d0
;        test=where(phobs gt 360.d0)
;        if min(test) ne -1 then phobs[test]=phobs[test]-360.d0
;	print,'PLANE OF SKY',r,theta/!dtor,phi/!dtor
;	print,'LOSOFFSET',GridPramsStruct.LOSoffset
; 	print,'MODEL 3D',rmod,thmod/!dtor,phmod/!dtor
; 	print,'OBS 3D',r3d,theta3d/!dtor,phobs
;	print,'Dodisk',Lospramsstruct.dodisk
;	print,'occult',Lospramsstruct.occult

; 
; calculate model structure (containing magnetic field, density, temperature, etc.)
; but use ModSolStruct if passed through
;
         if norunmodel eq 0 then begin
          IF strupcase(ModPramsStruct.Name) EQ 'NUMCUBE' $
           or strupcase(ModPramsStruct.Name) EQ 'ADAPTCUBE' $
           or strupcase(ModPramsStruct.Name) EQ 'AWSOM' $
           or strupcase(ModPramsStruct.Name) EQ 'PSIMAS' THEN BEGIN
             IF strupcase(ModPramsStruct.Name) EQ 'NUMCUBE' THEN $
                call_procedure,ModPramsStruct.Name,rmod,thmod,phmod,ModPramsStruct,ModSolStruct,nreinit=nreinit,nowidgmess=nowidgmess $
                ELSE call_procedure,ModPramsStruct.Name,rmod,thmod,phmod,ModPramsStruct,ModSolStruct,nreinit=nreinit
          ENDIF ELSE call_procedure,ModPramsStruct.Name,rmod,thmod,phmod,$
		ModPramsStruct,ModSolStruct
	  nreinit=0
	 endif

;          return to original directory for_drive was called from:

         if working_dir ne '' then cd,old_dir

         if tag_exist(ObsPramsStruct,'Pop2TRegime') then begin
          if tag_exist(ModSolStruct,'RegimeForce') then begin
           if ObsPramsStruct.Pop2TRegime ne 0 then begin
             if ObsPramsStruct.Pop2TRegime ne ModSolStruct.RegimeForce then begin
              if keyword_set(nowidgmess) then message,/info,'POP2TREGIME changed to '+strtrim(string(ModSolStruct.RegimeForce),2)+' as required by model.' else d=dialog(/warning,'POP2TREGIME changed to '+strtrim(string(ModSolStruct.RegimeForce),2)+' as required by model.')
              ObsPramsStruct.Pop2TRegime = ModSolStruct.RegimeForce
            endif
           endif
          endif
         endif

         testdens = where(ModSolStruct.Dens lt 0.,c)
         if c gt 0 then message,'Negative densities',/info
;
; we need to project any vector (e.g. B) onto the observer's frame coords 
; (r3D,theta3D,phi3D) from (rmod,thmod,phmod)
;
	 if strupcase(ObsPramsSTruct.LineName) eq 'BR' $
	  or strupcase(ObsPramsSTruct.LineName) eq 'BTH' $
	  or strupcase(ObsPramsSTruct.LineName) eq 'BPH' $
	  or strupcase(ObsPramsSTruct.LineName) eq 'BX' $
	  or strupcase(ObsPramsSTruct.LineName) eq 'BY' $
	  or strupcase(ObsPramsSTruct.LineName) eq 'BZ' $
	  or strupcase(ObsPramsSTruct.LineName) eq 'EXPFAC' $
	  or strupcase(ObsPramsSTruct.LineName) eq 'BPOS' $
	  or strupcase(ObsPramsSTruct.LineName) eq 'BHOR' $
	  or strupcase(ObsPramsSTruct.LineName) eq 'BXDIVBMAG' $
	 then begin
       		for_vectunrot,ModSolStruct.Br,$
			ModSolStruct.Bth,ModSolStruct.Bph,$
                       	Brobs,Bthobs,Bphobs,rmod,thmod,phmod,$
                       	phi,LosPramsStruct.Bang,LosPramsStruct.Thetao
	 endif 

;
; take care of velocity
;
         if tag_exist(ModPramsStruct,'VELIMPOSE') then velimpose=ModPramsStruct.VelImpose else velimpose=0.

	 if strupcase(ObsPramsStruct.LineName) eq 'VX' or strupcase(ObsPramsStruct.LineName) eq 'VY' or $
	  strupcase(ObsPramsStruct.LineName) eq 'VZ' or strupcase(ObsPramsStruct.LineName) eq 'VR' or $
	  strupcase(ObsPramsStruct.LineName) eq 'VTH' or strupcase(ObsPramsStruct.LineName) eq 'VPH' then begin
		        if tag_exist(ModSolStruct,'VR') and tag_exist(ModSolStruct,'VTH') and tag_exist(ModSolStruct,'VPH') and velimpose eq 0. then begin 
			  for_vectunrot,ModSolStruct.Vr,ModSolStruct.Vth,ModSolStruct.Vph,$
                       	    VelR,VelTh,VelPh,rmod,thmod,phmod,$
                            phi,LosPramsStruct.Bang,LosPramsStruct.Thetao
		  	endif else begin
                          if tag_exist(ModSolStruct,'Vel') or velimpose ne 0. then begin
                           Bmag=sqrt(ModSolStruct.Br^2+$
                                ModSolStruct.Bth^2+$
                                ModSolStruct.Bph^2)
			   test=where(Bmag eq 0.d0)
			   if min(test) ne -1 then Bmag[test]=1.d0

 			   if velimpose ne 0. then begin
		             usevel=velimpose
;
; if topology set -- only put velocity on open field lines
;
			     if tag_exist(modsolstruct,'open') then begin
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
                               phi,LosPramsStruct.Bang,LosPramsStruct.Thetao
			       VelR=abs(VelR)
			   if min(test) ne -1 then Velr[test]=0.d0  
			   if min(test) ne -1 then Velth[test]=0.d0  
			   if min(test) ne -1 then Velph[test]=0.d0  
			  endif else begin
			    Velr=0.d0*rmod 
			    Velth=0.d0*rmod 
			    Velph=0.d0*rmod 
			  endelse
			endelse
	 endif

	 case strupcase(ObsPramsStruct.LineName) of
 	    'OPEN': begin
		     if tag_exist(modsolstruct,'open') then begin
			qntuse=ModSolStruct.Open
		     endif else begin
	                 if keyword_set(nowidgmess) then message,/info,'No topological info available.'  else d=dialog(/warning,'No topological info available.')
			 qntuse=0.d0*rmod
		     endelse
            end
            'VR': qntuse=Velr
            'VTH': qntuse=Velth
            'VPH': qntuse=Velph
            'VX': begin
                  	qntuse= Velr*sin(theta3D)*cos(phi3D)+$
                        Velth*cos(theta3D)*cos(phi3D)-$
                        Velph*sin(phi3D)
            end
            'VY': begin
                  	qntuse= Velr*sin(theta3D)*sin(phi3D)+$
                        Velth*cos(theta3D)*sin(phi3D)-$
                        Velph*cos(phi3D)
            end
            'VZ': begin
                  	qntuse= Velr*cos(theta3D)-$
                        Velth*sin(theta3D)
            end
            'BR': qntuse=Brobs
            'BTH': qntuse=Bthobs
            'BPH': qntuse=Bphobs
            'BX': begin
                  	qntuse= Brobs*sin(theta3D)*cos(phi3D)+$
                        Bthobs*cos(theta3D)*cos(phi3D)-$
                        Bphobs*sin(phi3D)
            end
            'BY': begin
                   	qntuse= Brobs*sin(theta3D)*sin(phi3D)+$
                        Bthobs*cos(theta3D)*sin(phi3D)+$
                        Bphobs*cos(phi3D)
            end
            'BZ': begin
                   	qntuse= Brobs*cos(theta3D)-$
                        Bthobs*sin(theta3D)
            end
            'BHOR': begin
		      ;for NINST then sqrt(bth^2+bph^2 - unrotated)
                      qntuse=sqrt(ModSolStruct.Bth^2 + ModSolStruct.Bph^2) 
            end
            'BPOS': begin
                      ;Bpos=sqrt(by^2 +bz^2)
                      By=Brobs*sin(theta3D)*sin(phi3D)+$
                         Bthobs*cos(theta3D)*sin(phi3D)+$
                         Bphobs*cos(phi3D)
                      Bz=Brobs*cos(theta3D)-$
                         Bthobs*sin(theta3D)
                      qntuse=sqrt(By^2 + Bz^2)
            end
            'BXDIVBMAG': begin
                        Bmag=sqrt(ModSolStruct.Br^2+$
                                ModSolStruct.Bth^2+$
                                ModSolStruct.Bph^2)
		        test=where(Bmag eq 0.d0)
			if min(test) ne -1 then Bmag[test]=1.d0
                  	qntuse= (Brobs*sin(theta3D)*cos(phi3D)+$
                        	Bthobs*cos(theta3D)*cos(phi3D)-$
                        	Bphobs*sin(phi3D))/Bmag
            end
            'BMAG': begin
                      Bmag=sqrt(ModSolStruct.Br^2+$
                                ModSolStruct.Bth^2+$
                                ModSolStruct.Bph^2)
                      qntuse=Bmag
            end
            'DENS': begin
;
; take care of situations where a Pop1 is defined and should be used as Dens
;
                        if tag_exist(ModSolStruct,'Pop1Dens') $
			  then qntuse=ModSolStruct.Pop1Dens $
			  else qntuse=ModSolStruct.Dens
            end
 	    'POP2DENS': begin
                     if tag_exist(ModSolStruct,'Pop2Dens') then begin
                        if ndim(ModSolStruct.Pop2Dens) eq 0 then begin
                          qntuse=ModSolStruct.Pop2Dens + r3D*0.
                        endif else qntuse=ModSolStruct.Pop2Dens
     		     endif else begin
	                 if keyword_set(nowidgmess) then message,/info,'No distinct POP2DENS set. Showing DENS.' else d=dialog(/warning,'No distinct POP2DENS set. Showing DENS.')
                         qntuse=ModSolStruct.Dens
                     endelse
            end
            'TEMP': qntuse=ModSolStruct.Temp
 	    'POP2TEMP': begin
                     if tag_exist(ModSolStruct,'Pop2Temp') then qntuse=ModSolStruct.Pop2Temp $
     			else begin
	                 if keyword_set(nowidgmess) then message,/info,'No distinct POP2TEMP set. Showing TEMP.' else d=dialog(/warning,'No distinct POP2TEMP set. Showing TEMP.')
                         qntuse=ModSolStruct.Temp
                      endelse
            end
            'PRES': begin
;                     Thermal Pressure:
                      Pres = ModSolStruct.Pres
                      qntuse=Pres
            end
            'PTOT': begin
;                     Total Pressure:
                      Bmag=sqrt(ModSolStruct.Br^2+$
                                ModSolStruct.Bth^2+$
                                ModSolStruct.Bph^2)
                      Ptot=ModSolStruct.Pres + Bmag/8.d0/!dpi
                      qntuse=Ptot
            end
	    'STREAM': begin
		     qntuse=ModSolStruct.Stream
	    end
            'BETA': begin
                      Pthermal = ModSolStruct.Pres
                      Bmag2=(ModSolStruct.Br^2+$
                             ModSolStruct.Bth^2+$
                             ModSolStruct.Bph^2)
                      Pmag = Bmag2/(8.d0*!dPi)
		      Pzero = where(Pmag eq 0.)
                      beta = Pthermal/Pmag
		      if min(Pzero) ge 0. then beta[Pzero] = 0.
                      qntuse=beta
            end
	    'EXPFAC': begin
		      if strupcase(ModPramsStruct.Name) NE 'PFSSMOD' then begin
			 if tag_exist(ModSolStruct,'ExpFac') eq 0 then begin
	                  if keyword_set(nowidgmess) then message,/info,'Expansion Factor only implemented for PFSS so far. Showing Zeros.' else d=dialog(/warning,'Expansion Factor only implemented for PFSS so far. Showing Zeros.')
                          qntuse=ModSolStruct.Br*0.d0
 			 endif else begin
;
; warning, these might not be very good -- probably should recalculate
;  as is done for PFSS below
;
  			  expfac=ModSolStruct.ExpFac
;
; there should not be any exactly zero expansion factor
;
			  zerotest=where(expfac eq 0.)
			  if min(zerotest) ne -1 then expfac[zerotest] = -9999.
			  
;
; this next line is for diagnostic purposes: comment when not using
;
;  			  expfac=ModSolStruct.Open
		          qntuse=expfac
			endelse
		      endif else begin
;		       if tag_exist(ModSolStruct,'ExpFac') eq 0 then begin
;
; figure out the magnetic field at the base of open field lines
;  (unless already in PFSS data cube)
;
; NOTE I HAVE COMMENTED OUT THIS SECOND OPTION because the interpolated
; expansion factor from the PFSS data cube field lines (topo cases) 
; are not good enough generally. Thus, field lines will be calculated
; through every point in the plane of sky and expansion factor calculated.
;

		        for_traceback,rmod,thmod,phmod,ModSolStruct.Br,ModSolStruct.Bth,ModSolStruct.Bph,Brphot1,Brphot2,Bthphot1,Bthphot2,Bphphot1,Bphphot2,linelengths,open,ModPramsStruct,nreinit=nreinit

;
; calculate expansion factor 
;   NOTE USING FIRST FOOTPOINT INFO ONLY -- appropriate for open
;
                        Bphotmag=sqrt(Brphot1^2+$
                                Bthphot1^2+$
                                Bphphot1^2)
                        Bmag=sqrt(ModSolStruct.Br^2+$
                                ModSolStruct.Bth^2+$
                                ModSolStruct.Bph^2)
;		        expfac=Bphotmag*open/Bmag/rmod/rmod
		        expfac=Bphotmag/Bmag/rmod/rmod
			zerotest=where(expfac eq 0.)
			if min(zerotest) ne -1 then expfac[zerotest] = -9999.
;		       endif else begin
; 			expfac=ModSolStruct.ExpFac
;		 	open=ModSolStruct.Open
;			zerotest=where(expfac eq 0.)
;			if min(zerotest) ne -1 then expfac[zerotest] = -9999.
; 		       endelse
		       qntuse=expfac
		      endelse
	    end
            'R': qntuse=r3D
            'THOBS': qntuse=theta3D/mdtor
            'PHOBS': begin
        		phobs=(phi3D+phi)/mdtor
        		test=where(phobs lt 0.d0)
        		if min(test) ne -1 then phobs[test]=phobs[test]+360.d0
        		test=where(phobs gt 360.d0)
        		if min(test) ne -1 then phobs[test]=phobs[test]-360.d0
                        qntuse=phobs
            end
            'THMOD': qntuse=thmod/mdtor
            'PHMOD': qntuse=phmod/mdtor
 	    'CAVHEIGHT': qntuse=ModSolStruct.CavHeight
 	    'CAVWIDTH': qntuse=ModSolStruct.CavWidth
 	    'CAVTOPR': qntuse=ModSolStruct.CavTopR
 	    'CAVTOPT': qntuse=ModSolStruct.CavTopT
 	    'THCS': qntuse=ModSolStruct.ThCs
; gamma here is tilt angle for cavmorph
 	    'GAMMA': qntuse=ModSolStruct.Gamma
 	    'FILLFACT': begin
                 if tag_exist(ModSolStruct,'FillingFactor') then begin
                   if ndim(ModSolStruct.FillingFactor) eq 0 then begin
                     qntuse=ModSolStruct.FillingFactor + r3D*0.
                   endif else qntuse=ModSolStruct.FillingFactor
     		 endif else begin
	              if keyword_set(nowidgmess) then message,/info,'No distinct FILL set. Set to 1.' else d=dialog(/warning,'No distinct FILL set. Set to 1.')
 	              qntuse=1. + r3D*0.
                 endelse
            end
 	    'POP2FILLFACT': begin
                 if tag_exist(ModSolStruct,'Pop2FillingFactor') then begin
                   if ndim(ModSolStruct.Pop2FillingFactor) eq 0 then begin
                     qntuse=ModSolStruct.Pop2FillingFactor + r3D*0.
                   endif else qntuse=ModSolStruct.Pop2FillingFactor
     		 endif else begin
	              if keyword_set(nowidgmess) then message,/info,'No distinct POP2FILL set. Set to 1.' else d=dialog(/warning,'No distinct POP2FILL set. Set to 1.')
 	              qntuse=1. + r3D*0.
                 endelse
            end
            else: begin
; special case
	         if (strupcase(ObsPramsStruct.Instrument) eq 'IONDENS') then begin
                    if tag_exist(ModSolStruct,'IonDens') then qntuse=ModSolStruct.IonDens $
 		     else begin
                      for_iondens,ObsPramsStruct,ModSolStruct,ModPramsStruct,r3d,ruse,iondens,nowidgmess=nowidgmess
	               qntuse=iondens
	 	     endelse
		 endif
            end
         endcase

;	print,'Quantity',qntuse

	endif
; (end conditional checking if there are any points worth calculating)

;  notdisk=-1 happens if PLANEOFSKY and no data with rpos within limits set by occult,upoccult,dodisk
;  notdisk=-2 happens if PLANEOFSKY and dodisk set and upoccult not
;  notdisk=-3 is USER or CARRMAP

        qntall=r*0. -9999.
        rdrivall=r*0.
        if min(notdisk) ge 0 then begin
; this will happen for PLANEOFSKY with some points NOTDISK
           qntall[notdisk]=qntuse 
	   rdrivall[notdisk]=rdriv
        endif else begin
         if min(notdisk) ne -1 then begin
;
; this will happen for PLANEOFSKY with DODISK set (and UPOCCULT not)
; or USER or CARRMAP
;  notdisk=-2 or -3
;
          qntall=qntuse
	  rdrivall=rdriv
	 endif else begin
; this will happen if no non-under disk points PLANEOFSKY        
;  notdisk=-1
         endelse
        endelse
	rdriv=rdrivall
	end
;************************************************************************
