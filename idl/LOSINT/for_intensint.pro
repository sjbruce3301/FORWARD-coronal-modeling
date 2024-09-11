;************************************************************************
     pro for_intensint,intensall,r,theta,phi,LosPramsStruct,ObsPramsStruct,ModPramsStruct,$
       GridPramsStruct,datadump,StokesStruct,WLStruct,reinit=reinit,nreinit=nreinit,memory=memory,verbose=verbose,$
        bg_input=bg_input,bg_output=bg_output,$
	mapname=mapname,working_dir=working_dir,parallel=parallel,nowidgmess=nowidgmess

;+
; Name: FOR_INTENSINT
;
; Purpose:  calculates integrated line-of-sight integrated observable
;			from plasma properties (e.g., N, T, B)
;			defined by model (ModPramsStruct.Name) 
;			in spherical coordinates (r3D, theta3D, phi3D)
;			and integrated over tau  (line of sight angle)
;			or xlos (line of sight distance)
;
; 			note:  coordinates r,theta,phi are plane of sky coordinates
;  			r3D,theta3D,phi3D are "physical" spherical 3-d coordinates. 
; INPUTS
;	r - r in plane of the sky
;	theta - theta (polar angle, 0 - 2 pi, counterclockwise from north) in plane of the sky
;	phi - central meridian longitude 
; 	
;	LosPramsStruct - Structure containing parameters for the line of sight integration
;		LOSUSE - line of sight angle TAU (default) or distance along line of sight XLOS 
;      		LOSMIN -  minimum line of sight angle
;                       LOSUSE = TAU default -pi/2 for infinite LOS
;                       LOSUSE = XLOS default -1.
;			LOSUSE = TLOS default -1 (but scaled by elongation
;                               note xlos = 0 is plane of the sky
;				   tau=0, tlos=0 is at Thomson Sphere
;      		LOSINT -  resolution in along the line of sight, default 0.01 (radians)
;                       LOSINT = TAU default 0.01 (RADIANS)
;                       LOSINT = XLOS default 0.01 (RSUN)
;                       LOSINT = TLOS default 0.01 (RSUN)
;      		NLOS - number of points to integrate along line of sight
;                       default generally determined for symmetry`
;                       if AXISYM set, cut in half
;		DODISK - do calculation on disk of sun
;               AXISYMMULT - default = 1; if axisymm set to 2, NLOS is halved so double LOS integration
;               BANG -- bang solar B angle IN RADIANS, default zero -- nonzero requires call to procedure FOR_THROT 
;               THETAO -- thetao latitude rotation IN RADIANS, default 90 -- thetao ne 90 requires call to procedure FOR_THROT 
;
;	ObsPramsStruct - Structure containing description of the observable to be modeled
;		(see OBS_NAME for descriptions of the possible observation types)
;               INSTRUMENT, (unless LABELONLY set)
;               LINENAME, (unless LABELONLY set)
;               LINENUM (number with one to one correspondence to LINENAME), (unless LABELONLY set)
;               LABEL text lable for plots
;	ModPramsStruct - Structure containing name and model parameters
;		NAME
;		model parameters as specified in MOD_NAME function
;	GridPramsStruct
;
; 	DATADUMP - turns off chunking and outputs datadump file
; KEYWORDS INPUTS: 
;
;  	REINIT - For speed. Set reinit=1 on first time in a loop, 
;                    to calculate response functions.  On subsequent times,
;                    use reinit = 0. 
;		     However, if Pop2TRegime=1, will always recalculate 
;			response functions independent of reinit.
;			Default=1
;	NREINIT - first time through numerical code loads up data cube, after that common block
;
;		NOTE - WAVECOMP is no longer needed, because if any of the 
;			polarimeter lines *COMP are called with line=VLOS
;			the ATOM and INPUT files are used for wavelength
;			resolved case. It is still carried around for
;			backwards compatibility.
; 	VERBOSE - allows comments to print
;
;	WORKING_DIR -- if set, executable is run from that directory and all I/O goes there
;	PARALLEL - enables running in parallel batch mode (see FOR_SETTINGDEFAULTS)
;
; OUTPUTS:
;	INTENSALL-output array of observable
;
;       STOKESSTRUCT - If comp (or fe11comp or si9comp or si10comp or cormag or wavecomp) or raido or UV is 
;		  set, also output a stokes structure containing all of Stokes 
;		  paramaters (not just the one set to intensall)
; 
; Calls: MODPRAMSSTRUCT.NAME,
;		FOR_DOROTS -- which calls FOR_THROT 
;		FOR_DOPOP2INIT, FOR_INTPOS, FOR_INTCOMPSETUP
;		FOR_INTMEM, FOR_INTLOS, FOR_RADIOCALC, FOR_INTEGRATE, FOR_UVCALC,
;		FOR_COMPOUTPUT, FOR_EUVSXRCALC, FOR_PBBCALC, FOR_DENSTEMPFIX
;		FOR_DOPO2INIT, FOR_INTPOS, FOR_INTCOMPCLOSE, FOR_POLSTRUC, FOR_DUMPCUBE
;		Fortran code FORCOMP (testing IDL version) called within FOR_INTCOMPCLOSE
;
; REQURES LASCO PACKAGE
;
; Called by: FOR_FORWARDMAP
;
; HISTORY: 
;	Written by Sarah Gibson, Jim Dove, Terry Kucera 2010-2015
;
; 	Version 2.0 July 2014
;	Split into subroutines November 2015
;	   Feb 2016 -- removed LyC_Reinit, added POP1DENS abilityo
;		added DLOS keyword to FOR_INTEGRATE, nunwrap to FOR_FILLIN
;	   early 2019 -- fixed call to for_fillin
;   	   June 2019 -- used slash for PC compatibility
;	   April 2020 -- removed dlos from call to for_fillin and for_integrate because unnecessary
;	   October 2020 -- added WL variable XPOLF, XPOLB, XPOLG
;	   November 2020 -- passed testnearuse through to for_integrate to deal
;		with far field points properly
;	   January 2021 -- broke apart functionality of VERBOSE and new DATADUMP inputs/keyword
;	   September 2021 -- filled intensall with -9999 for starters
;		put in a check or zero size nunwrap
;		passed through nowidgmess
;	   October 2021 -- added doprint to for_denstempfix
;	   November 2021 -- took out spaces for datadump, passed through mapname
;	   December 2021 -- passed GridPramsStruct through to 
;		for_integrate and for_pbbcalc so they have distobs
; 	   January 2022 -- added TPOLF, TPOLB, TPOLG angular distance from TS functionality
; 	   February 2022 -- add BG_INPUT, BG_OUTPUT to pass through to FOR_INTEGRATE
;			also passed working_Dir through with them
;		   also fixed an unlikely bug for datadump if working_dir not set
;	   March 2022 -- changed to TPOLU, TPOLS
;		changed xoff --> losoff
;		updated treatment of near/far
;	   April 2022 -- moved the change directory to working_dir into this program
;		not just for comp, for TURBHY
;          Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;		removed test for dimension of LOSOFF because now sorted with changes
;		to for_get_grid and for_intpos
;	   Sep 2023 -- added hooks for AWSOM
;		passed r_pos (r) through for_integrate
;	   Dec 2023 -- passed rbite through to for_fillin.pro
;	   Jul 2024 -- passed through ulimb
;			added PR
;	   Aug 2024 -- added WLRAT hooks
;		also removed call to for_integrate for VIR specpol
;
;-

        slash=path_sep()

;
; Get date from ObsPramsStruct some versions might not have it, so check

	if not tag_exist(ObsPramsStruct,'date') then date='' else date=ObsPramsStruct.date

; backward compatibility with Don Schmit's method for calculating FE line ratio
          if strupcase(ObsPramsStruct.LineName) eq 'FE12DE' or strupcase(ObsPramsStruct.LineName) eq 'FE12RATIO' then ObsPramsStruct.IClass='EISRATIO'
;
;	also make sure instrument is capitalized

	ObsPramsStruct.Instrument=strupcase(ObsPramsStruct.Instrument)


; UNWRAP
;If input arrays have more than 1D, reform them into 1D arrays

	CoordSize=size(r)	
	nx=CoordSize[1]
	NDim=CoordSize[0]
	nunwrapall=product(CoordSize[1:NDim]) ; map input onto 1-D arrays of dimension nunwrapall
        nunwrapall=long(nunwrapall)
	r=reform(r,nunwrapall,/overwrite)
	theta=reform(theta,nunwrapall,/overwrite)
	phi=reform(phi,nunwrapall,/overwrite)

;
; output array to be filled and returned

	intensall=dblarr(nunwrapall)-9999.

;
;  remove occulted points to get reduced set nunwrap
;

        for_intpos,r,theta,phi,ObsPramsStruct,LosPramsStruct,GridPramsStruct,ruse,thuse,phuse,losoffuse,alfa,notdisk,nunwrap,verbose=verbose

;
; now here is the reduced array (not including disk) that we will do calculations for
;  note if there are no points above disk, nunwrap=0

        if nunwrap ne 0 then begin
	 intens=dblarr(nunwrap)
;
; here is the Stokes array for CLE
; also for radio, UV- can still use this for Faraday rotation,
; first array is rotation measure, second is Faraday rotation
;
	 stokesred=dblarr(6,nunwrap)
                    
;
; set up for CLE type polarimetry calculations
;
;
; first change directory to working_directory where I/O will occur
; otherwise I/O will occur in directory for_drive is called from
;	(this used to be inside the comp conditional but also need for TURBHY model
;
         if working_dir ne '' then cd,working_dir,current=old_dir

         if strpos(strupcase(ObsPramsStruct.instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.instrument) eq 'CORMAG' then begin
          for_intcompsetup,ObsPramsStruct,LosPramsStruct,nunwrap,working_dir,comprun,lun,verbose=verbose,nowidgmess=nowidgmess
	 endif

;  NOW - -MEMORY MAY BE AN ISSUE SO ALLOW POSSIBILITY OF MAKING BITE SIZE PIECES of LENGTH NMEM
; note if datadump is set it will turn off memory

         for_intmem,memory,nunwrap,nx,nbite,nmem,verbose=verbose,datadump=datadump


; Take nbites bites all of length nmem 
;  except the last if it is a non-integer remainder:        
;
; LOOP over bites if necessary

	 founddata=0
  	 t00 = SYSTIME(/SECONDS)
	 for ibite=0,nbite-1 do begin
          if (ibite mod 500 eq 0 and memory eq -1) or (memory ne -1) then doprint=1 else doprint=0
  	  t0 = SYSTIME(/SECONDS)

; Starting and ending line of sight address:

	  nst=ibite*nmem
	  nfin=nst+nmem-1

; Deal with the possibility of last bite not exactly fitting
; (might be fewer line of sights for the last bite)

	  if nfin gt nunwrap-1 then nfin = nunwrap-1

	  nactualbite=nfin-nst+1

	  rbite=ruse[nst:nfin]
	  thbite=thuse[nst:nfin]
	  phbite=phuse[nst:nfin]
	  alfabite=alfa[nst:nfin]
	  losoffbite=losoffuse[nst:nfin]

; DETERMINE spherical coords along LOS in viewer's coordinate system 
;		[r3D,theta3D,phi3D] dimension [nlos,nactualbite]
;      also determine line of sight vector (x2 or tau2) 
; 		and differential line of sight (dlos)
;			(note this is dx, even if using TAU -- in that case not equal space)
;

	  for_intlos,rbite,thbite,phbite,alfabite,losoffbite,LosPramsStruct,ObsPramsStruct,GridPramsStruct,r3D,theta3D,phi3D,r2,cmer2,x2,tau2,dlos,doprint,verbose=verbose

	  if verbose eq 1 then begin
	   print,'PLANE OF SKY',rbite,thbite/!dtor,phbite/!dtor
	   print,'LOSOFFSET',losoffbite[0]
	   print,'OBS 3D',r3D,theta3D/!dtor,(phi3D+cmer2)/!dtor
	  endif

;
;  determine physical Sun coordinates [rmod,thmod,phmod]
;	dimension [nlos,nactualbite]
 
;  keeping rmod as a variable, in case in future we use some weird stretchy model
	  rmod=r3D
;
;	now rotate away from viewer's coordinate system by bang, thetao, cmer (contained in phbite)
;

	  for_dorots,thmod,phmod,r3D,theta3D,phi3D,cmer2,LosPramsStruct.Bang,LosPramsStruct.Thetao

	  if verbose eq 1 then print,'MOD 3D',r3D,thmod/!dtor,phmod/!dtor
;
; don't bother doing calculation for excluded points (rmod ge 1000.)
;	these are points above threshold radius, or sometimes behind solar disk
;	  note if there are points r3D<1 along LOS remaining, it will
;	  invalidate entire LOS. FOR_INTLOS replaces the value of r3D with NaN
;	  for these points, which should result in NaN for physical quantities in
;	  model call
;	if any points are excluded, instead of [nlos,nactualbite] arrays will be 1D 
;	and so will be ModSolStruct.Dens e.g.
;	(except EISRATIO and RADIO (non Faraday) and CoMP which need to keep the NLOSXNPOS dimension
; 	(note RADIO should not have any points anyway, as defined by for_intlos; COMP and EISRATIO
;		could have them, but they will be calculated just far away)
;
	  testfar=where(rmod ge 1000.,far)
	  testnear=where(rmod lt 1000.,near)
 	  if far gt 0 and strupcase(ObsPramsStruct.Instrument) eq 'RADIO' then stop
          if near eq 0 then begin
                print, 'no non-excluded points'
		testnearuse=-2
	  endif else begin
	   if far gt 0 and near gt 0 and (strupcase(ObsPramsStruct.IClass) ne 'EISRATIO') and  strupcase(ObsPramsStruct.Instrument) ne 'RADIO' and strpos(strupcase(ObsPramsStruct.instrument),'OMP') le 0 and strupcase(ObsPramsStruct.instrument) ne 'CORMAG' and strupcase(ObsPramsStruct.LineName) ne 'WLRAT' then begin
		 rmoduse=rmod[testnear]
		 thmoduse=thmod[testnear]
		 phmoduse=phmod[testnear]
		 r2use=r2[testnear]
		 tau2use=tau2[testnear]
		 cmer2use=cmer2[testnear]
		 dlosuse=dlos[testnear]
		 testnearuse=testnear
	   endif else begin
		rmoduse=rmod
		thmoduse=thmod
		phmoduse=phmod
		r2use=r2
		tau2use=tau2
		cmer2use=cmer2
		dlosuse=dlos
		testnearuse=-1
	   endelse
	  endelse

	  if min(testnearuse) ne -2 then begin
; 
;  CALL MODEL to get physical quantities of model (N,T,B,V) at points along the line of sight.
;
	   IF strupcase(ModPramsStruct.Name) EQ 'NUMCUBE' $
	    or strupcase(ModPramsStruct.Name) EQ 'ADAPTCUBE' $
	    or strupcase(ModPramsStruct.Name) EQ 'AWSOM' $
            or strupcase(ModPramsStruct.Name) eq 'PSIMAS' THEN BEGIN
	     IF strupcase(ModPramsStruct.Name) EQ 'NUMCUBE' THEN $
		call_procedure,ModPramsStruct.Name,rmoduse,thmoduse,phmoduse,ModPramsStruct,ModSolStruct,nreinit=nreinit,nowidgmess=nowidgmess $
		ELSE call_procedure,ModPramsStruct.Name,rmoduse,thmoduse,phmoduse,ModPramsStruct,ModSolStruct,nreinit=nreinit
	   ENDIF ELSE call_procedure,ModPramsStruct.Name,rmoduse,thmoduse,phmoduse,ModPramsStruct,ModSolStruct

; ***
; NOTE ModSolStruct will be 1D with dimensions TESTNEARUSE if there are any points
; designated as do not use (rmod ge 1000).  By convention, if arrays have "use" in their name
; they are of consistent dimension with ModSolStruct
;
; ALSO NOTE that if any vector quantity is returned, it will be in (rmod,thmod,phmod) vector components
;  and if an observable uses these quantities, they must be transformed to the (r3D,theta3D,phi3D)
;  observer's frame
; such a transformation for vector B and vector V is done 
; in FOR_FIELDCALLS called by FOR_UVCALC, FOR_RADIOCALC, and FOR_COMPOUTPUT
;***
;stop

;
; set up for POP2 if set
;

           if reinit eq 1 then for_dopop2init,ObsPramsStruct,ModSolStruct,pop2tregime,nowidgmess=nowidgmess

;
; take care of situations where a Pop1 is defined, but should only be used as Dens
;  if pop2tregime ne 0 
; 
	   if pop2tregime ne 0 and tag_exist(ModSolStruct,'Pop1Dens') then ModSolStruct.Dens=ModSolStruct.Pop1Dens

;
; fix some densities for negative values
; (possible with some analytic model parameter regimes)

	   for_denstempfix,ModPramsStruct,ObsPramsStruct,ModSolStruct,denssignuse,doprint
            
;
; calculate integrand depending on observable (sumB or sumpB)
;	dimension [nlos,nactualbite]
;	(except EISRATIO and RADIOBITE (non Faraday) which are already integraed over nlos so just [nactualbite]

;
; WHITE LIGHT CORONAGRAPH
;

	   if (strupcase(ObsPramsStruct.LineName) eq 'PB') or (strupcase(ObsPramsStruct.LineName) eq 'TB') $
		or (strupcase(ObsPramsStruct.LineName) eq 'XPOLF') or (strupcase(ObsPramsStruct.LineName) eq 'XPOLB') $
		or (strupcase(ObsPramsStruct.LineName) eq 'TPOLU') or (strupcase(ObsPramsStruct.LineName) eq 'TPOLS') $
		or (strupcase(ObsPramsStruct.LineName) eq 'TPOLG') $
		or (strupcase(ObsPramsStruct.LineName) eq 'PR') $
		or (strupcase(ObsPramsStruct.LineName) eq 'XPOLG') or (strupcase(ObsPramsStruct.LineName) eq 'P') $
		then begin
                 if tag_exist(ObsPramsStruct,'ULimb') then ulimb=ObsPramsStruct.Ulimb else ulimb=0.63
		 for_pbbcalc,r3D,LosPramsStruct,ModSolStruct,GridPramsStruct,pop2tregime,tau2use,r2use,denssignuse,testnearuse,ulimb,sumB,sumpB
 		endif

   	   if strupcase(ObsPramsStruct.LineName) eq 'WLRAT' then $
	     for_codexoutput,ObsPramsStruct,ModSolStruct,ModPramsStruct,LosPramsStruct,rmod,thmod,phmod,r3D,theta3D,phi3D,cmer2,intensbite,wlspecbite,wavelengths

;
; EUV/SXR SPECT
;
	   if (strupcase(ObsPramsStruct.IClass) eq 'UV/EUV SPECTROMETERS') or $
	       (strupcase(ObsPramsStruct.IClass) eq 'EISRATIO') or $
	       (strupcase(ObsPramsStruct.IClass) eq 'EUV/XRAY IMAGERS') or $
	       (strupcase(ObsPramsStruct.IClass) eq 'VISIBLE SPECTROMETERS') or $
	       (strupcase(ObsPramsStruct.IClass) eq 'SXT') or $
	       (strupcase(ObsPramsStruct.IClass) eq 'LINE-OF-SIGHT INTEGRATED DIAGNOSTICS') $
		  then begin
		    for_euvsxrcalc,ObsPramsStruct,ModSolStruct,pop2tregime,r3D,dlosuse,denssignuse,testnearuse,reinit,emint,pop2emint,sumB,LyC_reinit=LyC_reinit,nowidgmess=nowidgmess
	          endif

;
; UV SPECTPOL
;
           if (strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS') then for_uvcalc,ObsPramsStruct,ModSolStruct,ModPramsStruct,LosPramsStruct,pop2tregime,rmoduse,thmoduse,phmoduse,r3D,theta3D,phi3D,cmer2use,testnearuse,StokesUV_I,StokesUV_Q,StokesUV_U

;
; RADIO SPECTPOL
;
           if (strupcase(ObsPramsStruct.Instrument) eq 'RADIO' or strupcase(ObsPramsStruct.Instrument) eq 'FARADAY') then for_radiocalc,ObsPramsStruct,ModSolStruct,ModPramsStruct,LosPramsStruct,rmoduse,thmoduse,phmoduse,r3D,theta3D,phi3D,dlosuse,cmer2use,testnearuse,radiobite

;
; VISIBLE/IR SPECTPOL
;
;
; physical state information for visible/IR polarimetry (fortran code input)
;
	   if  strpos(strupcase(ObsPramsStruct.Instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' then $
	     for_compoutput,ObsPramsStruct,ModSolStruct,ModPramsStruct,LosPramsStruct,rmod,thmod,phmod,r3D,theta3D,phi3D,cmer2,nfin,nst,lun,compbite,complbite,wavelengths,velbite,centbite,linebite,lchisqbite

;
; Now INTEGRATE (everything but visible/IR polarimetry or WL spectral ratio which integrate internally)!
;
	   if  strpos(strupcase(ObsPramsStruct.Instrument),'OMP') le 0 and strupcase(ObsPramsStruct.Instrument) ne 'CORMAG' and strupcase(ObsPramsStruct.LineName) ne 'WLRAT' then begin
             for_integrate,r3D,sumB,sumpB,rbite,ObsPramsStruct,LosPramsStruct,ModSolStruct,GridPramsStruct,x2,tau2,rbite,thbite,emint,pop2emint,StokesUV_I,StokesUV_Q,StokesUV_U,radiobite,testnearuse,intensbite,working_dir=working_dir,bg_input=bg_input,bg_output=bg_output
           endif

;
; now fill in the bite-sized chunks
; for array intens
; and also stokesred for radio/UV and NoFTRAN visible/IR polarimeters 
;

           for_fillin,ObsPramsStruct,LosPramsStruct,nst,nfin,r3D,x2,tau2,StokesUV_I,StokesUV_Q,StokesUV_U,rbite,radiobite,compbite,complbite,wavelengths,wlspecbite,velbite,centbite,linebite,lchisqbite,intensbite,nunwrap,intens,stokesred,stokeslred,wlspec,velocities,centwave,linewidth,lchisq,Qprime,Uprime

	   founddata=1

	  endif ; end check for any non-excluded data testnearuse ne -2

	  nreinit=0b
	  reinit=0b

          if verbose ne 0 then begin
            if doprint eq 1 then begin
	 	print,'for_intensint: number of seconds since run start',SYSTIME(/SECONDS)-t00
	 	print,'for_intensint: number of seconds since bite start',SYSTIME(/SECONDS)-t0
		print,'for_intensint: bite # = ',ibite
	    endif 
	  endif

         endfor

;      Done with memory bite loop!

;
; if running parallel=1 bail here
; and pass back zero intensity
; basically modelcube4comp and other FORCOMP inputs file exist at this point, and FORCOMP
; can be called from command line, and then FOR_DRIVE rerun with parallel=2
; See FOR_SETTINGDEFAULT
;
         if (strpos(strupcase(ObsPramsStruct.Instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG') and ObsPramsStruct.FCompPrams.noftran ne 1 then begin 
           if parallel eq 1 then begin
            free_lun,lun
            goto,bail
           endif
         endif
;
; make sure there is some data
;
	 if founddata eq 1 then begin
;
; make Stokes structure for radio and uv 

          if (strupcase(ObsPramsStruct.Instrument) eq 'RADIO' or $
           strupcase(ObsPramsStruct.Instrument) eq 'FARADAY') or $
           (strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS') then $
           for_polstruc,ObsPramsStruct,LosPramsStruct,notdisk,nunwrapall,CoordSize,NDim,stokesred,StokesStruct

; make WL spectral structure for WLRAT
	  if strupcase(ObsPramsStruct.LineName)  eq 'WLRAT' then $
            wlstruct={wlrat:intens,wlspec:wlspec,wl1:obspramsstruct.wavelength_ang,wl2:obspramsstruct.wavelength2_ang} 
;
; write out cube (note memory=0 will be forced so no bites)
; note also this could have the bad points removed, so 1D instead of 2D
; mainly for diagnostic purposes now
; 
          if datadump eq 1 then begin
           if keyword_set(mapname) eq 1 and mapname ne 'defaults' then prefix=mapname else begin
	    prefix= string(SYSTIME(/julian),format='(f18.7)')
            prefix=strcompress(prefix,/remove_all)
	   endelse
           if n_elements(working_dir) eq 1 then begin
            if working_dir ne '' then $
              datadumpfile = working_dir+slash+prefix+'_datadump.sav' $
              else datadumpfile=prefix+'_datadump.sav'
           endif else datadumpfile=prefix+'_datadump.sav'
	   for_dumpcube,ModSolStruct,ModPramsStruct,LosPramsStruct,cmer2,rmod,thmod,phmod,testnearuse,r3D,theta3D,phi3D,datadumpfile
	   cd,current=thedirectory
	   print,'wrote file ',datadumpfile,' from directory ',thedirectory
          endif

          if strpos(strupcase(ObsPramsStruct.Instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG' then $ 
          for_intcompclose,ObsPramsStruct,LosPramsStruct,comprun,lun,thuse,stokesred,stokeslred,wavelengths,velocities,centwave,linewidth,lchisq,parallel,notdisk,nunwrapall,CoordSize,NDim,intens,StokesStruct,verbose=verbose

	 endif
;          return to original directory for_drive was called from:
;		(used to be inside for_intcompclose but also needed for TURBHY)j

         if working_dir ne '' then cd,old_dir
;
; Parallel run bail point
         bail:

         if min(notdisk) ge 0 then intensall[notdisk]=intens else intensall = intens
;
; end conditional checking to see if any points under disk
;  nunwrap ne 0
;
        endif

;reformat to initial dimensions of the input coordinates
	
        if verbose eq 1 then begin
	  print,'Quantity',intensall
	  print,'LOSUSE',LosPramsstruct.LosUse
	endif

        intensall=LosPramsStruct.AxiSymMult*reform(intensall,CoordSize[1:NDim],/overwrite)
        r=reform(r,CoordSize[1:NDim],/overwrite)
        theta=reform(theta,CoordSize[1:NDim],/overwrite)
        phi=reform(phi,CoordSize[1:NDim],/overwrite)

;print,r3D,theta3D,phi3D,StokesStruct.I,StokesStruct.Q,StokesStruct.U,StokesStruct.V
;print,r3D,theta3D,phi3D,intensall
end
;************************************************************************
