;************************************************************************
;+
pro giblowprams,outarray,date=date,working_dir=working_dir,$
		      frontheight=frontheight0,legsang=legsang0,$
		      apar=apar0,ao=ao0,alnotrbub=alnotrbub0,$
		      sig1=sig10,gl_out=out0,pio=pio0, Bonly=Bonly0, $
		      C1Bonly=C1Bonly0,C2Bonly=C2Bonly0,$
                      aa=aa0,bb=bb0,cc=cc0,dd=dd0,ee=ee0,ff=ff0,$
		      isothermal=isothermal0,$
		      alpha=alpha0,velmult=velmult0,time=time0,$
		      bubbleonly=bubbleonly0,velimpose=velimpose0,$
		      saveprams=saveprams,readprams=readprams0
;
;Name: giblowprams
;
;Purpose: To create structure containing information concerning the
;         Gibson-Low CME model; To be called by driver routine and resulting
;         structure will be named ModPramsStruct (with ModPramsStruct.name='giblow')
;
; Called by FOR_MODELDEFAULTS
;
;;Keyword Inputs:  
;		   
;         MORPHOLOGY/POSITION
;
;		FRONTHEIGHT, LEGSANG-- these are the position of the front
;			of the bubble and the angle subtended by its legs
;			at the bubble's broadest point, for time = 0 (phiss=1)
;			legsang INPUT in DEGREES
;			these inputs along with apar will determine 
;			OUTPUTS xo and rbub (see below)	
;			DEFAULT FRONTHEIGHT=1.35, LEGSANG=25.
;
;         PHYSICAL PROPERTIES
;
;	        APAR - (also affects MORPHOLOGY) just as in the Bogdan and Low model,
;          		this allows an expansion of the field and introduces
; 		        currents. apar is the radial shift transformation used to make a
;          		tear-drop shape (r -> r + apar)
;			DEFAULT APAR=0.050  OUTPUT unchanged
;
;		AO - this is the constant of proportionality between
;	   		the stream function S and the pressure Pi
;			DEFAULT AO=1.75  OUTPUT divided by -Muse
;			so that the amplitude of Ao ends up being the
;			same as the maximum amplitude of Br 
;			(unstretched/bubble coords)
;			Note for Giblow Muse is negative
;
;	     	ALNOTRBUB - the eigenvalue parameter of the twisted flux
;	    		rope model -- OUTPUT alnot (rbub calculated below)
;			RESTRICTED TO EIGENVALUES OF J5/2 BESSEL FUNCTION
;			Just the first 6 from Abramewicz and Stegun
;			DEFAULT ALNOTRBUB=1,
;				corresponds to
;				smallest eigenvalue -- 5.763854
;			NOTE CHANGING SIGN CHANGES CHIRALITY 
;			    (direction of twist around "apple core" of spheromak)
;
; 		SIG1 - the rotation about the spheromak core (rotation
;           		about x-axis)  INPUT in DEGREES
;			DEFAULT SIG1=90  OUTPUT in RADIANS
;
;		(GL_)OUT -  a parameter which scales the outer field solution
;			since there is a discontinuity around bubble for field
;			(but not total pressure) this can be used to scale external
;			field as desired  DEFAULT out=.1  OUTPUT unchanged
;
;		PIO - this is a constant background pressure for the external solution
;			better not to use this DEFAULT PIO=0  OUTPUT unchanged
;
;    		AA,BB,CC,DD,EE,FF - background density, pressure parameters, 
;			for hydrostatic equilibrium OUTPUT as above but
;			aa, cc, ee  multiplied by 1d8 
;			see below for DEFAULT
;
;               BONLY - If set, force an isothermal solution and use a
;                       density that is simply a fraction of the
;                       background density.  E.G., set bonly=.5 to set 
;                       fraction to 0.5 (can also be > 1 for overdense structure,
;			with cavity substracture introduced via C1BONLY/C2BONLY below). 
;			This is used to get have
;                       realistic B field and density/temp useful for
;                       producing Stokes Parameters; also for modeling morphological
;			CMEs for coronal/heliospheric modeling
;			BUT NOTE NOT IN FORCE BALANCE
;			DEFAULT 0 (unset)
;
;		C1BONLY; C2BONLY -- only used if BONLY set; then creates a cavity
;			which is C1BONLY fractional density (relative to background)
;			and C2BONLY fractional height (relative to bubble)
;			DEFAULT C1Bonly=.5, C2Bonly=.9
;			(but only used if BONLY set)
;
;		ISOTHERMAL - temperature to set if doing BONLY
;			default 1.55d6
;
;	 	BUBBLEONLY - returns zero for all parameters outside the bubble
;
;
;      TEMPORAL PROPERTIES
;
;	 	ALPHA, VELMULT, TIME - these are parameters of the self-similar transform
;	     		alpha positive = accel, neg = decel, zero = const. vel
;			alpha nonzero requires an input file with velocity vs
; 			time, e.g. phiss_vs_t_0.490000_1.38000d-07.dat
;			velmult acts as a scaling factor for velocity 
;			if set to zero obtain magnetic field equivalent to
;				magnetostatic solution 
;			normalizes eta and alpha below (eta=velmult^2*1.38d-7)
;			time should be input in hours, will be converted to secs below
;			DEFAULT alpha=0 (const. vel; accel default alpha/eta=.49), 
;			velmult=.1, phiss=1, time=0
;			OUTPUT ALPHA*VELMULT^2, ETA=VELMULT^2*1.38d-7, PHISS (see below)
;
;  VELIMPOSE - impose a velocity of constant magnitude VELIMPOSE directed along the field
;               overwrites any velocity field already loaded in if nonzero
;			UNITS KM/SEC
;                       DEFAULT 0.d0
;
;	BOOKKEEPING
;
;              	SAVEPRAMS - if keyword set, write parameters to filename
;                       (which is the keyword value saveprams)
;                       or if set to 1, then return pramarray
;
;              	READPRAMS - if keyword set, read in parameters from filename
;                       (which is the keyword value filename)
;                       or if a structure than read directly
;      
; 
;;ModPramsStruct Outputs:  
;
;		NAME -- giblow-- so that procedure can be called in intensint
;		LABEL -- Gibson and Low -- for plot label
;
;		XO - this is the shift coordinates between
;	   		the bubble system and the sun's coordinate system.
; 	   		A choice of xo =1 should put the bubble at
;	   		the solar surface at the equator.  
;			Determined from frontheight, legsang, and apar
;			as evaluated at phiss=1, time=0
;
;		RBUB - this is the radius of the bubble.  Determined from 
;			values of frontheight, xo, and apar (phiss=1) 
;	
;		ALNOT -- this is the input eigen(alnotrbub)/rbub
;				eigen(1) = smallest eigenvalue -- 5.763854
;
;		SIG1 -- as above but converted to RADIANS
;
;		AA, CC, EE  -- as above but multiplied by 1d8
;
;		APAR, AO, OUT, PIO, BB, DD, FF, - as inputted
;		
; 		ALPHA, ETA -- scaled by VELMULT^2
;
;		PHISS -- the main self-similar coordinate transform parameter
;			phiss = 1 will be the magnetostatic solution
;			calculated from alpha, eta, and time
;			note that now this could be inputted
;			as array of same dimension as rpb.
;       		note this generally would be done such that
;       		(rpb,thetapb,phipb) and phiss are
;       		multidimensional arrays where one 
;			(or more) dimension is the spatial
;       		variation (which could be a single point) and
;       		the other dimension is temporal 
;			(thus, phiss is constant in
;       		the spatial dimension(s) and rpb,thetpb,phipb 
;			are constant in the temporal dimension)

;
;;Output: outarray - structure containing keyword output model parameters 
;
;Common Blocks: None
;
;Written: J. Dove, Jan 25, 2010
; revised S. E. Gibson, Feb 1, 2010
; revised to allow changes to saved parameters, May 18, 2010
; changed to procedure and adjusted readprams/saveprams I/O Jan 2013 SEG
; corrected text in message to list eigenvalues from 1-6 rather than 0-6; 
; also simplified syntax of conditionals for alnotrbubo -- Jan 2018 SEG
;	June 2019 used slash for PC compatibility
;	June 2019 - edited giblow.pro and getinside.pro to allow 
;		phiss to be an array
;	March 2022 -- changed velmult default to .1 to make more realistic
;		velocities in lower corona
;- 

slash=path_sep()

if n_elements(frontheight0) ne 0 then frontheight=frontheight0
if n_elements(legsang0) ne 0 then legsang=legsang0
if n_elements(apar0) ne 0 then apar=apar0
if n_elements(ao0) ne 0 then ao=ao0
if n_elements(alnotrbub0) ne 0 then alnotrbub=alnotrbub0
if n_elements(sig10) ne 0 then sig1=sig10
if n_elements(out0) ne 0 then out=out0
if n_elements(pio0) ne 0 then pio=pio0
if n_elements(Bonly0) ne 0 then Bonly=Bonly0
if n_elements(C1Bonly0) ne 0 then C1Bonly=C1Bonly0
if n_elements(C2Bonly0) ne 0 then C2Bonly=C2Bonly0
if n_elements(Isothermal0) ne 0 then Isothermal=Isothermal0
if n_elements(BubbleOnly0) ne 0 then BubbleOnly=BubbleOnly0
if n_elements(aa0) ne 0 then aa=aa0
if n_elements(bb0) ne 0 then bb=bb0
if n_elements(cc0) ne 0 then cc=cc0
if n_elements(dd0) ne 0 then dd=dd0
if n_elements(ee0) ne 0 then ee=ee0
if n_elements(ff0) ne 0 then ff=ff0
if n_elements(alpha0) ne 0 then alpha=alpha0
if n_elements(velmult0) ne 0 then velmult=velmult0
if n_elements(time0) ne 0 then time=time0
if n_elements(saveprams) ne 0 then saveprams=saveprams
if n_elements(readprams0) ne 0 then readprams=readprams0
if n_elements(bubbleonly0) ne 0 then bubbleonly=bubbleonly0
if n_elements(velimpose0) ne 0 then velimpose=velimpose0

if not keyword_set(readprams) then begin

; 
; set up defaults if necessary
; as in Dove et al 2011
;

   default,frontheight,1.35d0
   default,legsang,25.d0
   default,apar,0.05d0
   default,ao,1.75d0
   default,alnotrbub,1
   default,sig1,90.d0
   default,out,.1d0
   default,pio,0.d0
   default,Bonly,0.d0
   default,C1Bonly,0.5d0
   default,C2Bonly,0.9d0
   default,Isothermal,1.55d6
   default,Bubbleonly,0
;
; Schmit & Gibson 2011
;
   default,aa,1.27d0
   default,bb,22.8d0
   default,cc,5.97d0
   default,dd,16.0d0
   default,ee,1.57d0
   default,ff,3.87d0
;   default,aa,0.91d0
;   default,bb,23.6d0
;   default,cc,2.33d0
;   default,dd,16.6d0
;   default,ee,1.96d0
;   default,ff,6.81d0
;   default,aa,8.d0
;   default,bb,24.d0
;   default,cc,4.9d0
;   default,dd,6.d0
;   default,ee,0.07d0
;   default,ff,3.d0
;    default,aa,3.6d0
;    default,bb,15.3d0
;    default,cc,0.99d0
;    default,dd,7.34d0
;    default,ee,0.365d0
;    default,ff,4.31d0
;   default,aa,6.d0
;   default,bb,2.d0
;   default,cc,0.0
;   default,dd,0.d0
;  default,ee,0.d0
;   default,ff,0.d0
   default,alpha,0.d0
   default,velmult,0.1d0
   default,time,0.d0
   default,velimpose,0.d0

endif else begin

; read parameter file (a structure file)

   case 1 of 
      datatype(readprams) eq 'STR': restgen,readarray,file=readprams
     'STC': readarray=readprams
      else: message, 'must provide a named readprams file or a structure'
   endcase

   if tag_exist(readarray,'velimpose') then default,velimpose,readarray.velimpose else default,velimpose,0.d0
   default,frontheight,readarray.FrontHeight
   default,legsang,readarray.LegsAng
   default,apar,readarray.Apar
   default,ao,readarray.Ao
   default,alnotrbub,readarray.AlnotRbub
   default,sig1,readarray.Sig1
;
; backward compatibility
;
   if tag_exist(readarray,'gl_out') then default,out,readarray.GL_Out else $
       default,out,readarray.Out
   default,pio,readarray.Pio
   default,Bonly,readarray.Bonly
   default,C1Bonly,readarray.C1Bonly
   default,C2Bonly,readarray.C2Bonly
   default,Isothermal,readarray.Isothermal
   default,BubbleOnly,readarray.Bubbleonly
   default,aa,readarray.AA
   default,bb,readarray.BB
   default,cc,readarray.CC
   default,dd,readarray.DD
   default,ee,readarray.EE
   default,ff,readarray.FF
   default,alpha,readarray.Alpha
   default,velmult,readarray.VelMult
   default,time,readarray.Time

endelse

;
; only a few choices for eigenvalues
;

; this next is in case the user inputs the smallest eigenvalue explicitly
;
if abs(abs(alnotrbub)-5.763854d0) lt 0.01d0 then alnotrbub=1
alnotrbub=fix(alnotrbub)
if abs(alnotrbub) gt 6 or abs(alnotrbub) lt 1 then begin
    message,/info,'please use eigenvalues >/= 1 and </= 6 (or negative of this); resetting to 1'
    alnotrbub=1
endif

; save parameters to a file (stored in file named saveprams if it is a string)

pramarray={Name:'giblow',FrontHeight:frontheight,LegsAng:legsang,Apar:apar,Ao:ao,AlnotRbub:alnotrbub, $
          Sig1:sig1,GL_Out:out,Pio:pio,Bonly:bonly,C1Bonly:c1bonly,C2Bonly:c2bonly,Isothermal:isothermal,BubbleOnly:BubbleOnly,AA:aa,BB:bb,CC:cc,DD:dd,EE:ee,FF:ff,$
          Alpha:alpha,VelMult:velmult,Time:time,velimpose:velimpose}

if keyword_set(saveprams) then begin
     savefilename=saveprams
     if n_elements(working_dir) eq 1 and datatype(saveprams) eq 'STR' then if working_dir ne '' then savefilename=working_dir+slash+saveprams

     case 1 of
         datatype(saveprams) eq 'STR': savegen,pramarray,file=savefilename,/replace
         else: saveprams=pramarray
     endcase
endif

; now calculate output structure values (the ones the model uses)

mdtor=!dpi/180d0

xo=(frontheight+apar)/(1.+tan(legsang/2.*mdtor))
rbub=frontheight - xo + apar

signalpha=alnotrbub/abs(alnotrbub)
if abs(alnotrbub) eq 1 then alnotrbubuse=5.763459d0*signalpha 
if abs(alnotrbub) eq 2 then alnotrbubuse=9.095011d0*signalpha
if abs(alnotrbub) eq 3 then alnotrbubuse=12.322941d0*signalpha
if abs(alnotrbub) eq 4 then alnotrbubuse=15.514603*signalpha
if abs(alnotrbub) eq 5 then alnotrbubuse=18.689036*signalpha
if abs(alnotrbub) eq 6 then alnotrbubuse=21.853874*signalpha
alnot=alnotrbubuse/rbub
sig1=sig1*mdtor

gfunot = sin(alnot*rbub)/(alnot*rbub) - cos(alnot*rbub)
Muse = 8.d0*!dpi*(rbub*rbub/3.d0/gfunot - 1.d0/alnot/alnot)
ao_use = -ao/Muse

alpha=alpha*velmult^2
eta=1.38d-7*velmult^2
time=time*3600.d0
phiss = sqrt(eta)*time + 1.d0
if alpha gt 0. then begin
     print, 'have only set up the alpha/eta = 0.49 case, will assume that one'
     alpha=6.762d-8*velmult^2
; 
; note this datafile includes the solution to equation 6 of Gibson and Low
;
     restore,'$FORWARD/MODELS/GIBLOW/PARAMETERS/phiss_vs_t_0.490000_1.38000d-07.dat'
     timearr=timearr/velmult
     test = where(abs(timearr-time) eq min(abs(timearr-time)))
     phissuse = phissarr[test]
     phiss = phissuse[0]
endif
if alpha lt 0. then begin
     print, 'have not set up decelerating case'
     stop
endif

outarray={Name:'giblow',Label:'Gibson and Low',Xo:xo,Rbub:rbub,Apar:apar,Ao:ao_use,Alnot:alnot, $
          Sig1:sig1,Out:out,Pio:pio,AA:aa*1d8,BB:bb,CC:cc*1d8,DD:dd,EE:ee*1d8,FF:ff,$
          Alpha:alpha,Eta:eta,Phiss:phiss,Bonly:Bonly,C1Bonly:C1Bonly,C2Bonly:C2Bonly,Isothermal:isothermal,BubbleOnly:BubbleOnly,MagMod:1,velimpose:velimpose}


;print,'xo=',outarray.xo,' rbub=,', outarray.rbub, ' apar=',outarray.apar,' ao=',outarray.ao,' out=',outarray.out,' aa=',outarray.aa,' bb=', outarray.bb,' cc=',outarray.cc,' dd=',outarray.dd,' ee=',outarray.ee,'bubbleonly=',outarray.bubbleonly

end

;**********************************************************************
