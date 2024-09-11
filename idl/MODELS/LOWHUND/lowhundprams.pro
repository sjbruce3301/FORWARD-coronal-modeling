;************************************************************************
;+
pro lowhundprams,outarray,date=date,working_dir=working_dir,$
                      saveprams=saveprams,readprams=readprams,$
		      r_oinput=r_oinput,x_oinput=x_oinput,bxex=bxex,signlambda=signlambda,$
		      rhoex_input=rhoex_input, rhorat=rhorat,adip=adip,isothermal=isothermal,hydro=hydro,$
		      velimpose=velimpose,lh_out=out,aa=aa,bb=bb,cc=cc,dd=dd,ee=ee,ff=ff


;
;Name: lowhundprams
;
;Purpose: To create structure containing information concerning the
;         Low-Hund flux rope model; To be called by driver routine and resulting
;         structure will be named ModPramsStruct (with ModPramsStruct.name='lowhund')
;
;;Keyword Inputs:
;
;
; WARNING: Following the model convention, X and Z are in the plane of the sky 
;   and Y is out of the page. This is different than FORWARD with X out of the page and 
;   Y-Z in the plane.
;
;
;               R_OINPUT, X_OINPUT -- these are the radius of the flux rope, 
;			and the percent distance from the axis 
;			to the photosphere (at central meridian, Y=0)
;			R_O is input in units of solar radii
;			and X_O is multiplied by this to get distance
;			from axis to photosphere
;			- at X_O=-.999, the flux rope is tangent to photosphere
;			- at X_0=-.001, the flux rope is cut in half
;			  Note that X_O will be negative, with minimum
;			  value required to be greater than -1.
;                       DEFAULT R_O = 0.08, X_O = -0.6
;			OUTPUT changed to CGS 
;			Determine ZO,ZETA_O (see below)
;
;		BXEX -- external bipolar field at base external to rope
;			(this is a magnitude and unsigned -- negative values will
;			if inputted will be converted)
;			DEFAULT 10 Gauss
;
;		SIGNLAMBDA -- direction of twist
; 			this doesnt affect much except allows
; 			bz/by to be either sign
;			DEFAULT positive (1), OUTPUT UNCHANGED
;	
;		RHOEX -- number density
; 			at base outside flux rope
;			DEFAULT 5d8 (number density)
;			OUTPUT changed to cgs
;
;		RHORAT -- density depletion of cavity
;			note -- smaller value means more depleted (darker)
;			DEFAULT 0.5
;
;		ADIP -- external dipole field
;			FREE PARAMETER
;			DEFAULT 0 -- makes problem fully observationally constrained
;			OUTPUT UNCHANGED
;
;		HYDRO - forces hydrostatic density falloff 
;			Because the model was cartesian originally so it will have
;			density falling off relative to the X=0 plane, not the spherical
;			center r=0, which is wrong
;			hydro=0 -- analytic model solution everywhere
;			hydro ne 0 and positive  DEFAULT=1
;			  -- radial hydrostatic solution outside cavity/flux rope
;				and LH analytic model inside
;			hydro ne 0 and negative (like -1)
;			  -- radial hydrostatic solution everywhere, even in the cavity
;			DEFAULT: hydro = 1
;
;		ISOTHERMAL -- forces temperature to be isothermal 
;			note -- this screws up the polytropic condition on the model,
;			which creates a increasing temperature with height;  
;			while this is sort of ok for the low corona where the rope sits, 
;			LOS integration would have temperature increasing too much
;			DEFAULT: ISOTHERMAL=1
;			  the cavity/flux rope will keep the analytic model temperature 
;			   (unless hydro=-1)
;			  but outside the cavity (r>ro) it will be constant
;			  with its value being the model temperature at r=ro at the equator
;			ISOTHERMAL=0
;			  will be consistent with density and temperature
;			  if hydro=0 that means will be LH model solution everywhere
;			ISOTHERMAL=a value (like 1.5e6) will be that value everywhere
;			  even in the cavity
;
;		(LH_)OUT - this will scale the field strength outside - 
;			again a kluge to better compare to other models like Lites Low
;
;  VELIMPOSE - impose a velocity of constant magnitude VELIMPOSE directed along the field
;               overwrites any velocity field already loaded in if nonzero
;                       DEFAULT 0.d0
			
;
;       BOOKKEEPING
;
;               SAVEPRAMS - if keyword set, write parameters to filename
;                       (which is the keyword value saveprams)
;                       or if set to 1, then return outarray
;
;               READPRAMS - if keyword set, read in parameters from filename
;                       (which is the keyword value filename)
;                       or if a structure than read directly
;
;;ModPramsStruct Outputs:
;
;               NAME -- lowhund-- so that procedure can be called in intensint
;               LABEL -- Low-Hund -- for plot label
;
;		X_O, R_O, SIGNLAMBDA,ADIP
;			As above
;
;		ZETAO --- boundary variable zeta
;	
;		AO -- model parameter ...
;			follows from choice of RHOEX
;
;		LAMBDA -- model parameter.. 
;			follows from BXEX, RO, ADIP
;
;		A1 -- model parameter ...
; 		        follows from choice of rhorat, and a_o, adip and lambda
;
;		ISOTHERMAL, HYDRO
;
; changed to procedure and adjusted readprams/saveprams I/O Jan 2013 SEG
; fixed bug where saved parameters were not original inputs Oct 2013 SEG
;	June 2019 used slash for PC compatibility
; Sept 2021 -- forced BXEX always positive and added comments
; Feb 2023 -- updated treatment of hydro/isothermal; also removed adip as
;	parameter that can be changed since it does not seem to be set up
;	correctly (possibly a units issue) and it is unnecessary for solution.

slash=path_sep()

if not keyword_set(readprams) then begin

; possibility of fixing temperature to one value

 default,isothermal,1.d0
 default,hydro,1.d0
 default,out,1.d0

 default,aa,1.27d0
 default,bb,22.8d0
 default,cc,5.97d0
 default,dd,16.d0
 default,ee,1.57d0
 default,ff,3.87d0
; default,aa,8.d0
; default,bb,24.d0
; default,cc,4.9d0
; default,dd,6.d0
; default,ee,0.07d0
; default,ff,3.d0

; ***cavity size  and location***

 default,r_oinput,0.08
 default,x_oinput,-0.6

; ***external bipolar field*** 

 default,bxex,10.
 bxex=abs(bxex)

; ***direction of twist***

 default,signlambda,1

; ***number density***

; adjusted so default will be the same as the hydrostatic external solution

 default,rhoex_input,8.81d8

; ***density depletion in cavity***
;  note if rhoex_input is changed and hydro=not zero, this won't
;  necessarily be the depletion relative to the true exterior

  default,rhorat,0.5

; **FREE PARAMETER ADIP***

 adip=0.0
; default,adip,0.0
; forcing adip=0 for now because not set up right

; velimpose
 default,velimpose,0.d0

endif else begin

; read parameter file (a structure file)

 case 1 of
      datatype(readprams) eq 'STR': restgen,pramarray,file=readprams
      datatype(readprams) eq 'STC': pramarray=readprams
      else: message, 'must provide a named readprams file or a structure'
 endcase

 if tag_exist(pramarray,'velimpose') then default,velimpose,pramarray.velimpose else default,velimpose,0.d0
 default,isothermal,pramarray.isothermal
 default,hydro,pramarray.hydro
 if tag_exist(pramarray,'lh_out') then default,out,pramarray.lh_out else $
	default,out,pramarray.out
 default,aa,pramarray.aa
 default,bb,pramarray.bb
 default,cc,pramarray.cc
 default,dd,pramarray.dd
 default,ee,pramarray.ee
 default,ff,pramarray.ff
 default,r_oinput,pramarray.r_oinput
 default,x_oinput,pramarray.x_oinput
 default,bxex,pramarray.bxex
 default,signlambda,pramarray.signlambda
 default,rhoex_input,pramarray.rhoex_input
 default,rhorat,pramarray.rhorat
; default,adip,pramarray.adip
; forcing adip=0 for now because not set up right
 adip=0
endelse

;
; BOOKKEEPING
;

; save parameters to a file (stored in file named saveprams if it is a string)

   if keyword_set(saveprams) then begin
     savefilename=saveprams
     if n_elements(working_dir) eq 1 and datatype(saveprams) eq 'STR' then if working_dir ne '' then savefilename=working_dir+slash+saveprams

;	     pramarray={Name:'lowhund',R_oinput:r_oinput,X_oinput:x_oinput,$
;			Bxex:bxex,Signlambda:signlambda,Rhoex_input:rhoex_input,Rhorat:rhorat,Adip:adip,Isothermal:isothermal,Hydro:hydro,LH_out:out,AA:aa,BB:bb,CC:cc,DD:dd,EE:ee,FF:ff,velimpose:velimpose}
;
; removing Adip for now
	     pramarray={Name:'lowhund',R_oinput:r_oinput,X_oinput:x_oinput,$
			Bxex:bxex,Signlambda:signlambda,Rhoex_input:rhoex_input,Rhorat:rhorat,Isothermal:isothermal,Hydro:hydro,LH_out:out,AA:aa,BB:bb,CC:cc,DD:dd,EE:ee,FF:ff,velimpose:velimpose}
	;
	     case 1 of
		 datatype(saveprams) eq 'STR': savegen,pramarray,file=savefilename,/replace
		 else: saveprams=pramarray
	     endcase
	   endif
	g = 2.74d4 
	Mp=1.673d-24
	Rsun = 6.9570d10

	r_o=r_oinput*Rsun
	x_o=x_oinput*r_o

	zo=sqrt(r_o*r_o - x_o*x_o)

	zeta_o = 1./2./r_o

	rhoex=rhoex_input*Mp

	; ***model parameter a_o***

	 a_o=0.5*rhoex*g*(x_o+r_o)^3

	; **model parameter lambda**

	; equations  11,17, 21 and 22
	; assign direction of twist using signlambda

	 getmag,lambda,adip,zo,x_o,r_o,bxex
	 lambda=signlambda*lambda

	; **model parameter a1**
	; follows from choice of rhorat, and a_o, adip and lambda
	; note rhorat is only true at cavity center

	 getastuff,lambda,adip,a1,0.,a_o,r_o,zeta_o,rhorat

;
;	 outarray={Name:'lowhund',Label:'Low-Hund',X_o:x_o,R_o:r_o,Signlambda:signlambda,$
;		Adip:adip,Zeta_o:zeta_o,A_o:a_o,Lambda:lambda,A1:a1,Isothermal:isothermal,Hydro:hydro,out:out,AA:aa*1d8,BB:bb,CC:cc*1d8,DD:dd,EE:ee*1d8,FF:ff,MagMod:1,VelImpose:VelImpose}
;
; taking out Adip for now since it is not set up right
;
	 outarray={Name:'lowhund',Label:'Low-Hund',X_o:x_o,R_o:r_o,Signlambda:signlambda,$
		Zeta_o:zeta_o,A_o:a_o,Lambda:lambda,A1:a1,Isothermal:isothermal,Hydro:hydro,out:out,AA:aa*1d8,BB:bb,CC:cc*1d8,DD:dd,EE:ee*1d8,FF:ff,MagMod:1,VelImpose:VelImpose}


end

;**********************************************************************
