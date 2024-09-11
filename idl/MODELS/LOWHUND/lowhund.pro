
        pro lowhund,rpB,thetapB,phipB,ModPramsStruct,ModSolStruct

;
; Name: LOWHUND
;
; Purpose: Calculate the density, temperature, magnetic field of the
;          Low-Hund model at a location rpB, thetapB, phipB (true spherical
;          coordinates - need to be converted to model Cartesian)
;
; Inputs:
;          rpB, thetapB, phipB -- position in 3D space where model is to be evaluated
;				rpB in units of RSUN, thetapB, phipB in units of RADIANS
;
;          ModPramsStruct - structure associated with model, containing
;                           model name (LOWHUND), model parameters
;                           set up in giblowprams.pro
;
; Outputs: ModSolStruct - Solution of model, containing density,
;                         pressure, magnetic field (Br, Bth, Bph)
;
; Calls 
;
;  Written: S. Gibson
;
; Version 2.0 July 2014
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
; 	Feb 2023 -- updated hydro isothermal treatment; also
;		removed adip as parameter to be varied as it 
;		is not set up right and isnt necessary

; Adapted from ff_stepdens in original version of code

; Read in Parameters:

	x_o=ModPramsStruct.X_o
	r_o=ModPramsStruct.R_o
	lambdasign=ModPramsStruct.Signlambda
;	adip=ModPramsStruct.Adip
; forcing 0 for now because not set up right
	adip=0
	zeta_o=ModPramsStruct.Zeta_o
	a_o=ModPramsStruct.A_o
	lambda=ModPramsStruct.Lambda
	a1=ModPramsStruct.A1
	isothermal=ModPramsStruct.Isothermal
	hydro=ModPramsStruct.Hydro
	out=ModPramsStruct.Out

        aa1 = ModPramsStruct.AA
        bb = ModPramsStruct.BB
        cc = ModPramsStruct.CC
        dd = ModPramsStruct.DD
        ee = ModPramsStruct.EE
        ff = ModPramsStruct.FF

; Convert coordinates to model Cartesian
  
; NOTE: we are making the rope independent in the phi
; direction rather than the y direction, because it makes
; a more realistic LOS integration.  However, one must be
; sure to use dimensions of the rope (e.g. 0.025 Rsun for radius
; as default) that make the curvature small, because otherwise,
; this introduces a curl of magnetic field that is unbalanced

	Rsun = 6.9570d10

        x = Rsun*(rpB*sin(thetapB)) + x_o - Rsun
        z = Rsun*(rpB*cos(thetapB))

	r = sqrt(x*x+z*z)
	zeta = -(x+r_o)/(z^2+(x+r_o)^2)

    	geta,r,zeta,lambda,adip,a1,Ain,Aout,zeta_o,r_o

  	testin=where(r lt r_o)

  	AA = Aout
  	by=AA*0.
 	getbzbx,z,x,r,zeta,lambda,adip,a1,bzin,bzout,bxin,bxout,zeta_o,r_o
  	bz=bzout
  	bx=bxout

	g = 2.74d4
	Mp = 1.673d-24
  	dens=2.*a_o/(g*(x+r_o)^3)
  	pres=a_o/((x+r_o)^2)
	testunder=where(x lt 0.)
	if min(testunder) ne -1 then dens[testunder]=2.*a_o/(g*(r_o)^3)
  	if min(testunder) ne -1 then pres[testunder]=a_o/((r_o)^2)

	if min(testin) ne -1 then begin
  	  AA[testin] = Ain[testin]
  	  by[testin]=lambda*sqrt(Ain[testin])
  	  bz[testin]=bzin[testin]
  	  bx[testin]=bxin[testin]
  	  dens[testin]=2.*a_o/(g*(x[testin]+r_o)^3) + 2.*a1*Ain[testin]/(g*(x[testin]+r_o)^3)
  	  pres[testin]=a_o/((x[testin]+r_o)^2) + a1*Ain[testin]/((x[testin]+r_o)^2)
	endif

; convert mass density to number density
;  rho --> N
  	dens=dens/Mp

	st=sin(thetapB)
	ct=cos(thetapB)
	sp=sin(phipB)
	cp=cos(phipB)

; NOTE we should recheck these to see if they are the
;     best kluge to use

 	br= bx*st + bz*ct
	bth= bx*ct - bz*st
	bph = by

; these are the true cartesian to spherical coords transform
;
;	br=by*st*sp + bx*st*cp + bz*ct
;	bth=by*ct*sp + bx*ct*cp - bz*st
;	bph = -bx*sp + by*cp

;
; at this point the density is as in LH analytic solution
;  which can cause problems, espcially outside the rope
;	because it's for a plane parallel atmosphere
;	wrapped onto a spherical coordinate system
; 
  	testout=where(r ge r_o,c)

; default hydro will be +1
;  this will create a radial hydrostatic dropoff for both
;  density and pressure, fit to coronal values

	if hydro ne 0 then begin
;
; number density
	  dens_hydro=(aa1*rpB^(-bb)+cc*rpB^(-dd)+ee*rpB^(-ff))
	  pres_hydro=-1*Mp*g*Rsun*((aa1/(-bb+1))*rpB^(-bb+1)+(cc/(-dd+1))*rpB^(-dd+1)$
		+(ee/(-ff+1))*rpB^(-ff+1))
	  if hydro gt 0 then begin
;
; everywhere outside cavity gets radial hydrostatic solution
;
           dens[testout]=dens_hydro[testout]
           pres[testout]=pres_hydro[testout]
          endif else begin
;
; everywhere including cavity (no longer a cavity) gets radial hydrostatic solution
           dens=dens_hydro 
           pres=pres_hydro
          endelse
 	endif
	
;
; temperature

 	kk=1.381d-16
	temp=pres/dens/kk

; 
; we have the option of forcing a single constant temperature if we want
;  otherwise it will be consistent with density and pressure and change with height
;	which is the default (ISOTHERMAL=0)
;
	if isothermal ne 0 then begin
;
; this changed temperature only above the cavity, not everywhere outside it
;   (also I think there was a bug in the 1.5 instead of 1. in front of g*r_o*Mp
;
;	  maxtest = where(rpB gt 1.+2.*r_o/Rsun,d)
;	  if d ne 0 then temp[maxtest] = 1.5*g*r_o*Mp/kk
;
	  if c ne 0 then temp[testout] = g*r_o*Mp/kk
	  if isothermal ne 1 then temp[*]=isothermal
	endif
; we have the option to have a multiplier on the external field 

	if c ne 0 then begin
         br[testout]=out*br[testout]
	 bth[testout]=out*bth[testout]
	 bph[testout]=out*bph[testout]
        endif

        Vel=Pres*0.d0 + ModPramsStruct.VelImpose

; now output variables requested

        ModSolStruct={Pres:Pres,Dens:Dens,Temp:temp,Br:br,Bth:bth,Bph:bph,Vel:Vel}

end

