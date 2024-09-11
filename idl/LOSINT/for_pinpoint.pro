pro for_pinpoint,pB,pol,r3D,tau3D,Density,rpos,distobs,xx_plus,xx_minus,xx_ground,tau_plus,tau_minus,tau_ground,ulimb,$
	dimrad=dimrad,thresdens=thresdens,threspB=threspB,threstaunorm=threstaunorm

;+
; Name: FOR_PINPOINT
;
; Purpose:  determine weighted center of mass along LOS using WL polarization ratio
;
; INPUTS 
;
;	POL == polarization ration (pB/B)
;		dimension POS
;	PB = polarization brightness
;		dimension POS
;	R3D -- radial distance along LOS 
;		dimension LOS X POS
;	TAU3D -- integration angle from TS along LOS
;		dimension LOS X POS
;	DENSITY -- density
;		dimension LOS X POS
;	RPOS -- plane of sky radius (relates to elongation)
;		dimension POS
;	DISTOBS -- distance to observer (defaults to 215 Rsun)
;
;       THRESDENS, THRESPB, THRESTAUNORM being tested-- at the moment are only internal to this program,
;		to be set by uncommenting below
;
; OUTPUTS 
;	XX_PLUS -- X position of solution with mass in front of Thomson sphere
;	XX_MINUS -- X position of solution with mass behind Thomson sphere
; 	XX_GROUND -- X position of ground truth solution - position center of mass along LOS
;	TAU_PLUS -- Tau position along LOS of solution with mass in front of Thomson sphere
;	TAU_MINUS -- Tau position along LOS of solution with mass behind Thomson sphere
; 	TAU_GROUND -- Tau position along LOS of ground truth solution - position center of mass along LOS
;
; Called by: FOR_INTEGRATE
;
; Calls: FOR_BFUN, FOR_CFUN
;
;
; HISTORY:
;       Written by Sarah Gibson 2020
;
;	December 2021-- made DISTOBS a required input
;       January 2022 -- added TPOLF, TPOLB, TPOLG angular distance from TS functionality
;		also changed the way center of mass was calculated, making it 
;		relative to observer not TS
;	July 2024 passed through ulimb
;-

default,dimrad,51
;default,threstaunorm,.01
;default,thresdens,5d7
;default,threspB,1d-6
;  distance to observer is usually 1 AU
;	set in distobs

; elongation angle epslon
epslon = atan(rpos,distobs)


; points of intersection with Thomson sphere
rp2=rpos*cos(epslon)


; Br/Bt = PR
PR=(1-pol)/(1+pol)

; from Billings/Guhathakurta thesis
; Br/Bt = 1. - sin^2(chi)*scatfunc(r)
; chi=asin(sqrt(scatfunc*(1.-PR)))
;
; since we don't know r, we can assume the sun as a point source,
;  scatfunc --> 1, this reduces to
;  chi=asin(sqrt(1-PR))
;  chi=acos(sqrt(PR))
;
;    this is what is in DeForest et al eq 3 and ok for > 2-3 Rsun
;	BUT introduces error below this
;	For lower heights
;	we will introduce another dimension and solve
;	for best fit to radius - knowing that it is at minimum rpos
;	and we will assume it lies within 2.*rpos of the plane of sky
;	*this intrinsically assumes that we can treat the scattering
;	*as a single super-particle at a particular position
;	*but then so does this entire analysis
;

get=size(rpos)
dimrpos=get[1]
radius=dblarr(dimrpos,dimrad)
PR_use=radius*0.
epslon_use=radius*0.

for i = 0,(dimrad-1) do begin
  argument=(rpos[*]^2+((i-(dimrad-1)/2.)*2.*rpos[*]/((dimrad-1)/2.))^2)
  radius[*,i]=sqrt(argument)
  PR_use[*,i]=PR
  epslon_use[*,i]=epslon
endfor

scatfunc=for_bfun(radius,ulimb=ulimb)/for_cfun(radius,ulimb=ulimb)

;  also consider both foreground and background solutions
;  be careful of how scattering angle chi is defined -- 
;  in calculations using chi below I'm assuming 
;  chi_minus is greater than 90, thus 180-chi_plus
;  If one considers the two signs of the sqrt, one gets the two angles defined
;  as +/- each other because sin(-x)=-sin(x)
;  which is fine, but would be treated differently below
 
chi_plus=asin(sqrt(scatfunc*(1.-PR_use)))
chi_minus=!dpi - chi_plus

;
; note -- scatfunc*(1.-PR) may be negative -- this just means
; the superparticle definitely is not located in such points
;   	note- - there may be cases where 2*rpos is not enough
;	and as a result no solution will be found along the entire LOS. 
;	These will be commented on at end of program (bad sim points)
;

; point source

chi_plus_ps=acos(sqrt(PR))
chi_minus_ps= !dpi - chi_plus_ps

; 
; from poster
;  feature in the foreground
;
; pi/2-xi_plus (which is theta) + (pi-chi_plus) (which is chi_plus') + epslon=pi

xi_plus = epslon_use - chi_plus + !dpi/2.d0
xi_plus_ps = epslon - chi_plus_ps + !dpi/2.d0

;
; feature in background
;  (pi/2-xi_minus)(where xi_minus is going to be smaller than xi_plus, or even negative if behind the POS) 
;      + (pi-chi_minus) (which is chi_minus')  + epslon = pi

;xi_minus = -!dpi/2.d0 + chi_minus - epslon_use
;xi_minus_ps = -!dpi/2.d0 + chi_minus_ps - epslon
;  note I had a confusing but at least consistently wrong sign convention that
;  I am replacing with a more correct but same result convention, here and below
xi_minus = epslon_use - chi_minus + !dpi/2.d0
xi_minus_ps = epslon - chi_minus_ps + !dpi/2.d0

;
; distance from Earth to localized density
;

dist_plus=distobs*(sin(!dpi/2.d0 - xi_plus)/sin(!dpi-chi_plus)) 
dist_plus_ps=distobs*(sin(!dpi/2.d0 - xi_plus_ps)/sin(!dpi-chi_plus_ps)) 
;dist_minus=distobs*(sin(!dpi/2.d0 + xi_minus)/sin(!dpi-chi_minus)) 
;dist_minus_ps=distobs*(sin(!dpi/2.d0 + xi_minus_ps)/sin(!dpi-chi_minus_ps)) 
; sign convention adjusted as above
dist_minus=distobs*(sin(!dpi/2.d0 - xi_minus)/sin(!dpi-chi_minus)) 
dist_minus_ps=distobs*(sin(!dpi/2.d0 - xi_minus_ps)/sin(!dpi-chi_minus_ps)) 

;
; these next will be the same
;  they are radial distance to scattering points
;

rad_plus=distobs*(sin(epslon_use)/sin(!dpi-chi_plus)) 
rad_plus_ps=distobs*(sin(epslon)/sin(!dpi-chi_plus_ps)) 
rad_minus=distobs*(sin(epslon_use)/sin(!dpi-chi_minus))
rad_minus_ps=distobs*(sin(epslon)/sin(!dpi-chi_minus_ps))

;
; now collapse on to the correct radii

xi_plus_use=rpos*0.
xi_minus_use=rpos*0.
rad_plus_use=rpos*0.
rad_minus_use=rpos*0.
dist_plus_use=rpos*0.
dist_minus_use=rpos*0.
xx_plus=rpos*0.
xx_minus=rpos*0.
tau_plus=rpos*0.
tau_minus=rpos*0.
xx_plus_nocor=rpos*0.
xx_minus_nocor=rpos*0.
tau_plus_nocor=rpos*0.
tau_minus_nocor=rpos*0.
xx_ground=rpos*0.
tau_ground=rpos*0.

get=size(r3D)
dimlos=get[1]
xx3D=dblarr(dimlos)
dfromTS=dblarr(dimlos)
dist3D=dblarr(dimlos)
xi3D=dblarr(dimlos)

for i = 0,dimrpos-1 do begin
 xi3D[*] = epslon[i] + tau3D[*,i]
 xx3D[*]=r3D[*,i]*cos(!dpi/2.d0 -xi3D)
;
; note, dist3D is now distance from Earth and always positive
;  dfromTS is distance from TS 
;  both are along LOS 
; and XX3D is distance from plane of sky along X axis
;
 dfromTS[*]=r3D[*,i]*sin(tau3D[*,i])
 dearthTS=sqrt(distobs^2-rp2[i]^2)
 dist3D[*]=dearthTS - dfromTS[*]
; for low elongations, parallel LOS, dist3D and XX are the same
 if pol[i]*0. eq 0. and pol[i] ne -8888. then begin
   posrad=where(abs(rad_plus[i,*]-radius[i,*]) eq min(abs(rad_plus[i,*]-radius[i,*])))
;
; there should be two solutions -- symmetric about the POS *do I mean TS?*
; it should not matter which is chosen
; for comparison, will also calculate distance if we assumed point source
;
   xi_plus_use[i]=xi_plus[i,posrad[0]]
   xi_minus_use[i]=xi_minus[i,posrad[0]]
   rad_plus_use[i]=rad_plus[i,posrad[0]]
   rad_minus_use[i]=rad_minus[i,posrad[0]]
   dist_plus_use[i]=dist_plus[i,posrad[0]]
   dist_minus_use[i]=dist_minus[i,posrad[0]]
   xx_plus[i]=rad_plus_use[i]*cos(!dpi/2.d0 -xi_plus_use[i])
   xx_plus_nocor[i]=rad_plus_ps[i]*cos(!dpi/2.d0 -xi_plus_ps[i])
   xx_minus[i]=rad_minus_use[i]*cos(!dpi/2.d0 -xi_minus_use[i])
   xx_minus_nocor[i]=rad_minus_ps[i]*cos(!dpi/2.d0 -xi_minus_ps[i])
;   xx_minus[i]=-rad_minus_use[i]*cos(!dpi/2.d0 -xi_minus_use[i])
;   xx_minus_nocor[i]=-rad_minus_ps[i]*cos(!dpi/2.d0 -xi_minus_ps[i])
;   adjusted sign convention as above
   tau_plus[i]= xi_plus_use[i] - epslon[i]
   tau_minus[i]= xi_minus_use[i] - epslon[i]
   tau_plus_nocor[i]= xi_plus_ps[i] - epslon[i]
   tau_minus_nocor[i]= xi_minus_ps[i] - epslon[i]
;
; for ground truth check, figure out where along the line of sight
; the center of mass is for distributed source 
; relative to intersection with TS
;
; allow exclusion of density below a given threshold
;
   denslos=Density[*,i]
   if keyword_set(thresdens) then begin
    thresground=where(Density[*,i] lt thresdens)
    if min(thresground) ne -1 then denslos[thresground]=0.
   endif
   dist_ground=total(denslos*dist3D)/total(denslos)
   dist_fromTS_ground=dearthTS-dist_ground
   r_ground=sqrt(dist_fromTS_ground^2+rp2[i]*rp2[i])
   tau_ground[i]=atan(dist_fromTS_ground,rp2[i])
   xx_ground[i]=r_ground*sin(epslon[i]+tau_ground[i])
; 
;    if abs(xx_ground[i]-total(denslos*xx3D)/total(denslos)) gt 1d-8 then stop
;  - so this could have been shortened to
;   xx_ground[i]=total(denslos*xx3D)/total(denslos)
;	for background, non parallel lines of sight 
;	means that far field will be intersecting huge heights 
;	along entire LOS and very little (if any) points behind POS
;	and low densities if so, so XPOLG will be positive (and big)
;	which is why TPOLG is probably more useful
;
;   print,'ground truth center of mass distance from plane of sky=',xx_ground[i]
;   print,'test ground truth=',total(denslos*xx3D)/total(denslos)
;   print,'min/max distance from plane of sky - ground truth',minmax(xx3D)
;   print,'polarization position (foreground)=',xx_plus[i]
;   print,'polarization position (foreground) - point source assumption =',xx_plus_nocor[i]
;   print,'polarization position (background)=',xx_minus[i]
;   print,'polarization position (background) - point source assumption =',xx_minus_nocor[i]
 endif else begin
  xx_plus[i]=-8888. 
  xx_minus[i]=-8888. 
  xx_ground[i]=-8888. 
  tau_plus[i]=-8888. 
  tau_minus[i]=-8888. 
  tau_ground[i]=-8888. 
 endelse
endfor

;
; try removing background by putting a threshold on pB
;

if keyword_set(threspB) then begin
 threspol=where(pB lt threspB)
 if min(threspol) ne -1 then begin
  xx_plus[threspol]=-8888.
  xx_minus[threspol]=-8888.
  tau_plus[threspol]=-8888.
  tau_minus[threspol]=-8888.
 endif
endif

if keyword_set(threstaunorm) then begin
 threstau_plus=where(tau_plus lt threstaunorm*max(abs(tau_plus)))
 if min(threstau_plus) ne -1 then begin
  xx_plus[threstau_plus]=-8888.
  tau_plus[threstau_plus]=-8888.
 endif
 threstau_minus=where(tau_minus lt threstaunorm*max(abs(tau_minus)))
 if min(threstau_minus) ne -1 then begin
  xx_minus[threstau_minus]=-8888.
  tau_minus[threstau_minus]=-8888.
 endif
endif
;
; there can be places where we get NaNs because
; we haven't extended radius far enough. For now just
; report on these and make them bad points
test=where(xx_plus*0. ne 0. or xx_minus*0. ne 0.)
if min(test) ne -1 then begin
  print,n_elements(test),' bad sim points'
;  print,xx_plus[test]
;  print,xx_minus[test]
  xx_plus[test]=-8888.
  xx_minus[test]=-8888.
  tau_plus[test]=-8888.
  tau_minus[test]=-8888.
endif
test=where(xx_ground*0. ne 0.)
if min(test) ne -1 then begin
  print,n_elements(test),' bad ground points'
;  print,xx_ground[test]
  xx_ground[test]=-8888.
  tau_ground[test]=-8888.
endif

;
; uncomment
; if want to check point source vs numerically solved
;
;tau_plus=tau_plus_nocor
;tau_minus=tau_minus_nocor
;xx_plus=xx_plus_nocor
;xx_minus=xx_minus_nocor

end
