;
;	cram_sim.pro
;
;	does integration to determine intensity of K-corona

function cram_sim,lamdaobs,rho,te,LOS_int_absvalue

;lamdaobs = 8000		;observed wavelength 
;rho = 1.5			;observed radial distance (limb = 1)
;te = 1				;electron temperature

; 	****  Constant Definition  ****

on_error,3
e = 2.718282
c = 300000			;speed of light (km/s)
solradii = 6.9599 * 10.^10.	;solar radius (cm)
au = 1.495979 * 10.^13.		;earth-sun distance (cm)
irradtoflux = (au/solradii)^2/!pi	;irradiance to flux conversion	
sunang = 6.8*10.^(-5)		;steradians covered by sun
cmpersr = !pi*solradii^2/sunang		;centimeters per steradian

crossec = 0.6677 * 10.^(-24.)	;Thomson scattering crossection (cm^2)
factoredout = 1/2./sqrt(!pi)*3/16/!pi*crossec	;constants factored out of
						;integration
qp = 5508. * sqrt(te)		;mean electron thermal velocity
				; = sqrt(2kT/m)

intr = 0. & intt = 0.
intro = 0. & intto = 0.
intre = 0. & intte = 0.
inttot = 0. & introt = 0. 
xdep = fltarr(3,41)

;	****  Set Wavelength Scale  ****


restore,'/Users/sbruce/Documents/GitHub/FORWARD-coronal-modeling/irradiance_solar.sav' ;load irradiance tester file (generated from python script)
irtot = irtotnl
lamdasrc = reform(irtot(0,*))	;wavelength scale of atlas - take out only wavelength values into 1D array
izero = reform(irtot(1,*)) * irradtoflux	;convert to solar flux - take out irrad values into 1D array and multiply by conversion factor.

;trim wavelength range down to +-4.0  velocity half-widths from observation wavelength
ldif = lamdaobs*qp/c
ldif = ldif * 4.0
lamdamin = lamdaobs - ldif	
lamdamax = lamdaobs + ldif
lambeg = get_closest(lamdasrc,lamdamin)
lamend = get_closest(lamdasrc,lamdamax)
lamdasrcc = lamdasrc(lambeg:lamend)
izeroc = izero(lambeg:lamend)

;find mean size of wavelength step
nalm = n_elements(lamdasrc)
delamda = -total(lamdasrc(0:nalm-2)-lamdasrc(1:nalm-1))/(nalm-1)

dellam = lamdaobs-lamdasrcc	;dif b/w source and emitted wavelength
delt = lamdasrcc/c*qp

;	****  define limb darkening coefficients  ****
;	****  from Allen, 3rd ed, par. 81  ****

lam = [30,32,35,37,38,40,45,50,55,60,80,100,150]*100

u2 = [.74,.88,.98,1.03,.92,.91,.99,.97,.93,.88,.73,.64,.57]
v2 = [.20,.03,-.1,-.16,-.05,-.05,-.17,-.22,-.23,-.23,-.22,-.20,-.21]
u2r = spline(lam,u2,lamdasrc,1)
v2r = spline(lam,v2,lamdasrc,1)
u2rc = u2r(lambeg:lamend)
v2rc = v2r(lambeg:lamend)
foi = 1-u2rc/3.-v2rc/2.

;alam = [-.4,-.02,.25,.42,.26,.2,.54,.68,.74,.78,.92,.97]
;blam=[1.2,.97,.79,.68,.78,.81,.60,.49,.43,.39,.25,.18]
;clam=[.5,.1,-.3,-.4,-.2,-.1,-.44,-.56,-.56,-.57,-.56,-.53]
;foi = al2+.667*bl2+.409*cl2
;qlam = 0.862 - (lamdasrc-3700)/1300.*0.212	;limb darkening coefficient

;	****  define some variables in integration  ****

minx = -LOS_int_absvalue		;min distance along line of sight , normal -7 to 7
maxx = LOS_int_absvalue		;max distance along line of sight
xstep = 0.5			;step size along line of sight
maxr = fix(sqrt(maxx^2 + rho^2))
xs = findgen(maxr*20+1.)/20. + 1.003
allhgt = [1.003,1.005,1.01,1.03,1.06,1.1,1.2,1.4,1.6,1.8,2.0,2.2,2.5,3.0,4.0,5.0,10.0]
allne = 10^[9.0,8.7,8.4,8.25,8.10,7.96,7.67,7.18,6.83,6.56,6.31,6.10,5.81,5.45,4.97,4.7,4.0]
;	minimum-equator region model
;allne = 10^[9.0,8.8,8.6,8.45,8.36,8.23,7.90,7.44,7.05,6.78,6.52,6.28,6.00,5.65,5.18,4.9,4.1]
;	maximum-equator region model
;allne = 10^[9.0,8.6,8.3,8.12,7.98,7.81,7.30,6.64,6.13,5.78,5.5,5.25,5.0,4.7,4.3,4.0,3.4]
;	polar-region model
alled = spline(allhgt,allne,xs,9)

xstepcm = xstep*solradii	;number of cm along x-step (cm)
xstepnum = (maxx-minx)/xstep + 1

nomegsteps = 50			;number of omega steps
minomega  = 0.			;minimum omega value to consider (rad)

maxphi = !pi			;maximum phi value to consider (rad)
phistep = .08			;step size of phi variable (rad)
phistepnum = (maxphi/phistep) +1

intre = 0. & intte = 0.		;reset x-integration variable

;*************  Integration along Line of Sight (x) *******************
for x = minx,maxx,xstep do begin
;print,x
;print,minx,maxx

r = sqrt(x^2 + rho^2)			;distance from center of sun
;elecden = 1.67 * 10^(4+4.04/r)		;electron density at x (cm^-3)
dnum = get_closest(xs,r)
elecden = alled(dnum)
totalelecs = elecden*xstepcm		;total number of scattering electrons

maxomega = asin(1/r)			;maximum omega = edge of Sun
omegstep = (maxomega-minomega)/nomegsteps	;size of omega steps
if omegstep eq 0 then omegstep = 1	

coschi = x/r
chi = acos(coschi)			;chi = suncenter-electron-earth angle
sinchi = sin(chi)

intro = 0. & intto = 0.			;reset omega integration variable
omegold = asin(1./r)

    ;*********  Integration over Omega (omeg) ************
    for omeg = minomega,maxomega,omegstep do begin

    sinomeg = sin(omeg)
    cosomeg = cos(omeg)
    theta = asin(r*sinomeg)           ;outward normal and incident beam angle
    mu = cos(theta)
    ;    limb-darkening modification factor - wavelength dependent 
;   iodc = al2 + bl2*mu + cl2*(1-mu*alog(1+1/mu))
    iodc = 1 - u2rc - v2rc + u2rc*mu + v2rc*mu^2
    qfac = iodc/foi

    izerocor = izeroc*qfac		;flux corrected for limb darkening

    intr = 0. & intt = 0.		;reset phi integration variable

        ;*********  Integration over Phi (phi) ************

        for phi = 0.,maxphi,phistep do begin

        sinphi = sin(phi)
        scatang = !pi - acos(cosomeg*coschi + sinomeg*sinchi*sinphi)
		;scattering angle of electron; solar source-electron-earth
	alphasin = (sinphi*sinomeg/sin(!pi-scatang))<1.00
        alpha = asin(alphasin)
		;angle b/w scattering plane and sun center-electron-earth plane
        gamma = scatang/2
		;direction of n(1), one axis of velocity coordinate system

        qrad = (cos(alpha)^2*cos(scatang)^2 + sin(alpha)^2)
		;amount of radially polarized light scattered toward earth
        qtan = (cos(alpha)^2 + sin(alpha)^2*cos(scatang)^2)
		;amount of tangentially polarized light scattered toward earth

            ;*********  Integration over Wavelength (lamdasrc) ************
  
            b = cos(gamma)
            expon = -((dellam/2/delt/b)^2)	
            expon = expon>(-80.)
            dist = (e^expon)/delt/b

            intcont = dist*izerocor	;scattered intensity contribution func.

            intint = total(intcont)*delamda	;total scattered intensity

        intr = intr + qrad*intint		;radially polarized intensity
        intt = intt + qtan*intint		;tangentially pol. intensity
        

        ;********  End of phi (phi) integration  *********
        endfor
   
    intro = intro + intr*sinomeg
    intto = intto + intt*sinomeg

    ;********  End of omega (omeg) integration  *********
    endfor

intre = intre + intro*totalelecs*omegstep
intte = intte + intto*totalelecs*omegstep
xdep(0:1,x*2+20) = [intto*omegstep,intro*omegstep]
xdep(2,x*2+20) = totalelecs

;********  End of line-of-sight (x) integration  *********
endfor

intot = intte*phistep*2*factoredout
intor = intre*phistep*2*factoredout
polp = (intot-intor)/(intot+intor)
xdep(0:1,*) = xdep(0:1,*)*phistep*2*factoredout
;print,'Ir'+string(intot)+'     It'+string(intor)
;print,'Polarization Percentage - '+string(polp)

return,intot;,intor,polp
;return,[intot,intor], xdep
;print,'xdep:'+string(xdep)
end
