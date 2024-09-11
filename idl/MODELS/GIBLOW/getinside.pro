	pro getinside,rin,phissin

; 
; CALLED BY GIBLOW
;
; CALLS ZERO2TINY
;
	common gltranscoord,rlam
	common glprams,apar,xo,rbub,ao,Pio,alpha,eta
	common glprams2,aa,bb,cc,dd,ee,ff,alnot,sig1,out
	common glinblock,Pbackin,Dbackin

;  Feb 2018  Added tests for small nubmers (SEG)
;	also replaced sin(thout) with st, cos(thout) with mu
;  March 2018 -- Added call to zero2tiny
;  June 2019 -- added capability for phiss variation
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)


;
;  this program determines values of density, pressure, and magnetic pressure
;  along the bubble boundary -- thus, for a given value of rin (physical, pre-ss radial coord)
;  quantities will be calculated using the OUTSIDE solution that need to be added to the INSIDE
;  gas pressure and density so that total pressure is constant across the bubble boundary
;  NOTE, inside solution stream function (and so Pgas and Pmag) is zero at surface
;   also, outside solution is potential so density and pressure will be
;   in radial  HEbalance

        r1 = xo 
 
;   just so I dont have to type a lot I am calling the radius and theta
;   coors rout and thout
;
        rout = rin
;
; calculte theta along the boundary for each r, and rotate so that the
; bubble is at the equator
;
        rat = ((r1*r1 + rout*rout - rbub*rbub)/(2*rout*r1))
        testpos = where(rat ge 1.)
        testneg = where(rat lt -1.)
        if min(testpos) ne -1 then rat[testpos] = 1.
        if min(testneg) ne -1 then rat[testneg] = -1.
        thout = acos(rat)

;
;    break down bits of stream function
;
        st = sin(thout)
        mu = cos(thout)
;
; problems occur for near-zero theta if the number
;  gets in the noise
;
	zero2tiny,st
	zero2tiny,mu

        al1 = r1*r1 - rbub*rbub
        al2 = 2*r1*r1 - rbub*rbub
;
;  eq. B2

        psi0 = mu
;
;  eq. B3

        f = r1*(rout*rout + al1) - rout*mu*al2
        g = al1*al1 + r1*r1*rout*rout - 2*rout*r1*al1*mu
        h = 1.d0/(sqrt(g))
        psi1 = (-1.d0/rbub)*f*h
;
;  eq. B4

        i = rout*rout + r1*r1 - 2.d0*rout*r1*mu
        psi3 = (1/rbub)*sqrt(i)
;
;  eq. B5

        Streamout = psi0 + psi1 + psi3
;
;  now take derivatives for magnetic fields, etc.
;
        dpsi0dr = 0.d0
        dpsi0dth = -st
        dpsi0dr2 = 0.d0
        dpsi0drdth = 0.d0
        dpsi0dth2 = -1.d0*mu
;
        dfdr = 2.d0*r1*rout - mu*al2
        dfdmu = -rout*al2
        dfdrdmu = -al2
        dfdr2 = 2.d0*r1
        dfdmu2 = 0.d0
;
        dgdr = 2.d0*rout*r1*r1 - 2.d0*r1*al1*mu
        dgdmu = -2.d0*rout*r1*al1
        dgdrdmu = -2.d0*r1*al1
        dgdr2 = 2.d0*r1*r1
        dgdmu2 = 0.d0
;
        dhdr = -dgdr/2.d0/(g^1.5)
        dhdmu = -dgdmu/2.d0/(g^1.5)
        dhdrdmu = -dgdrdmu/2.d0/(g^1.5) + 3.d0*dgdmu*dgdr/4.d0/(g^2.5d0)
        dhdr2 = -dgdr2/2.d0/(g^1.5d0) + 3.d0*dgdr*dgdr/4.d0/(g^2.5d0)
        dhdmu2 = 1.5d0*(dgdmu^2)/2.d0/(g^2.5d0)
;
        dpsi1dr = (-1.d0/rbub)*(dfdr*h + f*dhdr)
        dpsi1dr2 = (-1.d0/rbub)*(2.d0*dfdr*dhdr + dfdr2*h + f*dhdr2)
;
        dpsi1dmu = (-1.d0/rbub)*(dfdmu*h + f*dhdmu)
        dpsi1drdmu = (-1.d0/rbub)*(dfdmu*dhdr + dfdr*dhdmu + dfdrdmu*h + f*dhdrdmu)
        dpsi1dmu2 = (-1.d0/rbub)*(dfdmu2*h + 2.d0*dfdmu*dhdmu + f*dhdmu2)
;
        dpsi1dth = -st*dpsi1dmu
        dpsi1drdth = -st*dpsi1drdmu
        dpsi1dth2 = -mu*dpsi1dmu +(st)^2*dpsi1dmu2
;
        didr = 2.d0*rout - 2.d0*r1*mu
        didmu = -2.d0*rout*r1
        didrdmu = -2.d0*r1
        didr2 = 2.d0
        didmu2 = 0.d0
;
        dpsi3dr = (1/rbub)*didr/2/sqrt(i)
        dpsi3dmu = (1/rbub)*didmu/2/sqrt(i)
        dpsi3drdmu = (1/rbub)*(didrdmu/2/sqrt(i) - didmu*didr/4/(i^1.5))
        dpsi3dr2 = (1/rbub)*(didr2/2/sqrt(i) - (didr)^2/4/(i^1.5))
        dpsi3dmu2 = (1/rbub)*didmu2/2/sqrt(i) -(1/rbub)*(didmu^2)/4/(i^1.5)
;
        dpsi3dth = -st*dpsi3dmu
        dpsi3drdth = -st*dpsi3drdmu
        dpsi3dth2 = -mu*dpsi3dmu + (st)^2*dpsi3dmu2
;
        dAdr = dpsi0dr + dpsi1dr + dpsi3dr
        dAdth = dpsi0dth + dpsi1dth + dpsi3dth
        dAdrdth = dpsi0drdth + dpsi1drdth + dpsi3drdth
        dAdr2 = dpsi0dr2 + dpsi1dr2 + dpsi3dr2
        dAdth2 = dpsi0dth2 + dpsi1dth2 + dpsi3dth2
;
;  now calculate field
;
;  eq. B1

        brlambout =  dAdth/rout/rout/st
        bthlambout =  -dAdr/rout/st
        bphlambout = 0.
;
;  now calculate field derivatives
;
        dbrdr = (-2.d0*dAdth/rout+ dAdrdth)/rout/rout/st
        dbthdr = (dAdr/rout - dAdr2)/rout/st
;
        dbrdth = -mu*dAdth/rout/rout/(st)^2
        dbrdth = dbrdth + dAdth2/rout/rout/st
        dbthdth = mu*dAdr/rout/(st)^2
        dbthdth = dbthdth - dAdrdth/rout/st
;
        rnolam = rout - apar
        Presin= (rout^2/rnolam^2)*(1-(rout^2/rnolam^2))*(brlambout^2)/8.d0/!dpi
        Presin= Presin/(phissin^4)
;
;
;  here are the magnetic pressure in the rpB,thetapB
;  phipB coordinate systems !  we have to also get rid of selfsim stuff
;
;  dlamdr is one, because rlam = rsquig + apar.
;
        dlamdr = 1
;
        Bpresin = ((brlambout*(rout^2/rnolam^2)/(phissin^2))^2 + (bthlambout*(rout/rnolam)*dlamdr/(phissin^2))^2)/8.d0/!dpi
;
        Pbackin = Presin + Bpresin
        Pbackin = Pbackin*out*out
;
;  now calculate the background density
;
        dldr = -apar/rnolam/rnolam
        lr = (apar+rnolam)/rnolam
;
        ufunc = (r1*r1 + rout*rout - rbub*rbub)/(2*rout*r1)
        dufdr = (1/r1) - ufunc/rout
 
        dthdr = -(1/sqrt(1-ufunc*ufunc))*dufdr
 
        dstuffdr = -(lr*dldr*brlambout*brlambout + lr*lr*brlambout*dbrdr + .5*dldr*bthlambout*bthlambout + lr*bthlambout*dbthdr)/4.d0/!dpi
;
;
        dstuffdth= -(lr*lr*brlambout*dbrdth + lr*bthlambout*dbthdth)/4.d0/!dpi
;
        dpdr = dstuffdr + dstuffdth*dthdr
 
        Rsun = 6.9570d10
        GMm = 221.3d0
	mprot = 1.674d-24
        F = GMm/((rout-apar)^2)/(Rsun^2) + alpha*(rout-apar)*Rsun*mprot
;
        Dbackin = dpdr/F/Rsun/phissin/phissin/phissin
        Dbackin = Dbackin*out*out
;
        end

