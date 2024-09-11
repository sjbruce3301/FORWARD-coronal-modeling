pro ff_stepdens

common prams,a_o,r_o,zeta_o,bzex,rhorat
common grid,y,z,r,zeta

;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set up plotting
;

read,'xwin (0) or ps (1)',nplot

if nplot eq 0 then set_plot,'x'

if nplot eq 1 then begin
  set_plot,'ps'
  device,filename='rope.ps'
endif

if nplot eq 0 then begin
  set_plot,'x'
  window,xs=825,ys=750
endif

!p.multi=[0,2,2]
!p.charsize=1.2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; number of steps

nstep=11

a1ar=dblarr(nstep)
rhoratar=dblarr(nstep)
betamaxar=dblarr(nstep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; set up prams
;

g = 2.74e4 
Rsun = 7e10

;
; things that could be changed
;

; ***cavity size  and location***

r_o = .1*Rsun

zo = -.9*r_o
;zo = 0.
; in units of r_o -- zo -1 means right at bottom of circle
; and zo has to be greater than or equal to that

; yo is boundary at that height
yo=sqrt(r_o*r_o - zo*zo)

gridget,zo

; ***external bipolar field*** at z = zo y = yo (gauss)
;

bzex = 10.

;
; ***direction of twist***
; this doesnt affect much except allows
; by/bx to be either sign
;

lambdasign = 1

; ***number density***
; at coronal base outside cavity (z = zo)

rhoex=5d8*1.673e-24

;
; FREE PARAMETER ADIP
;

adip = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; from these we can deduce

zeta_o = 1./2./r_o

; (follows from lowh equation 13)

; FREE PARAMETER A_0

a_o=0.5*rhoex*g*(zo+r_o)^3

; follows from choice of external density at z = zo

; FREE PARAMETER LAMBDA 

getmag,lambda,adip,yo,zo
lambda=lambdasign*lambda

; follows from bzex, ro, and value of adip
; equations 7,17, 21 and 22
; assign direction of twist using lambdasign

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; now step through density depletion

delta= -1./(nstep-1)
rhorat=1.-delta

for i = 0,nstep-1 do begin

; ***density ratio***

  rhorat=rhorat+delta

; depletion at cavity center axis y = 0, z = 0 
; e.g. 20 percent is rhorat of 0.8

; FREE PARAMETER  A1

  getAstuff,lambda,adip,a1,0

; follows from choice of rhorat, and a_o, adip and lambda
; note rhorat is only true at cavity center

  getA,lambda,adip,a1,Ain,Aout

  test=where(r lt r_o)


  a1ar[i]=a1
  rhoratar[i]=rhorat

  AA = Aout
  AA[test] = Ain[test]

  bx=AA*0.
  bx[test]=lambda*sqrt(Ain[test])

  getbybz,lambda,adip,a1,byin,byout,bzin,bzout

  by=byout
  by[test]=byin[test]
  
  bz=bzout
  bz[test]=bzin[test]

  dens=2.*a_o/(g*(z+r_o)^3)
  dens[test]=2.*a_o/(g*(z[test]+r_o)^3) + 2.*a1*Ain[test]/(g*(z[test]+r_o)^3)
  dens=dens/1.673e-24

  depletion = 2.*a_o/(g*(z+r_o)^3) - dens*1.673e-24
  depletion = depletion/(2.*a_o/(g*(z+r_o)^3))

  depletion2 = depletion*0.
  depletion2[test] = a1*Ain[test]/a_o

  pres=a_o/((z+r_o)^2)
  pres[test]=a_o/((z+r_o)^2) + a1*Ain[test]/((z[test]+r_o)^2)

;
; temperature

  kk=1.381e-16
  TT = pres/dens/kk

; beta

  beta = 8.*!pi*pres/(bx*bx+by*by+bz*bz)
  betamax = max(beta[test])
  betamaxar[i]=betamax

; now make 2d plots

  contour,alog10(dens),y/Rsun,z/Rsun,nlevels=10,/follow,title='log(number density)',xtitle=strtrim(string(1.-rhorat),2)+' depletion at axis'
  contour,depletion,y/Rsun,z/Rsun,levels=[0.01,.1,.2,.3,.4,.5,.6,.7,.8,.9],/follow,title='depletion',xtitle=strtrim(string(1.-rhorat),2)+' depletion at axis'
 
; plot_io, z[0,*]/Rsun,dens[0,*],xrange=[0,.2],xs=1,yrange=[1e7,1e9],ys=1
  contour,aa,y/Rsun,z/Rsun,nlevels=10,/follow,title='A',xtitle='adipole/r_o='+strtrim(string(adip/rsun),2)+'lambda='+strtrim(string(lambda),2)
;  contour,depletion2,y/Rsun,z/Rsun,nlevels=3,/follow,title='depletion',xtitle=strtrim(string(1.-rhorat),2)+' depletion at axis'
;  contour,bx,y/Rsun,z/Rsun,nlevels=10,/follow,title='Bx',xtitle='betamax='+strtrim(string(betamaxar[i]),2)
;  contour,by,y/Rsun,z/Rsun,nlevels=10,/follow,title='By, a1='+strtrim(string(a1),2),xtitle='ad='+strtrim(string(adip),2)+', a1='+strtrim(string(a1),2)+', l='+strtrim(string(lambda),2)+'
;  contour,bz,y/Rsun,z/Rsun,nlevels=10,/follow,title='Bz'
;  contour,alog10(pres),y/Rsun,z/Rsun,nlevels=10,/follow,title='log(pres)',xtitle='Temp (10^6 K) min='+strtrim(string(min((TT/1e6))),2)+', max='+strtrim(string(max((TT/1e6))),2)
;  contour,TT/1e6,y/Rsun,z/Rsun,title='Temp (1e6 K)',levels=[.5,1.,5,10,15],c_annotation=['.5','1','5','10','15']
  contour,beta,y/rsun,z/rsun,nlevels=10,/follow,title='beta'

stop
endfor

;
; plots

!p.charsize=1.2
!p.multi=0
plot,rhoratar,betamaxar,xtitle='rhorat',ytitle='max beta', title='lambda='+strtrim(string(lambda),2)+'bext.'+strtrim(string(bzex),2)


if nplot eq 1 then device,/close

end

;*******************************************************************
pro gridget,zo

common prams,a_o,r_o,zeta_o,bzex,rhorat
common grid,y,z,r,zeta

;
; set up grid
;
y = dblarr(100,100)
z = dblarr(100,100)
;
; 
for i = 0,99 do begin
  y[*,i] = dindgen(100)*.03*r_o - 1.5*r_o
  z[i,*] = dindgen(100)*.03*r_o + zo
endfor

r = sqrt(y^2+z^2)
zeta = -(z+r_o)/(y^2+(z+r_o)^2)

end
;*******************************************************************
pro getbybz,lambda,adip,a1,byin,byout,bzin,bzout

common prams,a_o,r_o,zeta_o,bzex,rhorat
common grid,y,z,r,zeta

dasheardrout = -(r_o^2*lambda^2/4.)/r
dasheardrin = -r*lambda^2/4.

dzetadz = zeta/(z+r_o) + 2.*zeta^2
dzetady = zeta^2*2.*y/(z+r_o) 

dadzout=dasheardrout*z/r + adip*dzetadz
dadzin=dasheardrin*z/r + adip*dzetadz + 4.*!pi*a1*(1./zeta + 1./zeta_o)*dzetadz

dadyout=dasheardrout*y/r + adip*dzetady
dadyin=dasheardrin*y/r + adip*dzetady + 4.*!pi*a1*(1./zeta + 1./zeta_o)*dzetady

byin=dadzin
byout=dadzout
bzin=-dadyin
bzout=-dadyout

end
;*******************************************************************
pro getmag,lambda,adip,yo,zo

common prams,a_o,r_o,zeta_o,bzex,rhorat

; external B field evaluated at z = zo, y = yo=sqrt(r_o^2-zo^2)

; bzex= lambda^2*ro/4. - dipterm

dipterm=2.*yo*(zo+r_o)
dipterm=dipterm/(yo*yo + (zo+r_o)^2)
dipterm=dipterm/(yo*yo + (zo+r_o)^2)
dipterm=dipterm*adip

lambda=sqrt((4./r_o)*(bzex+dipterm))

end
;*******************************************************************
pro getAstuff,lambda,adip,a1,zuse

common prams,a_o,r_o,zeta_o,bzex,rhorat
common grid,y,z,r,zeta

; only need to evaluate this at one point, not whole grid
; in other words, taking delta-rho at one point
; flux rope center y = 0, z = 0

ruse= zuse
zetause = -1/(zuse+r_o)

Ashearin = (1./8.)*lambda^2*(r_o^2 - ruse^2)

Apot = adip*(zetause + zeta_o)

AA= 4*!pi*(alog(-zetause/zeta_o)+(zetause+zeta_o)/zeta_o)
BB=Ashearin+Apot
CC = (1-rhorat)*a_o

a1p = (-BB + sqrt(BB^2-4.*AA*CC))/2./AA
a1m = (-BB - sqrt(BB^2-4.*AA*CC))/2./AA

; a1 must be less than zero, if rhorat less than one, and A pos
; I am pretty sure AA is always negative inside the cavity
; BB is definitely positive, and so is CC
; (note adip needs to be negative because zeta is negative)
; a1m is thus always positive, and not the solution
; a1p should work since BB^2-4.*AA*CC should be bigger than BB^2 (AA neg)
; BUT watch out, if zo is small, roundoff errors mess it up 

if a1p gt 0 then print,'Oops, zo too small'
if a1p lt 0 then a1 = a1p

end
;*******************************************************************
pro getA,lambda,adip,a1,Ain,Aout

common prams,a_o,r_o,zeta_o,bzex,rhorat
common grid,y,z,r,zeta

Ashearin = (1./8.)*lambda^2*(r_o^2 - r^2)
Ashearout = -(1./8.)*lambda^2*r_o^2*alog(r^2/r_o^2)

Acorona = 4*!pi*a1*(alog(-zeta/zeta_o)+(zeta+zeta_o)/zeta_o)

Apot = adip*(zeta + zeta_o)

;
; note -- I am not including prominence current sheet
;

Ain = Ashearin + Acorona + Apot
Aout = Ashearout  + Apot

test=where(r lt r_o)

negA = where(Ain[test] lt 0.)
if min(negA) ne -1 then begin
     print,'negative stream function, bad prams'
stop
endif

end
