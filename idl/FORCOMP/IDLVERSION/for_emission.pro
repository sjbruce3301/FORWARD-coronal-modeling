pro  for_emission, kr,t0
;c
;c purpose: calculate emission coefficients for stokes vector 
;c
;c inputs:
;c       kr   transition index (1=first transition, etc)
;c       t0   geometric tensor (table 1 of
;c            casini, r. & judge, p. g., 1999. ap j 522, 524-539)
;c outputs:
;c   emiss(kr,i,ny)  emission coefficient in ph/cm2/s/sr/angstrom 
;c   where  kr is the line index
;c          i is the stokes index (0=intensity, 1=q, etc.)
;c          nu is the frequency index
;c
; CALLED BY FOR_M1SYNTH
; CALLS FOR_FUN6J, FOR_FUN9J
;
;c common:
;c       cse csline catom coron sgnm clu
;
@include.icle
@include.icle.input
@include.icle.atom
@include.icle.cse
@include.icle.corona
@include.icle.emerge
;
sqrt2=.14142135623730950488d1
sqrt3=.17320508075688772935d1
;
;t0=dblarr(4,3)
em_fact=dblarr(5)
;
ij0=trn[kr].irad
ij1=trn[kr].jrad
rj0=cse.aj(ij0)
rj1=cse.aj[ij1]
;
; calculate c_coeff
;
hnu=const.hh*const.cc / (trn[kr].alamb/1.d8)
c_coeff=totn*sqrt(2.d0*rj1+1.d0)*cse.rho[ij1,0]*trn[kr].a *hnu/4.d0/const.pi
;print,kr,trn[kr].alamb,cse.rho[ij1,0]
;print,kr,ij1,c_coeff,totn,rj1,cse.rho[ij1,0],trn[kr].a
g0=lvl[ij0].glande
g1=lvl[ij1].glande
geff=0.5*(g0+g1)+0.25*(g0-g1)*(rj0*(rj0+1.d0)-rj1*(rj1+1.d0))
sigma=cse.rho[ij1,2]/cse.rho[ij1,0]
;print,cse.rho[ij1,2],cse.rho[ij1,0]
;
;   calculate dcoeff and ecoeff
;
sg=const.sg
d_coeff=-sg(round(rj0+rj1))*sqrt(3.d0*(2.d0*rj1+1.d0)) $
        *for_fun6j(1.d0,1.d0,2.d0,rj1,rj1,rj0)
;
e_coeff=3.d0*sqrt(2.d0*rj1+1.d0) $
        *(sg(nint(rj1-rj0))*g1*sqrt(rj1*(rj1+1.d0)*(2.d0*rj1+1.d0)) $
          *for_fun6j(1.d0,2.d0,1.d0,rj1,rj1,rj1) $
          *for_fun6j(1.d0,1.d0,1.d0,rj1,rj1,rj0) $
          -g0*sqrt(rj0*(rj0+1.d0)*(2.d0*rj0+1.d0)) $
          *for_fun9j(1.d0,2.d0,1.d0,rj0,rj1,1.d0,rj0,rj1,1.d0))
;
;   calculate emission coefficients
;
em_fact(0)=1.d0+d_coeff*sigma*t0(0,2)
em_fact(1)=d_coeff*sigma*t0(1,2)
em_fact(2)=d_coeff*sigma*t0(2,2)
em_fact(3)=-(sqrt2/sqrt3)*alarmor*(geff+e_coeff*sigma)*t0(3,1)
;
; emiss(*,4,*): magnetogram stokes v
;
em_fact(4)=-(sqrt2/sqrt3)*alarmor*geff*t0(3,1)
iny=indgen(trn[kr].nq)
for istks=0,2 do emerge.emiss(kr,istks,iny)=em_fact(istks)*trn[kr].phi(iny)*c_coeff

scl=c_coeff*1.e-13*trn[kr].alamb

for istks=3,4 do emerge.emiss(kr,istks,iny)=-1.*em_fact(istks)*trn[kr].phip(iny)*scl

if(input.idebug ne 0) then print,'kr,alarmor,geff,sigma,c_coeff',kr,alarmor,geff,sigma,c_coeff

;print,emerge.emiss(0,0,0),emerge.emiss(0,1,0),emerge.emiss(0,2,0),emerge.emiss(0,3,0),emerge.emiss(0,4,0)
;print,em_fact(0),em_fact(1),em_fact(2),em_fact(3),em_fact(4)
;print,d_coeff,sigma,t0(0,2),t0(1,2),t0(2,2),t0(3,1),alarmor,geff,e_coeff
;print,sigma

return
end
