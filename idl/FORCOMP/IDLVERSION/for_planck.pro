function for_planck,y,t
;
; purpose: calculates planck function bny at angular frequency u, temp t
;  (note- u  and planck are in angular frequency units)
;
;  note in this version Y IS SCALAR and T CAN BE SCALAR OR VECTOR
;
; CALLED BY FOR_FIELD_INT
;
@include.icle
;
u=y*0.5d0/const.pi
x=const.hh*u/const.bk/t
planck=x*0.d0
;
big= where(x lt 84.d0,nb)
if(nb gt 0) then    planck[big]=2.0d0*const.hh*u/const.cc*u/const.cc*u/(exp(x[big])-1.0d0)
;
small= where(x ge 84.d0,ns)
if(ns gt 0) then planck[small]=2.0d0*const.hh*u/const.cc*u/const.cc*u*exp(-x[small])
planck=planck*0.5d0/const.pi
;
return,planck
end
