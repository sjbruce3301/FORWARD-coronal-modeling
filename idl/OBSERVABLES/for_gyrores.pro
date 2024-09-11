;
; Programs written/modified from Fortran by Stephen White
;
; Version 2.0 July 2014
;---------------------------------------------------------------------------

pro for_gyrores,f=f,s=s,n=n,temp=temp,theta=theta,l=l,taux=taux,tauo=tauo

;
; Called by FOR_RADIOMODEL (below)
;

if not keyword_set(f) then begin
print,' '
print,'Usage: gyrores,f=freq(Hz),n=n_e(cm-3),temp=temp(K),l=b_scale(cm),'
print,'            s=s,theta=theta(deg), taux=taux, tauo=tauo'
print,' '
print,'       Returns tau in x and o at harmonic s<=8.'
print,' '
goto,last
endif

; use Robinson's quasi-exact expressions rather than Zlotnik's:
; the former also work at perpendicular propagation
; calculate the coefficient

   s = long(s)
   if ((s lt 1) or (s gt 8)) then begin
      print,'Harmonic s must be in range 1-8. Returning defaults.'
      taux=0.0 & tauo=0.0
      return
   endif
   fact=[1.,1.,2.,6.,24.,120.,720.,5040.,8*5040.,72.*5040.,720*5040.]
   coeff=!dpi*!dpi/2.*8.064d+7*n*l/f/3.0d+10
   if not keyword_set(theta) then theta=45.0
   fp=8980.d0*sqrt(n)
   theta=theta*!dpi/180.0
   sthet2=sin(theta)^2
   cthet2=cos(theta)^2
   arg=(sin(theta)^2)*1.38d-16*temp/2./9.11d-28/9.0d+20
   b=f/2.80d+6/s
   for_coldpl,'X',theta,f,f/s,fp,temp,nind,tee,qa,cfact,vgz
   factor=coeff*s*s/fact[s]*((s*s*arg)^(s-1))
   if (s eq 1) then taux=0.0 else $
       taux=factor*((1.+tee*cos(theta))^2+(temp/5.9413d9*tee)^2)/(1.+tee*tee)
   for_coldpl,'O',theta,f,f/s,fp,temp,nind,tee,qa,cfact,vgz
   tauo=factor*((1.+tee*cos(theta))^2+(temp/5.9413d9*tee)^2)/(1.+tee*tee)

last:

end

;---------------------------------------------------------------------------
