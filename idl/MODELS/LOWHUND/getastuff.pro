;*******************************************************************
pro getastuff,lambda,adip,a1,xuse,a_o,r_o,zeta_o,rhorat

; only need to evaluate this at one point, not whole grid
; in other words, taking delta-rho at one point
; flux rope center z = 0, x = 0

ruse= xuse
zetause = -1.d0/(xuse+r_o)

Ashearin = (1.d0/8.d0)*lambda^2*(r_o^2 - ruse^2)

;
; note for now adip=0 so this next term is zero
Apot = adip*(zetause + zeta_o)

AA= 4.d0*!dpi*(alog(-zetause/zeta_o)+(zetause+zeta_o)/zeta_o)
BB=Ashearin+Apot
CC = (1.d0-rhorat)*a_o

a1p = (-BB + sqrt(BB^2-4.d0*AA*CC))/2.d0/AA
a1m = (-BB - sqrt(BB^2-4.d0*AA*CC))/2.d0/AA

; a1 must be less than zero, if rhorat less than one, and A pos
; I am pretty sure AA is always negative inside the cavity
; BB is definitely positive, and so is CC
; (note adip needs to be negative because zeta is negative)
; a1m is thus always positive, and not the solution
; a1p should work since BB^2-4.*AA*CC should be bigger than BB^2 (AA neg)
; BUT watch out, if x_o is small, roundoff errors mess it up 

if a1p gt 0 then begin
  print,'Oops, x_o too small'
  a1=0
endif

if a1p lt 0 then a1 = a1p

if a1p eq 0 then a1 = 0

end
