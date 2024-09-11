;*******************************************************************
pro geta,r,zeta,lambda,adip,a1,Ain,Aout,zeta_o,r_o

;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)

;

; 
; equation 17

Ashearin = (1./8.)*lambda^2*(r_o^2 - r^2)
Ashearout = -(1./8.)*lambda^2*r_o^2*alog(r^2/r_o^2)

; equation 20 also see appendix text before A6

Acorona = 4.d0*!dpi*a1*(alog(-zeta/zeta_o)+(zeta+zeta_o)/zeta_o)

;
; Adipole in equation 22
;  (note for now adip forced to zero)

Apot = adip*(zeta + zeta_o)

;
; warning -- adip hasnt been tested -- there
; may be units issues (rsun or rsun*rsun)
; and possibly should be forced to be negative

;
; inside/outside using Equation 16 of LH
;  Acorona is zero in outside
;  and ignoring Aprom
;

Ain = Ashearin + Acorona + Apot
Aout = Ashearout  + Apot

testin=where(r lt r_o)

if min(testin) ne -1 then begin
  negA = where(Ain[testin] lt 0.)
  if min(negA) ne -1 then begin
          print,'negative stream function, bad prams'
	  stop
  endif
endif

end
