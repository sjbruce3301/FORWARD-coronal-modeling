pro for_uv,wl,u0,v0
;
;  this function calculates allen u and v coefficients for
;  limb darkening:, see allen (aq 1973, section 81)
;
; purpose: calculates limb darkening coefficients
;
; inputs:
;       wl wavelength  ** in microns **
; outputs:
;       u,v coefficients u2 and v2 of allen (1973, p171)
; common:
;
; restrictions:
;       outside of wavelength range the extremal values are used.
; 
; CALLED BY FOR_FIELD_INT
;	
reflam=[.2,.22,.245,.265,.28,.30,.32,.35,.37,.38, $
        .40,.45,.5,.55,.6,.8,1.,1.5,2.,3.,5.,10.]
u=[.12,-1.3,-.1,-.1,.38,.74,.88,.98,1.03,.92,$
   .91,.99,.97,.93,.88,.73,.64,.57,.48,.35,.22,.15]
v=[.33,1.6,.85,.9,.57,.2,.03,-.1,-.16,-.05,$
   -.05,-.17,-.22,-.23,-.23,-.22,-.2,-.21,-.18,-.12,-.07,-.07]
;     
;  use LINTERP as it uses extremal values when outside range
;

linterp,reflam,u,wl,u0
linterp,reflam,v,wl,v0
return
end
