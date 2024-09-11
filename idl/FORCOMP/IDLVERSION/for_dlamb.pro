pro for_dlamb,qq,kr

;C PURPOSE: CONVERT FROM DELTA LAMBDA IN Q UNITS TO DELTA LAMBDA IN ANGSTROM
;C
;C INPUTS:
;C     QQ DELTA LAMBDA IN TYPICAL DOPPLER WIDTH UNITS
;C     KR RADIATIVE TRANSITION INDEX
;C
;C OUTPUTS:
;C     DLAMB
;
; CALLS FOR_ALAMB
; CALLED BY FORCOMP
;
;C COMMON:
;C     CATOM CCONST
;C COMMENTS: OCTOBER 6, 1999, P. JUDGE
;C 

@include.icle
@include.icle.input

common c, param,catom,const,input

      QN=input.QNORM*1.E5/CC
      DLAMB=-FOR_ALAMB(KR)*QQ*QN/(1.0+QQ*QN)
      RETURN
      END
C
C************************************************************************
C
