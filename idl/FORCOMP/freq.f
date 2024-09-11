C
C PURPOSE: CALCULATES THE FREQUENCY QUADRATURE
C
C INPUTS:
C
C OUTPUTS:
C
C COMMON:
C     CATOM CSLINE CCONST
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
      SUBROUTINE FREQ
C
C  CALCULATES THE FREQUENCY QUADRATURE
C
      INCLUDE 'PREC'
      INCLUDE 'PARAM'
      INCLUDE 'CATOM'
      INCLUDE 'CSLINE'
      INCLUDE 'CCONST'
C
C  FREQUENCY QUADRATURE IN LINES
C
      DO 200 KR=1,NLINE
        CALL FREQL(KR)
        IF(IWIDE(KR)) THEN
          KT=KTRANS(KR)
          FRQ(0,KT)=CC/ALAMB(KR)*1.E8
          DO 100 NY=1,NQ(KR)
            FRQ(NY,KT)=FRQ(0,KT)*(1.0+Q(NY,KR)*QNORM/CC*1.E5)
  100     CONTINUE
        ENDIF
  200 CONTINUE
C
C  FREQUENCY QUADRATURE IN CONTINUA
C
      DO 300 KR=NLINE+1,NRAD
        CALL FREQC(KR)
  300 CONTINUE
C
      RETURN
      END
C
C************************************************************************
C
