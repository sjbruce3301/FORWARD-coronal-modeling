C
C PURPOSE: CALCULATES LINE PROFILE 
C
C INPUTS:
C
C OUTPUTS:
C
C COMMON:
C
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C ---Sept 2022 replaced pre 6jul22 version
C 
      SUBROUTINE PROFIL
C
C  
C
      INCLUDE 'PREC'
      INCLUDE 'PARAM'
      INCLUDE 'CATOM'
      INCLUDE 'CATMOS'
      INCLUDE 'CATMO2'
      INCLUDE 'CSLINE'
      INCLUDE 'CLU'
      INCLUDE 'CINPUT'
      INCLUDE 'CCONST'
C
      SQRTPI=SQRT(PI)
C
C  CALCULATE DOPPLER WIDTH DYND IN UNITS OF QNORM
C
       DNYD=SQRT(2.*BK*TEMP/AWGT)*1.E-5/QNORM
C
C  INCLUDE MICROTURBULENCE
C
      DNYD=SQRT(DNYD*DNYD+VTURB*VTURB)
      DO KR=1,NLINE
        DO NY=1,NQ(KR)
          IF (IND(KR).EQ.2) THEN 
            V=(Q(NY,KR)-VEL)/DNYD
            PHI(KR,NY)=EXP(-V*V)/SQRTPI
            PHIP(KR,NY)=(-2.0*V)*PHI(KR,NY) 
          ENDIF
       END DO
      IF(IDEBUG .NE. 0) WRITE(LOUT,1001) 'PROFIL: PHI(KR,*)',
     *  (PHI(KR,NY),NY=1,NQ(KR))
      END DO
 1001 FORMAT(A/10(1X,E9.2))
      RETURN
      END
