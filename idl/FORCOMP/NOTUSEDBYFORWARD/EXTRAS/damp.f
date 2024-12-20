C
C PURPOSE: CALCULATES DAMPING PARAMETERS, DOPPLER WIDTHS
C     VARIOUS DATA ARE WRITTEN TO COMMON BLOCKS
C INPUTS:
C
C OUTPUTS:
C
C COMMON:
C     CATOM CATMOS CATMO2 CORON CCONST CINPUT
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
      SUBROUTINE DAMP

C
C  CALCULATES DAMPING PARAMETERS
C  INPUT: GA, GW, GS IN  ROUTINE  ATOM WHERE
C  GAMMA = GR + GV + GS AND
C  GR=GA
C  GS=GQ*NE
C  GV=GW*GVW(H)
C
C  GVW(H) = 8.08 * (1+0.41*NHE/HD) * VH**0.6 * HD * C6**0.4
C  VH**0.6 = (8*BK*T/PI*(1/MH+1/AWGT))**0.3
C  C6 = 1.01E-32 * (13.6*ION)**2 *
C       (1/(EV(CONT)-EV(UPPER))**2 - 1/(EV(CONT)-EV(LOWER))**2)
C  REFERENCE MIHALAS 1978: STELLAR ATMOSPHERES, 2ND ED
C  NHE/NH IS SET TO 0.1
C  NB: C6 SHOULD BE DIVIDED BY 2*PI WHEN COMPARED WITH GREY AND UNSOLD
C
C
C  FOR H-ALPHA GS=4.737E-7*HD(1,K), REF. LINEAR-B
C
C:
C: DAMP   88-01-19  MODIFICATIONS: (MATS CARLSSON)
C:        IF GW=0 LEVELS WITHOUT OVERLYING CONTINUUM ARE ALLOWED
C:
C:        88-05-20  MODIFICATIONS: (MATS CARLSSON)
C:        IF GQ.LT.0 IT IS ASSUMED TO BE LG(C4)
C:
      INCLUDE 'PREC'
      INCLUDE 'PARAM'
      INCLUDE 'CATOM'
      INCLUDE 'CATMOS'
      INCLUDE 'CATMO2'
      INCLUDE 'CORON'
      INCLUDE 'CCONST'
      INCLUDE 'CINPUT'
      INCLUDE 'CLU'
      DATA ICALL /0/
      SAVE ICALL
C     
      IF (ICALL .EQ. 0) THEN 
         WRITE(LJOBLO,100)' DAMP: LINE DAMPING PARAMETERS SET TO ZERO'
 100     FORMAT(A)
         ICALL=ICALL+1
      ENDIF
C
      ZER=0.
      GS=ZER
      GV=ZER
      GR=ZER
      TOTHI=ZER
C
C  CALCULATE DOPPLER WIDTH
C
      TWO=2.
      DNYD=SQRT(TWO*BK*TEMP/AWGT)*1.E-5/QNORM
C
C  INCLUDE MICROTURBULENCE
C
      DNYD=SQRT(DNYD*DNYD+VTURB*VTURB)
C
C  CALCULATE GAMMA, ADAMP
C
      DO 330 KR=1,NLINE
         I=IRAD(KR)
         J=JRAD(KR)
C     
C     FIND CONTINUUM LEVEL
C     
         IF(GW(KR).NE. ZER) THEN
            DO 200 IC=J+1,NK
               IF(ION(IC).EQ.ION(J)+1) GOTO 300
 200        CONTINUE
            CALL STOP(' DAMP: NO OVERLYING CONTINUUM')
 300        CONTINUE
            ZZ=ION(I)
            C625=1.283984E-12*ZZ**0.8*(1./(EV(IC)-EV(J))**2-
     *           1./(EV(IC)-EV(I))**2)**.4
            GV=GW(KR)*8.411*(8.*BK*TEMP/PI*(1./(1.008*UU)+
     *           1./AWGT))**0.3*TOTHI*C625
         ELSE
            GV=0.0
         ENDIF
         GR=GA(KR)
         IF(GQ(KR).GE. 0.0) THEN
            GS=GQ(KR)*ED
         ELSE
C     FORMULA FROM GRAY, P.237
            GSLG=19.4+2./3.*GQ(KR)+LOG10(ED*BK)+LOG10(TEMP)/6.
            GS=10.**GSLG
         ENDIF
C     IF(ATOMID.EQ.'H   ' .AND. J.EQ.3 .AND. I.EQ.2)
C     *     GS=4.737E-7*HD(1)
         GAMMA=GR+GV+GS
         DOP=DNYD*QNORM/ALAMB(KR)*1.E13
         ADAMP(KR)=GAMMA/(4.*PI*DOP)
         DDP=DNYD*QNORM*1.E8/CC*ALAMB(KR)
         IF(IWDAMP.GT.0) CALL WDAMP(KR,DDP,GAMMA,GR,GV,GS)
 330  CONTINUE
      RETURN
      END
C
C***********************************************************************
C
