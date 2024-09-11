C
C PURPOSE: GIVES LINE FREQUENCY QUADRATURE POINTS AND WEIGHTS.
C
C INPUTS:
C     KR  TRANSITION INDEX
C OUTPUTS:
C
C COMMON:
C     CSLINE
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
      SUBROUTINE FREQL(KR)
C
C  GIVES  FREQUENCY QUADRATURE POINTS AND CORRESPONDING WEIGHTS.
C  ONE- OR TWO-SIDED CASES.  THE POINTS AND WEIGHTS ARE BASED ON
C  THE TRAPEZOIDAL RULE APPLIED TO A MAPPING FUNCTION  THAT MAPS
C  EQUIDISTANT  POINTS IN  X TO  EQUIDISTANT POINTS IN  Q  IN THE
C  CORE (Q .LT. Q0), AND EQUIDISTANT POINTS IN LOG(Q) IN THE WINGS
C  (Q .GT. Q0).
C
C  NQ     : TOTAL NUMBER OF QUADRATURE POINTS                (IN)
C  QMAX   : MAXIMUM VALUE OF FREQUENCY (DOPPLER UNITS)       (IN)
C  IND    : =1 FOR ONE-SIDED AND =2 FOR TWO-SIDED CASES      (IN)
C  Q0     : TRANSITION FROM CORE TO WING (DOPPLER UNITS)     (IN)
C  Q      : QUADRATURE POINTS                               (OUT)
C  WQ     : QUADRATURE WEIGHTS                              (OUT)
C
C  CODED BY: AKE NORDLUND (OCT-1981).
C
C  REVISED BY MATS CARLSSON (JAN-1984): NEW OPTIONS
C  QMAX=Q0   Q LINEAR IN FREQUENCY
C  QMAX OR Q0.LT.0  Q GIVEN AND ONLY WEIGHTS ARE CALCULATED
C
C:
C: FREQL  88-05-03  MODIFICATIONS: (MATS CARLSSON)
C:        ADDED OPTIONS GAVE INCORRECT WEIGHTS FOR IND=1
C:        THIS AFFECTED COOLING RATES FOR LINES WITH FREQUENCY
C:        QUADRATURE GIVEN WITH QMAX=Q0 OR QMAX.LT.0 OR Q0.LT.0
C:        THE CORRECTED WEIGHTS ARE TWICE THE OLD ONES
C:
      INCLUDE 'PREC'
      INCLUDE 'PARAM'
      INCLUDE 'CSLINE'
C
      IF(QMAX(KR).EQ.Q0(KR)) THEN
        DQ=IND(KR)*QMAX(KR)/(NQ(KR)-1)
        Q(1,KR)=-QMAX(KR)*(IND(KR)-1)
        DO 50 NY=2,NQ(KR)
          Q(NY,KR)=Q(NY-1,KR)+DQ
          WQ(NY,KR)=DQ*2./IND(KR)
   50   CONTINUE
        WQ(1,KR)=0.5*DQ*2./IND(KR)
        WQ(NQ(KR),KR)=0.5*DQ*2./IND(KR)
      ELSE IF(QMAX(KR).GE.0.0 .AND. Q0(KR).GE.0.0) THEN
C
C CONSTANTS
C
        AL10=LOG(10.)
        A=10.**(Q0(KR)+.5)
        HALF=0.5
        XMAX=LOG10(A*MAX(HALF,QMAX(KR)-Q0(KR)-HALF))
C
C ONE-SIDED CASE
C
        IF (IND(KR).EQ.1) THEN
          DX=XMAX/(NQ(KR)-1)
          DO 100 J=1,NQ(KR)
            X=(J-1)*DX
            X10=10.**X
            Q(J,KR)=X+(X10-1.)/A
            WQ(J,KR)=2.*DX*(1.+X10*AL10/A)
  100     CONTINUE
          WQ(1,KR)=WQ(1,KR)*0.5
C
C TWO-SIDED CASE
C
        ELSE
          DX=2.*XMAX/(NQ(KR)-1)
          DO 110 J=1,NQ(KR)
            X=-XMAX+(J-1)*DX
            X10=10.**X
            Q(J,KR)=X+(X10-1./X10)/A
            WQ(J,KR)=DX*(1.+(X10+1./X10)*AL10/A)
  110     CONTINUE
        END IF
      ELSE
        DO 200 NY=2,NQ(KR)-1
          WQ(NY,KR)=0.5*(Q(NY+1,KR)-Q(NY-1,KR))
  200   CONTINUE
        WQ(1,KR)=0.5*(Q(2,KR)-Q(1,KR))
        WQ(NQ(KR),KR)=0.5*(Q(NQ(KR),KR)-Q(NQ(KR)-1,KR))
C
C  FOR ONE-SIDED CASE, MULTIPLY WEIGHTS BY 2.0
C
        IF(Q(1,KR).GE.0.0) THEN
          DO 300 NY=1,NQ(KR)
            WQ(NY,KR)=WQ(NY,KR)*2.0
  300     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
C
C***********************************************************************
C
