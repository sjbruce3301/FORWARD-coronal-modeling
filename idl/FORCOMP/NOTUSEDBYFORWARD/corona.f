C
C PURPOSE: GET CORONAL THERMAL AND MAGNETIC DATA FOR EACH GRID POINT
C
C INPUTS: NONE
C
C OUTPUTS:
C
C COMMON:
C     CATOM,CGRID,CATMOS,CATMO2,CCONST,CORON
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C
C  FOUR DIFFERENT REFERENCE FRAMES ARE NEEDED (SEE NOTES):
C
C	1. FRAME WITH Z-AXIS AS THE LOCAL VERTICAL AT THE OBSERVED 
C	   POINT (S)
C
C	2. FRAME WITH Z-AXIS AS THE MEDIAN OF THE LOS DISTRIBUTION,
C	   AND ORIENTED AS THE REFERENCE DIR. OF POLARIZATION (S')
C
C	3. FRAME WITH Z-AXIS AS THE SUN ROTATION AXIS (S_R)
C
C	4. FRAME WITH Z-AXIS AS THE LOCAL VERTICAL THROUGH THE
C	   REFERENCE POINT M OF THE MAGNETIC-SOURCE DISTRIBUTION (S_M)
C
      SUBROUTINE CORONA
      INCLUDE 'PREC'
      INCLUDE 'PARAM'
      INCLUDE 'CGRID'
      INCLUDE 'CATOM'
      INCLUDE 'CATMOS'
      INCLUDE 'CATMO2'
      INCLUDE 'CCONST'
      INCLUDE 'CORON'
      INCLUDE 'CLU'
      INCLUDE 'CINPUT'
      CHARACTER CORNAM*6
C      CALL CPTIME('CORONA',0,0,1)
C
C  SET UP PARAMETERS FOR THE GEOMETRY. ALL GEOMETRY IS SPECIFIED ON
C  INPUT IN GX, GY, GY WHICH ARE CARTESIAN (X,Y,Z) COORDINATES IN S FRAME
C
      GX2=GX*GX
      GY2=GY*GY
      GZ2=GZ*GZ
C     
      ALPHA=-ATAN2(GX,SQRT(GY2+GZ2))
C     
      SALPHA=SIN(ALPHA)
      CALPHA=COS(ALPHA)
C     
      BETA=ATAN2(GY,GZ)
C     
      SBETA=SIN(BETA)
      CBETA=COS(BETA)
C     
C     OBSERVER'S GEOMETRY IN THE "S-FRAME" (PHI=0), 
C     WITH REFERENCE DIRECTION ALWAYS PARALLEL TO Z
C     
      THETA=0.5*PI+ALPHA
      PGAMMA=BETA
C     
C     TRANSFORM TO POLAR COORDINATES FOR B-FIELD CALCULATION
C     
      RB=SQRT(GX2+GY2+GZ2)
      TB=ATAN2(SQRT(GX2+GY2),GZ)
      PB=ATAN2(GY,GX)
C     
C     COMPUTE MAGNETIC PARAMETERS
C     
C >>> USER SUPPLIED FIELD/PLASMA ROUTINES GO HERE <<<
C
      CORNAM=CRTN
      IPRINT=0
      IF(CORNAM(1:6) .EQ. 'DIPOLE') THEN 
         IPRINT=1
         CALL DIPOLE(RB,TB,PB,BS,TS,PS,PNE,TOTH,PT,PVEL,PTURBV,IPRINT)
      ELSE IF(CORNAM(1:3) .EQ. 'FFL') THEN 
         IPRINT=1
         CALL FFL(RB,TB,PB,BS,TS,PS,PNE,TOTH,PT,PVEL,PTURBV,IPRINT)
      ELSE IF(CORNAM(1:2) .EQ. 'ZL') THEN 
         IPRINT=1
         CALL ZL(RB,TB,PB,BS,TS,PS,PNE,TOTH,PT,PVEL,PTURBV,IPRINT)
      ELSE IF(CORNAM(1:5) .EQ. 'CUSER') THEN 
         IPRINT=1
         CALL CUSER(RB,TB,PB,BS,TS,PS,PNE,TOTH,PT,PVEL,PTURBV,IPRINT)
      ELSE 
         WRITE(LJOBLO,1001)CORNAM
 1001    FORMAT(' CORONA: SUBROUTINE  ',A,' IS NOT KNOWN')
         CALL STOP('CORONA: NO CORONAL DATA ROUTINE')
      ENDIF
C
C
      BFIELD=BS
      BTHETA=TS
      BPHI=PS
C     
      XB=SIN(BTHETA)*COS(BPHI)
      YB=SIN(BTHETA)*SIN(BPHI)
      ZB=COS(BTHETA)
C     
      YZB=SBETA*YB+CBETA*ZB
C     
      XXB=CALPHA*XB+SALPHA*YZB
      YYB=CBETA*YB-SBETA*ZB
      ZZB=-SALPHA*XB+CALPHA*YZB
C     
C     COMPUTE POLAR ANGLES OF B IN THE "S'-FRAME"
C     (VAN VLECK ANGLE = 54.7356103173)
C     
      THETAB=ATAN2(SQRT(XXB*XXB+YYB*YYB),ZZB)	
      PHIB=ATAN2(YYB,XXB)
C     
C     HEIGHT ABOVE LIMB
C     
      H=RB-1.0
C     
C     LARMOR FREQUENCY 
C     
      ALARMOR=(1.0E-8*(0.25E0/PI)*(EE/EM)*BFIELD)/QNORM
C     
C     THERMAL PARAMETERS
C     
      TEMP=PT
      ED=PNE
C     
      VTURB=PTURBV/QNORM
      VEL=PVEL/QNORM
C
C  NORMALIZE ATOMIC POPULATIONS
C
      TOTN=10.**(ABND-12.)*TOTH*EQION(TEMP)
C HYDROGEN POPULATIONS: ZERO IF NEUTRAL, TOTH FOR PROTONS
      DO I=1,5
         HD(I)=0.
      ENDDO
      HD(6)=TOTH
C
C      CALL CPTIME('CORONA',0,1,1)
      RETURN
      END

