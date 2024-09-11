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
      SUBROUTINE CORONAD(I)
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
      include 'CDOVE'
C
C  SET UP PARAMETERS FOR THE GEOMETRY. ALL GEOMETRY IS SPECIFIED ON
C  INPUT IN GX, GY, GZ WHICH ARE CARTESIAN (X,Y,Z) COORDINATES IN S FRAME
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
C 	(comment rewrite MAR 2014)
C	Calculate parameters to express Stokes vector
C	at point GX,GY,GZ
C	in the S' frame.  If we wanted to have reference
C 	direction along Z
C 	i.e., atomic frame S (local vertical/radial)
C 	we could just change PGAMMA=0
C     
       THETA=0.5*PI+ALPHA
       PGAMMA=BETA
C      PGAMMA=0.d0
C     
C     TRANSFORM TO POLAR COORDINATES FOR B-FIELD CALCULATION
C     
      RB=SQRT(GX2+GY2+GZ2)
      TB=ATAN2(SQRT(GX2+GY2),GZ)
      PB=ATAN2(GY,GX)
C     
C     COMPUTE MAGNETIC PARAMETERS
C     inputs here are from Jim Dove's file.
C     convert his inputs to bs,bt,bp
      Bx= Br3d(i)*sin(theta3D(i))*cos(phi3D(i))+
     *     Bth3d(i)*cos(theta3D(i))*cos(phi3D(i))-
     *     Bph3d(i)*sin(phi3D(i))
C                      
      By= Br3d(i)*sin(theta3D(i))*sin(phi3D(i))+
     * Bth3d(i)*cos(theta3D(i))*sin(phi3D(i))+
     * Bph3d(i)*cos(phi3D(i))
C
      Bz=  Br3d(i)*cos(theta3D(i)) - Bth3d(i)*sin(theta3D(i))
      bs=sqrt(Bx*Bx+By*By+Bz*Bz)
C
C
C watch out for null points
C (sg edit nov 2012)
C
C also see below adjust to thetab, phib
C to make field radial if B is small
C (sg edit aug 2015)

      if(bs.eq.0d0) then
C       print *,'WARNING - NULL PT'
         ts=0.d0
         ps=0.d0
      else 
        ps=atan2(by,bx)
        ts=acos(bz/bs)
      endif
C     end of Dove's input
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
C make field radial if B very small
C (sg edit aug 2015)
C
      if(bs.lt.1d-3)then
        thetab=0.d0
        phib=0.d0
      endif

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
      TEMP=temp3d(i)
      ED=dens3d(i)
      toth=0.8*ed
C     
      VTURB=10./QNORM
C
C changed VTURB to 20 from 0 Dec-20-2013 SG
C  later changed to 10
C
      VEL=VEL3d(i)/QNORM 
C       WRITE(*,*) 'VEL,VTURB,QNORM',VEL,VTURB,QNORM
C
C  NORMALIZE ATOMIC POPULATIONS
C
C       WRITE(*,*)'TEMP',TEMP
C       WRITE(*,*)'ABND',ABND
C       WRITE(*,*)'TOTH',TOTH
C       WRITE(*,*)'EQION',EQION(TEMP)
      TOTN=10.**(ABND-12.)*TOTH*EQION(TEMP)
C       WRITE(*,*),'TEMP,ABND,TOTH,EQION',TEMP,ABND,TOTH,EQION(TEMP)

C HYDROGEN POPULATIONS: ZERO IF NEUTRAL, TOTH FOR PROTONS
      DO II=1,5
         HD(II)=0.
      ENDDO
      HD(6)=TOTH
C
      RETURN
      END

