C
C PURPOSE: ADMINISTERS CALLING FOR SYNTHESIS OF M1 LINES ALONG ONE LOS
C
C INPUTS: IY, IZ: INDICES OF POINTS IN X AND Y GRID
C
C OUTPUTS:
C
C COMMON:
C
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 		edit NOV 5, 2012, Sarah Gibson
C 
	SUBROUTINE M1SYNTHD
C
C  SUBROUTINE FOR THE SYNTHESIS OF FORBIDDEN LINE STOKES 
C  PARAMETERS, ALONG A GIVEN LOS. 
C
	INCLUDE 'PREC'
	INCLUDE 'PARAM'
	INCLUDE 'CGRID'
	INCLUDE 'CATOM'
	INCLUDE 'CATMOS'
	INCLUDE 'CATMO2'  
	INCLUDE 'CSLINE'
	INCLUDE 'CORON'
	INCLUDE 'CCONST'
	INCLUDE 'CINPUT'
	INCLUDE 'CINTS'
	INCLUDE 'CLU'
        include 'CDOVE'
     	DIMENSION AA(MJTOT,MJTOT),BB(MJTOT)
     	DIMENSION T0(0:3,0:2)
	LOGICAL DISK,IGNORE
C
	DO KR=1,NLINE
	   DO M=0,4
	      DO NY=1,NQ(KR)
		 EMERGE(KR,M,NY)=ZERO
	      END DO
	   END DO
	END DO
C
C  LOOP OVER THE LOS COORDINATE
C
	DO I=1,NGX
	   DO KR=1,NLINE
	      DO M=0,4
		 DO NY=1,NQ(KR)
		    EMISS(KR,M,NY)=ZERO
		 END DO
	      END DO
	   END DO
	   GX=r3d(i)*sin(theta3d(i))*cos(phi3d(i))
	   GY= r3d(i)*sin(theta3d(i))*sin(phi3d(i))
	   GZ= r3d(i)*cos(theta3d(i))
C
C  IS THE LOS ON THE DISK?
C
	   DISK=.FALSE.
	   RRR=GZ*GZ+GY*GY
	   IF(RRR .LE. ONE*1.001) DISK= .TRUE.
C
C  note - this test is brought into the loop
C  in this version, and GY,GZ calculated explicitly,
C  this should not be necessary because they should not
C  change for a given LOS
C  also, DISK should never be TRUE because
C  we never pass points under the disk
C   TEST THIS
C  sg dec 2013
C 
	   if(i .eq. 1) then
              gxstep = (r3d(i+1)*sin(theta3d(i+1))*cos(phi3d(i+1))-gx)
	   else if(i .eq. ngx) then 
              gxstep = (r3d(i-1)*sin(theta3d(i-1))*cos(phi3d(i-1))-gx)
	   else
	      gxstep = (r3d(i+1)*sin(theta3d(i+1))*cos(phi3d(i+1))
     *               -  r3d(i-1)*sin(theta3d(i-1))*cos(phi3d(i-1)))
	   endif
c
	   gxstep=abs(gxstep)
	   WTX=0.5*RSUNCM*GXSTEP
c
C       
C       CALCULATE SOLAR PARAMETERS (HEIGHT, DENSITY, TEMPERATURE, 
C  MICROTURBULENCE, MAGNETIC FIELD IN CORONA)
C       
	   ii=i
	   CALL CORONAD(ii)
C       
C       IGNORE THOSE LOCATIONS WHERE LINE OF SIGHT IS EITHER
C       1. ON THE DISK
C       2. HAS ELECTRON DENSITY BELOW SMALL N
C       3. HAS SMALL ION FRACTIONS 
C       
c	GOT RID OF IGNORE FOR SMALLN
c	PUSH LIMIT FOR SOME MODELS
c	-SG
c	   if(i .eq. 100 .and. disk) write(*,*)'disk'
c	   IGNORE= DISK
c	   IGNORE= DISK .OR.
c     *      (TOTN .LT. 10.**(ABND-12.)*SMALLN)
c  PUT IT BACK IN DEC 2013 SG
	   IGNORE= DISK .OR. (ED .LT. SMALLN) .OR. 
     *      (TOTN .LT. 10.**(ABND-12.)*SMALLN)
c           IGNORE=.FALSE.
c           write(*,*),'DISK',DISK
c           write(*,*),'ED,SMALLN',ED,SMALLN
c           write(*,*),'TOTN',TOTN
c           write(*,*),10.**(ABND-12.)*SMALLN
	   CALL WATMOS(I,IY,IZ,IGNORE)
	   IF(.NOT. IGNORE) THEN 
	      CALL PROFIL
C       
C       LTE POPULATIONS AND COLLISIONAL RATES TO BE STORED
C       
	      IF(ICOLL .NE. 0) THEN 
		 CALL LTEPOP
		 CALL COLCAL
	      ENDIF
C       
C       BUILD AND SOLVE STATISTICAL EQUILIBRIUM EQUATIONS
C       
	      CALL SE0_BUILD(NDIM)
	      CALL SOLVE(NDIM,AA,BB)
	      IF(IDEBUG.EQ.1) WRITE(LOUT,100) 'M1SYNTHD: TEMP, ED',TEMP,ED
 100	      FORMAT(A,1P,2(1X,E9.2))
	      CALL SE0_COPY(NDIM,BB)
C       
C       SOLVE FOR EMERGENT STOKES PROFILES
C       
	      CALL T0TENS(T0)
C       
	      DO KR=1,NLINE
		 IF ((ALAMB(KR).GE.WLMIN).AND.(ALAMB(KR).LE.WLMAX))
     *               CALL EMISSION(KR,T0)
	      END DO
	      CALL TRAP(WTX)
	      CALL WDEBUG
	   ENDIF
	END DO
	RETURN
	END

