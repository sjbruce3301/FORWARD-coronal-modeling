C
C PURPOSE: WRITES EMERGENT STOKES PROFILES TO unformatted FILE
C
C INPUTS:
C
C OUTPUTS:
C
C COMMON: CSLINE, CATOM, CSLINE, CLU
C
C RESTRICTIONS:  
C
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 		COMMENTED CHANGES S GIBSON 
C		MOST RECENT DEC 2016 MINOR BUG IN CENTRAL WAVELENGTH
C 
	SUBROUTINE OUTU
C
	INCLUDE 'PREC'
	INCLUDE 'PARAM'
	INCLUDE 'GRDPAR'
	INCLUDE 'CSLINE'
	INCLUDE 'CATOM'
	INCLUDE 'CINPUT'
	INCLUDE 'CGRID'
	INCLUDE 'CCONST'
	INCLUDE 'CLU'
        common /udove/ ldove
        DIMENSION SUMS(MLINE,0:4)
	DATA ICALL/0/
	SAVE ICALL
C
	QN=QNORM*1.E5/CC
	IF(ICALL .EQ. 0) THEN 
	   KOUNT=0
C 
C changed to compare to CONVL(ALAMB) not ALAMB
C probably not a big difference, but cleaner
C October 2013 SG
C KOUNT counts the number of lines calculated
C in the wavelength range. 
C January 2014 SG
C
	   DO KR=1,NLINE
	      WW=CONVL(ALAMB(KR))
	      IF ((WW .GE. WLMIN) .AND. (WW .LE. WLMAX)) THEN 
		 KOUNT=KOUNT+1
	      ENDIF
	   ENDDO
	   WRITE(Ldove) KOUNT
	   DO KR=1,NLINE
	      WW=CONVL(ALAMB(KR))
	      IF ((WW .GE. WLMIN) .AND. (WW .LE. WLMAX)) THEN 
		 WRITE (Ldove)  KR
		 WRITE (Ldove)  WW
		 WRITE (Ldove) NQ(KR)
C		 WRITE (Ldove) (DLAMB(Q(NY,KR),KR),NY=1,NQ(KR))
		 WRITE (Ldove) (-ALAMB(KR)*Q(NY,KR)*QN,NY=1,NQ(KR))
	      ENDIF
	   END DO
           WRITE(Ldove) iwline
	   ICALL=ICALL+1
	ENDIF
C
C WW IS THE WEIGHT NEEDED TO INTEGRATE OVER WAVELENGTH
C
	DO KR=1,NLINE
C
C as above SG OCT 2013
C
	   WW=CONVL(ALAMB(KR))
	   AMAXINT=0.
           NCHANGE=0
	   IF ((WW .GE. WLMIN) .AND. (WW .LE. WLMAX)) THEN 
	      WW=ALAMB(KR)*QN/(1.0+QN)
	      DO IM=0,4
		 IF(IWLINE .GT. 0) WRITE (Ldove) 
     *              (EMERGE(KR,IM,NY),NY=1,NQ(KR))
		 SUMS(KR,IM)=0.
		 DO NY=1,NQ(KR)
		    SIGN=+1.
		    ADD=EMERGE(KR,IM,NY)
C
C integral over wavelengths of I, Q, U, V, Vmag
C V, Vmag are integrated unsigned
C   note this does not take into account velocity shift
C   assumes that V,Vmag antisymmetric about rest wavelength
C   will deal with in FORWARD
C
		    IF(IM .GE. 3 .AND. Q(NY,KR) .LT. 0.) SIGN=-1.
		    SUMS(KR,IM)=SUMS(KR,IM) + SIGN*ADD*WQ(NY,KR)*WW
C
C added central (maximum) intensity as output
C NOTE  IF MULTI VALUED GAUSSIAN BE CAREFUL (will address within FORWARD)
C will return first peak
C SG JAN 2014
C
		    IF(IM .EQ. 0 .AND. ADD .GE. AMAXINT) THEN 
                     AMAXINT=ADD
	             NCHANGE=NY
                    ENDIF
		 ENDDO
	      END DO
	      IF(IWLINE .GT. 0) WRITE (Ldove) 
     *              (WQ(NY,KR)*WW,NY=1,NQ(KR))
	      WRITE(Ldove) (SUMS(KR,IM),IM=0,4)
	      WRITE (Ldove) 
     *              AMAXINT,NCHANGE
C	      WRITE(*,*) AMAXINT,NCHANGE,EMERGE(KR,0,NQ(KR)/2)
	   ENDIF
	END DO
	RETURN
	END
