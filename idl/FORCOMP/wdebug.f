C
C PURPOSE: WRITES EMISSION COEFFICIENTS TO FILE
C
C INPUTS:
C
C WDEBUGS:
C
C COMMON: CSE, CSLINE, CATOM, CSLINE, CATOM, CORON, CINPUT, CLU
C
C RESTRICTIONS:  NONE
C
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
	SUBROUTINE WDEBUG
C
	INCLUDE 'PREC'
	INCLUDE 'PARAM'
	INCLUDE 'CSE'
	INCLUDE 'CSLINE'
	INCLUDE 'CATOM'
	INCLUDE 'CATMOS'
	INCLUDE 'CORON'
	INCLUDE 'CGRID'
	INCLUDE 'CINPUT'
	INCLUDE 'CLU'
C added 6jul22
	INCLUDE 'CINTS'
C
	IF(IDEBUG .EQ. 0) RETURN
	WRITE(LOUT,*)'GX,GY,GZ',GX,GY,GZ
        WRITE(LOUT,*)'THETAB,PHIB',THETAB,PHIB
C Note this next line moved up in 6jul22 version
	WRITE(LOUT,1004)' ELECTRON DENSITY=',ED, ' TOTAL ION POP =',TOTN
	DO KR=1,NLINE
	   IF ((ALAMB(KR) .GE. WLMIN) .AND. (ALAMB(KR) .LE. WLMAX)) THEN
	      WRITE (LOUT,*) 'LINE ', KR, ALAMB(KR)
	      WRITE (LOUT,1000) 'FREQUENCIES ',NQ(KR)
	      WRITE (LOUT,1001) (Q(NY,KR),NY=1,NQ(KR))
	      WRITE (LOUT,*) 'EMISSION COEFFICIENTS '
	      DO IM=0,3
		 WRITE (LOUT,*) 'I = ',IM
		 WRITE (LOUT,1002) (EMISS(KR,IM,NY),NY=1,NQ(KR))
	      END DO
	   ENDIF
	END DO
	WRITE(LOUT,*)'LEVEL LABEL      / POPULATIONS'
	TSUM=0.
	DO IJ=1,NK
C Note these next two lines updated in 6jul22 version
	   WRITE(LOUT,1003)IJ,LABEL(IJ),TOTN*RHO(IJ,0)*SQRT(TWO*AJ(IJ)+ONE) 
	   TSUM=TSUM+TOTN*RHO(IJ,0)*SQRT(TWO*AJ(IJ)+ONE) 
	ENDDO
C Note this next line updated in 6jul22 version
	WRITE(LOUT,1003)-1,'1 -SUMMED POP/ION POP',1.-TSUM/TOTN
 1004	format(A,1p,e9.2,a,e9.2,0p)
 1003	format(i4,1x,a25,1x,1p,e9.2,0p)
C
 1000	FORMAT(1X,A,1X,I4)
 1001	FORMAT(10(4X,F7.3))
 1002	FORMAT(4(1P,10(1X,E10.3),0P/))
	RETURN
	END
