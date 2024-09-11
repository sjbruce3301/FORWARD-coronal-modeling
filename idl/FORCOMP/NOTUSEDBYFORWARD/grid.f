C
C PURPOSE: GET GRID OF X Y Z 
C
C INPUTS:
C     NONE
C OUTPUTS:
C       
C COMMON:  GRID
C    
C COMMENTS: AUGUST 24, 2000, P. JUDGE
C
C 
	SUBROUTINE GRID
	INCLUDE 'PREC'
        INCLUDE 'GRDPAR'
        INCLUDE 'CGRID'
	INCLUDE 'CINPUT'
        INCLUDE 'CLU'
C
        CALL CSTRIP(LGRID,'GRID.DAT')
        READ(LDUMS,*)GXMIN,GXMAX,GYMIN,GYMAX,GZMIN,GZMAX,NGX
C     
        DELTA=(GXMAX-GXMIN)/FLOAT(NGX-1)
        NGY= INT((GYMAX-GYMIN)/DELTA) +1
        NGZ= INT((GZMAX-GZMIN)/DELTA) +1
        IF(NGX .GT. MX) THEN 
           WRITE(LJOBLO,1000) ' GRID: NGX = ',NGX,' MX = ',MX
           CALL STOP('GRID: NGX .GT. MX') 
        ENDIF
C
        IF(NGY .GT. MY) THEN 
           WRITE(LJOBLO,1000) ' GRID: NGY = ',NGY,' MY = ',MY
           CALL STOP('GRID: NGY .GT. MY') 
        ENDIF
C
        IF(NGZ .GT. MZ) THEN 
           WRITE(LJOBLO,1000) ' GRID: NGZ = ',NGZ,' MZ = ',MZ
           CALL STOP('GRID: NGZ .GT. MZ') 
        ENDIF
C
 1000   FORMAT(2(A,I4))
C
C  OUTPUT TO OUTPUT FILE
C	
	   WRITE(LOUT,2000)
 2000	   FORMAT('1 GRID (UNITS ARE SOLAR RADII)'//
     *     'DIRECTION               MIN        MAX            NPTS')
	   WRITE(LOUT,2001) GXMIN,GXMAX,NGX
	   WRITE(LOUT,2002) GYMIN,GYMAX,NGY
	   WRITE(LOUT,2003) GZMIN,GZMAX,NGZ
 2001	   FORMAT(1P,'X (LINE OF SIGHT)   : ',2(1X,E10.3),1X,I8,0P)
 2002	   FORMAT(1P,'Y (PLANE OF SKY E-W): ',2(1X,E10.3),1X,I8,0P)
 2003	   FORMAT(1P,'Z (PLANE OF SKY N-S): ',2(1X,E10.3),1X,I8,0P//)
C
C  OUTPUT TO ATMOS FILE
C	
	IF(IWATMO .NE. 0) THEN 
	   WRITE(LATMOS) GXMIN,GXMAX,NGX
	   WRITE(LATMOS) GYMIN,GYMAX,NGY
	   WRITE(LATMOS) GZMIN,GZMAX,NGZ
	ENDIF
 200	   FORMAT(A)
 203	   FORMAT(A,A)
 204	   FORMAT(1P,2(E11.4,1X),0P,I4)
      IF(NGX .LE. 1) WRITE(LJOBLO,301)
 301	FORMAT(' GRID: WARNING, NGX .LE. 1')
      IF(NGY .LE. 1) WRITE(LJOBLO,302)
 302	FORMAT(' GRID: WARNING, NGY .LE. 1')
      IF(NGZ .LE. 1) WRITE(LJOBLO,303)
 303	FORMAT(' GRID: WARNING, NGZ .LE. 1')
	RETURN
	END
