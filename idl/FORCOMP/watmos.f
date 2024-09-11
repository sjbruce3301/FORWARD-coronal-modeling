C
C PURPOSE: WRITES ATMOSPHERIC PARAMETERS TO OUTPUT LISTING
C
C INPUTS: IX, IY, IZ: INDICES OF POINTS IN X Y Z GRID
C         IGNORE- SKIP THIS POINT UNLESS IWATMO=2
C OUTPUTS:
C
C COMMON:
C
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
      SUBROUTINE WATMOS(IX,IY,IZ,IGNORE)
C
C  
C
      INCLUDE 'PREC'
      INCLUDE 'PARAM'
      INCLUDE 'CINPUT'
      INCLUDE 'CGRID'
      INCLUDE 'CATMOS'
      INCLUDE 'CATMO2'
      INCLUDE 'CATOM'
      INCLUDE 'CLU'
      COMMON /WATM/BX,BY,BZ
      LOGICAL IGNORE, PRI
C
      PRI=.FALSE.
      IF (IWATMO .EQ. 2) THEN 
         PRI=.TRUE.
      ELSE IF(IWATMO .EQ. 1) THEN
         IF (.NOT. IGNORE) PRI=.TRUE.
      ENDIF
      IF(PRI) THEN 
        WRITE(LATMOS) IX,IY,IZ
        WRITE(LATMOS) BX,BY,BZ,ED,TEMP,VEL*QNORM,VTURB*QNORM
      ENDIF
      RETURN
      END
C
C****************************************************************
C
