C
C PURPOSE: INITIALIZE VARIABLES.  SOME SHOULD BE RELACED BY INPUT FILE
C
C INPUTS:
C
C OUTPUTS:
C
C COMMON:
C
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
      SUBROUTINE STARTD
      INCLUDE 'PREC'
      INCLUDE 'PARAM'
      INCLUDE 'CATOM'
      INCLUDE 'CATMOS'
      INCLUDE 'CSLINE'
      INCLUDE 'CCONST'
      INCLUDE 'CINPUT'
      INCLUDE 'CLU'
      INCLUDE 'COPCL'
C
C  OPEN GLOBAL FILES ALWAYS NEEDED
C
      I=1
      CALL OPEN(LOUT,'OUT',I,'NEW')
      I=1
      CALL OPEN(LTIME,'TIME',I,'NEW')
      I=1
      CALL OPEN(LJOBLO,'JOBLOG',I,'NEW')
      I=1
      CALL OPEN(LDUMS,'DUMS',I,'NEW')
C
C SOME CONSTANTS, THAT COULD BE REPLACED BY READING INPUTS
C
      CALL RINPUT

      IF(IWATMO .NE. 0) THEN 
        I=0
        CALL OPEN(LATMOS,'ATMOS',I,'NEW')
        WRITE(LATMOS) CRTN
      ENDIF
      
C     
C  NEEDED FOR RACAH ALGEBRA
C
      CALL FCTSG
C
C  READ ATOMIC PARAMETERS
C
      CALL ATOM
      CALL WATOM(1)
      CALL ATOM1
      CALL COLRD
      CALL FREQ
      RETURN
      END

