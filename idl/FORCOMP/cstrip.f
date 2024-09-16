C
C PURPOSE: STRIP COMMENT LINES (LINES WITH * IN POSITION 1) FROM INPUT FILE
C
C INPUTS:
C     IUNIT   LOGICAL UNIT NUMBER OF INPUT FILE
C     FILE    FILENAME
C OUTPUTS:
C
C COMMON:
C     CLU
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C  THE FILE IS OPENED AND THE NON-COMMENT LINES ARE
C  WRITTEN TO UNIT LDUMS
C 
      SUBROUTINE CSTRIP(IUNIT,FILE)
      INCLUDE 'PREC'
      INCLUDE 'CLU'
C
      CHARACTER*132 TEXT
      CHARACTER*(*) FILE
      DATA ISTAT /1/
C
      CALL OPEN(IUNIT,FILE,ISTAT,'OLD')
      CALL REWIND(LDUMS)
  100 CONTINUE
        READ(IUNIT,200,END=900) TEXT
        IF(TEXT(1:1).NE.'*') WRITE(LDUMS,200) TEXT
  200   FORMAT(A)
      GOTO 100
C
  900 CONTINUE
      CALL REWIND(LDUMS)
      CALL CLOSE(IUNIT)
      RETURN
      END
C
C**********************************************************************
C