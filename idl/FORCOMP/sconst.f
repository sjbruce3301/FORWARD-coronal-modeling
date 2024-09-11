C
C PURPOSE: SETS CONSTANT PHYSICAL DATA
C
C INPUTS:
C
C OUTPUTS:
C
C COMMON:
C
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C 
      BLOCK DATA SCONST
      INCLUDE 'PREC'
      INCLUDE 'PARAM'
      INCLUDE 'CCONST'
      INCLUDE 'CBCNST'
      INCLUDE 'CINTS'
      DATA EE/1.602189E-12/,HH/6.626176E-27/,CC/2.99792458E10/,
     * EM/9.109534E-28/,UU/1.6605655E-24/,BK/1.380662E-16/,
     * PI/3.14159265359/,RSUNCM/6.9599E10/
      DATA ZERO/0./,ONE/1./,TWO/2./,THREE/3./,FOUR/4./,
     * FIVE/5./,SIX/6./,SEVEN/7./,EIGHT/8./,ZNINE/9./,TEN/10./
C
C  MACHINE DEPENDENT DATA:
C  NEWSTA   ACTUAL STATUS USED IN OPEN WHEN ASKED FOR STATUS='NEW'
C           RECOMMENDED VALUES:
C           VMS    'NEW'     GIVES A NEW VERSION
C           UNIX   'UNKNOWN' PREVENTS CRASH IF FILE ALREADY EXISTS
C  IRC      RECORD LENGTH OF DIRECT ACCESS FILES IS SET TO THE
C           NUMBER OF WORDS*IRC. IF THE RECORD LENGTH IS 
C           TO BE GIVEN IN 4 BYTE WORDS, IRC SHOULD BE 1 IN SINGLE
C           PRECISION, 2 IN DOUBLE PRECISION (EXAMPLES: VAX, DEC ULTRIX)
C           IF THE RECORD LENGTH IS TO BE GIVEN IN BYTES, IRC SHOULD BE
C           4 FOR 32 BIT PRECISION, 8 FOR 64 BIT PRECISION RUNS.
C
C PGJ  12-APRIL-1991  8 FOR DOUBLE PRECISION, 4 FOR SINGLE ON SUN IPC
C Note switched to 4 to be consistent with 6jul22 version
      DATA NEWSTA,IRC/'UNKNOWN',4/
C       DATA NEWSTA,IRC/'UNKNOWN',1/
C END PGJ 
C
      END
C
C***********************************************************************
C
