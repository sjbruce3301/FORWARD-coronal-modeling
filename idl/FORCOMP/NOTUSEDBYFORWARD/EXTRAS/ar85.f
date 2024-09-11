C
C PURPOSE: COMPUTE RATE FOR COLLISIONAL IONIZATION BY AUTOIONIZATION
C          USING FORMALISMS OF ARNAUD AND ROTHENFLUG 1985
C          LIMITED TO CORONAL APPLICATIONS
C
C INPUTS:
C          ILO  index of lower level of the atom
C          IHI  index of upper level of the atom
C
C OUTPUTS:
C          CEA  collisional transition probability between levels ILO IHI
C COMMON:
C
C COMMENTS: October 6, 1999, P. JUDGE
C           THIS CONTAINS SEVERAL SELF-CONTAINED ROUTINES IN ONE PACKAGE
C
      SUBROUTINE AR85CEA(ILO,IHI,CEA)
C
C  NEW ROUTINE FOR COMPUTING COLLISIONAL AUTOIONIZATION 
C  RATES USING FORMALISM AND FORMULAE FROM ARNAUD AND ROTHENFLUG 1985
C
C
C AR85CEA 94-02-22  NEW ROUTINE: (PHILIP JUDGE)
C        
      INCLUDE 'PREC'
      INCLUDE 'PARAM'
      INCLUDE 'CATOM'
      INCLUDE 'CATMOS'
      INCLUDE 'CCONST'
      INCLUDE 'CLU'
      CHARACTER*2 CSEQ, ARELEM, ATOMNM
      LOGICAL DEBUG
      DATA DEBUG /.FALSE./
C  INITIALIZE OUTPUT TO ZERO
      CEA=0.0
C
C
C  FIND ELEMENT
C
      ARELEM = ATOMID(1:2)
      IZ=IATOMN(ARELEM)   
      ZZ=0.0 + IZ
      IF (IZ .LT. 1 .OR. IZ .GT. 92) THEN 
        WRITE(LJOBLO,*)'AR85-CEA:  ATOMIC NUMBER = ',IZ, ' FOR ',ARELEM
        WRITE(LJOBLO,*)'AR85-CEA:  NO AUTOIONIZATION INCLUDED'
        RETURN
      ELSE IF(DEBUG) THEN
        WRITE(LJOBLO,*)'AR85-CEA: ',ARELEM, ' CHARGE ',ION(ILO)-1
      ENDIF
C
C  FIND ISO-ELECTRONIC SEQUENCE
C
      ICHRGE=ION(ILO)-1
      ISOSEQ=IZ-ICHRGE
      CSEQ=ATOMNM(ISOSEQ)  
      BKT=BK*TEMP/EE
C
C*********************************************************************
C LITHIUM SEQUENCE
C*********************************************************************
C
        IF (CSEQ .EQ. 'LI') THEN 
          CEAB=1. / (1. + 2.E-4*ZZ*ZZ*ZZ)
          CEAZEFF=(ZZ-1.3)
          CEAIEA=13.6*((ZZ-0.835)*(ZZ-0.835) - 0.25*(ZZ-1.62)*(ZZ-1.62))
          Y=CEAIEA/BKT
          F1Y=FONE(Y)
CPREPRINT          CUP= 8.0E+10 * CEAB /CEAZEFF/CEAZEFF/SQRT(BKT) 
CPREPRINT     *   * EXP(-Y)*(2.22*F1Y+0.67*(1.-Y*F1Y)+0.49*Y*F1Y+1.2*(Y-F1Y))
          CUP=1.60E-07*1.2*CEAB/CEAZEFF/CEAZEFF/SQRT(BKT)*EXP(-Y)*
     *     (2.22*F1Y+0.67*(1.-Y*F1Y)+0.49*Y*F1Y+1.2*Y*(1.-Y*F1Y))
C
C  TWO SPECIAL CASES:
C
C C IV - APP A AR85
          IF(ARELEM .EQ. 'C ') CUP = CUP*0.6
C N V  - APP A AR85
          IF(ARELEM .EQ. 'N ') CUP = CUP*0.8
C O VI - APP A AR85
          IF(ARELEM .EQ. 'O ') CUP = CUP*1.25  
C
C*********************************************************************
C SODIUM SEQUENCE
C*********************************************************************
C
        ELSE IF (CSEQ .EQ. 'NA') THEN 
          IF (IZ .LE. 16) THEN 
            CEAA=2.8E-17*(ZZ-11.)**(-0.7)
            CEAIEA=26.*(ZZ-10.)
            Y=CEAIEA/BKT
            F1Y=FONE(Y)
            CUP= 6.69E+7 * CEAA *CEAIEA/SQRT(BKT) * EXP(-Y)*(1. - Y*F1Y)
          ELSE IF (IZ .GE. 18 .AND.  IZ .LE. 28) THEN 
            CEAA=1.3E-14*(ZZ-10.)**(-3.73)
            CEAIEA=11.*(ZZ-10.)*SQRT(ZZ-10.)
            Y=CEAIEA/BKT
            F1Y=FONE(Y)
            CUP= 6.69E+7 * CEAA *CEAIEA/SQRT(BKT) * EXP(-Y)  
     *      *(1. - 0.5*(Y -Y*Y + Y*Y*Y*F1Y))
          ELSE 
            RETURN
          ENDIF
        ENDIF
C
C*********************************************************************
C MAGNESIUM-SULFUR SEQUENCES
C*********************************************************************
C
        IF(CSEQ .EQ. 'MG' .OR. CSEQ .EQ. 'AL' 
     *   .OR. CSEQ .EQ. 'SI' .OR. 
     *   CSEQ .EQ. 'P ' .OR. CSEQ .EQ. 'S ') THEN 
          IF(CSEQ .EQ. 'MG') CEAIEA=10.3*(ZZ-10.)**1.52
          IF(CSEQ .EQ. 'AL') CEAIEA=18.0*(ZZ-11.)**1.33
          IF(CSEQ .EQ. 'SI') CEAIEA=18.4*(ZZ-12.)**1.36
          IF(CSEQ .EQ. 'P ' ) CEAIEA=23.7*(ZZ-13.)**1.29
          IF(CSEQ .EQ. 'S ' ) CEAIEA=40.1*(ZZ-14.)**1.1
          CEAA=4.0E-13/ZZ/ZZ/CEAIEA
          Y=CEAIEA/BKT
          F1Y=FONE(Y)
          CUP= 6.69E+7 * CEAA *CEAIEA/SQRT(BKT) * EXP(-Y)  
     *    *(1. - 0.5*(Y -Y*Y + Y*Y*Y*F1Y))
        ENDIF
C
C
C*********************************************************************
C  SPECIAL CASES
C*********************************************************************
C
C
        IF(ARELEM .EQ. 'CA' .AND.  (ICHRGE .EQ. 0)) THEN 
          CEAA = 9.8E-17
          CEAIEA= 25.
          CEAB=1.12         
          CUP=6.69E+7*CEAA *CEAIEA/SQRT(BKT) * EXP(-Y)*(1. + CEAB*F1Y)
          WRITE(LJOBLO,*)'AR85-CEA SPECIAL CASE ',ARELEM, 
     *     ' ION ICHRGE ',ICHRGE
        ELSE IF(ARELEM .EQ. 'CA' .AND.  (ICHRGE .EQ. 1)) THEN 
          CEAA = 6.0E-17
          CEAIEA= 25.
          CEAB=1.12         
          CUP=6.69E+7*CEAA *CEAIEA/SQRT(BKT) * EXP(-Y)*(1. + CEAB*F1Y)
        ELSE IF(ARELEM .EQ. 'FE' .AND.  (ICHRGE .EQ. 3)) THEN 
          CEAA = 1.8E-17
          CEAIEA= 60.
          CEAB=1.0         
          CUP=6.69E+7*CEAA *CEAIEA/SQRT(BKT) * EXP(-Y)*(1. + CEAB*F1Y)
        ELSE IF(ARELEM .EQ. 'FE' .AND.  (ICHRGE .EQ. 4)) THEN 
         CEAA = 5.0E-17
         CEAIEA= 73.
         CEAB=1.0         
         CUP=6.69E+7*CEAA *CEAIEA/SQRT(BKT) * EXP(-Y)*(1. + CEAB*F1Y)
        ENDIF
      CEA=CUP
      RETURN
      END
C
C*********************************************************************
C
C
C*********************************************************************
C
       CHARACTER*2 FUNCTION ATOMNM(I)
C
C  ATOMNM 94-02-22  NEW ROUTINE: (PHILIP JUDGE)
C  GIVES ATOMIC NAME OF ARELEMENT IF I IS AN INTEGER CONTAINING
C  E.G. IF INPUT  IS 'H ' IT WILL RETURN 1, 'HE', IT WILL RETURN 2, ETC.
C
      PARAMETER (NDATA=28)
      CHARACTER*2 ARELEM(NDATA)
      DATA ARELEM /'H ','HE','LI','BE','B ','C ','N ','O ','F ','NE',
     * 'NA','MG','AL','SI','P ','S ','CL','AR',
     * 'K ','CA','SC','TI','V ','CR','MN','FE','CO','NI'/
      IF(I .LE. NDATA .AND.  I .GE. 1) THEN 
        ATOMNM= ARELEM(I)
      ELSE
        ATOMNM='  '
      ENDIF
      RETURN
      END

C
C***********************************************************************
C
      FUNCTION FONE(X)
C
C NEW ROUTINE 24-JAN-1994
C CALCULATES F1(X) NEEDED FOR COLLISIONAL RATES OF ARNAUD AND ROTHENFLUG 
C MODIFIED BY P. JUDGE 15-MAR-1994 
C FOR LARGE VALUES OF X, USE ASYMPTOTIC LIMIT
C 
      INCLUDE 'PREC'
      DATA SPLIT /50.0/
      IF(X .LE. SPLIT) THEN 
        FONE = EXP(X)*EXPINT(1,X,EX)
      ELSE
        FONE= 1./X
      ENDIF
      RETURN
      END
C
C**********************************************************************
C
      FUNCTION FTWO(X)
C
C NEW ROUTINE 24-JAN-1994  P. JUDGE
C CALCULATES F2(X) NEEDED FOR COLLISIONAL RATES OF ARNAUD AND ROTHENFLUG 
C IF ARGUMENT X IS < BREAK, THEN USE THE EQNS 5 AND 6 OF HUMMER (1983, JQSRT, 30 281)
C BREAK GIVEN BY HUMMER AS 4.0
C
C NOTE THAT THE SUGGESTED POLYNOMIAL EXPANSION OF ARNAUD AND ROTHENFLUG
C FAILS FOR ARGUMENTS < BREAK- SEE HUMMER'S ORIGINAL PAPER.
C IT IS UNCLEAR IF THE TABULATED IONIZATION FRACTIONS OF ARNAUD AND ROTHENFLUG
C CONTAIN THIS ERROR.   
C 
      INCLUDE 'PREC'
      INCLUDE 'CCONST'
      DIMENSION P(15), Q(15)
      DATA BREAK /4.0/
      DATA P /1.0,2.1658E+02,2.0336E+04,1.0911E+06,3.7114E+07,
     * 8.3963E+08,1.2889E+10,1.3449E+11,9.4002E+11,4.2571E+12,
     * 1.1743E+13,1.7549E+13,1.0806E+13,4.9776E+11,0.0000/,
     *     Q /1.0,2.1958E+02,2.0984E+04,1.1517E+06,4.0349E+07,
     * 9.4900E+08,1.5345E+10,1.7182E+11,1.3249E+12,6.9071E+12,
     * 2.3531E+13,4.9432E+13,5.7760E+13,3.0225E+13, 3.3641E+12/
      IF(X .GE. BREAK) THEN 
        PX=P(1)
        XFACT=1.0
        DO 1 I=2,15
          XFACT = XFACT/X
          PX=PX+ P(I)*XFACT
    1   CONTINUE
C        PX = P(1) + XI*(P(2) + (XI*P(3) +(XI*P(4) + (XI*P(5) + (XI*P(6) 
C     *   + (XI*P(7) + (XI*P(8) +(XI*P(9) + (XI*P(10) + (XI*P(11) +
C     *   (XI*P(12) + (XI*P(13) +(XI*P(14)+XI*P(15))))))))))))))
C        QX = Q(1) + XI*(Q(2) + (XI*Q(3) +(XI*Q(4) + (XI*Q(5) + (XI*Q(6) 
C     *   + (XI*Q(7) + (XI*Q(8) +(XI*Q(9) + (XI*Q(10) + (XI*Q(11) +
C     *   (XI*Q(12) + (XI*Q(13) +(XI*Q(14)+XI*Q(15))))))))))))))
        QX=Q(1)
        XFACT=1.0
        DO 2 I=2,15
          XFACT = XFACT/X
          QX=QX+ Q(I)*XFACT
    2   CONTINUE
        FTWO=PX/QX/X/X
      ELSE
C
C  HUMMER'S EQUNS 5 AND 6
C  GAMMA IS EULER'S CONSTANT (ABRAMOVICZ+STEGUN)
C
        GAMMA=0.5772156649  
        F0X = PI*PI/12.
        TERM=1.0
        COUNT=0.0
        FACT=1.0
        XFACT=1.0
        DO WHILE (ABS(TERM/F0X) .GT. 1.E-8)
          COUNT=COUNT+1.0
          FACT = FACT*COUNT
          XFACT=XFACT*(-X)
          TERM = XFACT/COUNT/COUNT/FACT
          F0X=F0X+TERM
          IF(COUNT .GT. 100.) CALL STOP('FTWO: TOO MANY ITERATIONS')
        ENDDO
        FTWO=EXP(X)*((LOG(X)+GAMMA)*(LOG(X)+GAMMA)*0.5 + F0X)
      ENDIF
      RETURN
      END

