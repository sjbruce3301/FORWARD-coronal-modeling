C
C PURPOSE: COMPUTES THE N'TH EXPONENTIAL INTEGRAL OF X
C
C INPUTS:
C     X  INDEPENDENT VARIABLE (-100. .LE. X .LE. +100.)
C     N  ORDER OF DESIRED EXPONENTIAL INTEGRAL (1 .LE. N .LE. 8)
C
C OUTPUTS:
C     EXPINT,  THE DESIRED RESULT
C COMMON:
C
C COMMENTS: October 6, 1999, P. JUDGE
C 
      FUNCTION OLDINT(N,X,EX)
C
C  OUTPUT - EXPINT,  THE DESIRED RESULT
C           EX,  EXPF(-X)
C  NOTE   RETURNS WITH E1(0)=0, (NOT INFINITY).
C  BASED ON THE SHARE ROUTINE NUEXPI, WRITTEN BY J. W. COOLEY,
C  COURANT INSTITUTE OF MATHEMATICAL SCIENCES, NEW YORK UNIVERSITY
C  OBTAINED FROM RUDOLF LOESER
C-----GENERAL COMPILATION OF 1 AUGUST 1967.
C
      INCLUDE 'PREC'
      DIMENSION TAB(20),XINT(7)
      DATA XINT/1.,2.,3.,4.,5.,6.,7./
      DATA TAB /.2707662555,.2131473101,.1746297218,.1477309984,
     1.1280843565,.1131470205,.1014028126,.0919145454,.0840790292,
     1.0774922515,.0718735405,.0670215610,.0627878642,.0590604044,
     1.0557529077,.0527977953,.0501413386,.0477402600,.0455592945,
     1.0435694088/
      DATA XSAVE /0./
C
      U=X
      IF(U)603,602,603
  602 EX=1.
      IF(N-1)800,800,801
  800 OLDINT=0.
      GOTO 777
  801 OLDINT=1./XINT(N-1)
      GOTO 777
  603 IF(U-XSAVE)604,503,604
  604 XSAVE=U
      XM=-U
      EMX=EXP(XM)
C
C  SELECT METHOD FOR COMPUTING EI(XM)
C
      IF(XM-24.5)501,400,400
  501 IF(XM-5.)502,300,300
  502 IF(XM+1.)100,200,200
  503 EISAVE=-ARG
      EXSAVE=EMX
C
C  NOW RECURSE TO HIGHER ORDERS
C
      IF(N-1)507,507,505
  505 DO 506 I=2,N
        EISAVE=(U*EISAVE-EXSAVE)/(-XINT(I-1))
  506 CONTINUE
  507 OLDINT=EISAVE
      EX=EXSAVE
  777 RETURN
C
C  EI(XM) FOR XM .LT. -1.0
C  HASTINGS POLYNOMIAL APPROXIMATION
C
  100 ARG=((((((U+8.573328740 )*U+18.05901697  )*U+8.634760893 )*U
     *+.2677737343)/XM)*EMX)/((((U+9.573322345 )*U+25.63295615  )*U
     *+21.09965308  )*U+3.958496923 )
      GOTO 503
C     EI(XM) FOR -1. .LE. XM .LT. 5.0
C     POWER SERIES EXPANSION ABOUT ZERO
  200 ARG=LOG(ABS(XM))
      ARG=((((((((((((((((.41159050E-14*XM+.71745406E-13)*XM+.76404637E-
     *12)*XM+.11395905E-10)*XM+.17540077E-9)*XM+.23002666E-8)*XM+.275360
     *18E-7)*XM+.30588626E-6)*XM+.31003842E-5)*XM+.28346991E-4)*XM+.2314
     *8057E-3)*XM+.0016666574)*XM+.010416668)*XM+.055555572)*XM+.25)*XM+
     *.99999999)*XM+.57721566)+ARG
      GOTO 503
C
C  EI(XM) FOR 5.0 .LE. XM .LT. 24.5
C  TABLE LOOK-UP AND INTERPOLATION
C
  300 I=XM+.5
      XZERO=I
      DELTA=XZERO-XM
      ARG=TAB(I-4)
      IF(DELTA)303,305,303
  303 Y=ARG
      DELTAX=DELTA/XZERO
      POWER=1./DELTAX
      DO 304 I=1,7
        POWER=POWER*DELTAX
        Y=((Y-POWER/XZERO)*DELTA)/XINT(I)
        ARG=ARG+Y
        IF(ABS(Y/ARG)-1.E-8)305,304,304
  304 CONTINUE
  305 ARG=EMX*ARG
      GOTO 503
C     EI(XM) FOR 24.5 .LE. XM
C     TRUNCATED CONTINUED FRACTION
  400 ARG=((((XM-15.)*XM+58.)*XM-50.)*EMX)/((((XM-16.)*XM+72.)*XM-96.)
     **XM+24.)
      GOTO 503
      END
C
C***************************************************************
C
