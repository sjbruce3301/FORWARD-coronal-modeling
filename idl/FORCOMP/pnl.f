        SUBROUTINE PNL(MM,M,N,X,PM,PD)
C
C       =====================================================
C       Purpose: Compute the associated Legendre functions 
C                Pmn(x) and their derivatives Pmn'(x)
C       Input :  x  --- Argument of Pmn(x)
C                m  --- Order of Pmn(x),  m = 0,1,2,...,n
C                n  --- Degree of Pmn(x), n = 0,1,2,...,N
C                mm --- Physical dimension of PM and PD
C       Output:  PM(m,n) --- Pmn(x)
C                PD(m,n) --- Pmn'(x)
C       =====================================================
C
        INCLUDE 'PREC'
        INCLUDE 'CINTS'
        DIMENSION PM(0:MM,0:MM),PD(0:MM,0:MM)
        DO 10 I=0,N
        DO 10 J=0,M
           PM(J,I)=ZERO
10         PD(J,I)=ZERO
        PM(0,0)=ONE
        IF (ABS(X).EQ.ONE) THEN
           DO 15 I=1,N
              PM(0,I)=X**I
15            PD(0,I)=I*(I+ONE)*X**(I+1)/TWO
           DO 20 J=1,N
           DO 20 I=1,M
              IF (I.EQ.1) THEN
                 PD(I,J)=1.0E+36
              ELSE IF (I.EQ.2) THEN
                 PD(I,J)=-(J+2)*(J+1)*J*(J-1)*X**(J+1)/FOUR
              ENDIF
20         CONTINUE
           RETURN
        ENDIF
        LS=1
        IF (ABS(X).GT.ONE) LS=-1
        XQ=SQRT(LS*(ONE-X*X))
        XS=LS*(ONE-X*X)
        DO 30 I=1,M
30         PM(I,I)=-LS*(TWO*I-ONE)*XQ*PM(I-1,I-1)
        DO 35 I=0,M
35         PM(I,I+1)=(TWO*I+ONE)*X*PM(I,I)
        DO 40 I=0,M
        DO 40 J=I+2,N
           PM(I,J)=((TWO*J-ONE)*X*PM(I,J-1)-
     &             (I+J-ONE)*PM(I,J-2))/(J-I)
40      CONTINUE
        PD(0,0)=ZERO
        DO 45 J=1,N
45         PD(0,J)=LS*J*(PM(0,J-1)-X*PM(0,J))/XS
        DO 50 I=1,M
        DO 50 J=I,N
           PD(I,J)=LS*I*X*PM(I,J)/XS+(J+I)
     &             *(J-I+ONE)/XQ*PM(I-1,J)
50      CONTINUE
        RETURN
        END
