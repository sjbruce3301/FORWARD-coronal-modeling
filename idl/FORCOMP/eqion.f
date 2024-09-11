C
C PURPOSE: CALCULATES CORONAL IONIZATION FRACTIONS
C
C INPUTS: TEMP
C
C OUTPUTS:
C
C COMMON:
C     CATOM 
C COMMENTS: OCTOBER 6, 1999, P. JUDGE
C Changes made for reading in new chianti.ioneq file June 2016
C  LXIFF now 1e-30 for all values for which XIFF = 0 up to element MXI rather than just NIN, 7-Oct-2016, TAK
C 
      FUNCTION EQION(TEMP)
      INCLUDE 'PREC'
      INCLUDE 'PARAM'
      PARAMETER (MXI=150)
      INCLUDE 'CATOM'
      INCLUDE 'CINPUT'
      INCLUDE 'CLU'
      DATA ICALL/0/
      DIMENSION TIFF(MXI), XIFF(MXI), XLIFF(MXI)
      SAVE ICALL,TIFF,XIFF,XLIFF,NIN
C     
      IF (ICALL .EQ. 0) THEN 
         ICALL=ICALL+1
C
C  FIND ELEMENT AND CHARGE
C
 
         IZ=IATOMN(ATOMID(1:2))   
         IF (IZ .LT. 1 .OR. IZ .GT. 92) 
     *      CALL STOP('EQION: ATOMIC NUMBER OUT OF RANGE')
         ISPEC=ION(1)
         I=1
         CALL OPEN(IUNIT,'IONEQ',I,'OLD') 
         READ(IUNIT,*) NIN
         IF(NIN .GT. MXI) CALL STOP('EQION: NIN .GT. MXI')
         READ(IUNIT,*) (TIFF(I),I=1,NIN)
         IEL=0
         ISP=0
         DO K=1,1000
             READ(IUNIT,'(2I3,150E10.2)', END=40), IEL,ISP,(XIFF(I),I=1,NIN)
           IF( (IEL .EQ. IZ) .AND. (ISP .EQ. ISPEC)) THEN
              CALL CLOSE(IUNIT) 
              GOTO 50
           ENDIF
         ENDDO
 40      CALL STOP('EQION: ELEMENT AND ION NOT FOUND IN THE IONEQ FILE')
 50      CONTINUE
         DO I=1,MXI
            XLIFF(I)=-30.
            IF(XIFF(I) .GT. 0.) XLIFF(I)=LOG10(XIFF(I))
         ENDDO
         IF(IWEQI .EQ. 1) THEN
            WRITE(LOUT,53) 
 53         FORMAT('1 IONIZATION EQUILIBRIUM: ',
     *             'SPLINE INTERPOLATION ON LOGARITHMIC VALUES'//
     *           ' LOG10 TE     ION FRACTION',
     *           ' (VALUES OUTSIDE TABULATED TE RANGE ARE ZERO'//)
            DO I=1, NIN
               IF(XIFF(I) .GT. 0.) WRITE(LOUT,54)TIFF(I),XIFF(I)
 54            FORMAT(1X,F9.2,3X,1P,E9.2,0P)
            ENDDO
            WRITE(LOUT,55) 
 55         FORMAT(//)
         ENDIF
      ENDIF
C
C INTERPOLATE IN LOG SPACE
C
      TLG=LOG10(TEMP)
      ZER=0.0
      NINTEP=MIN(ISPLIN,NIN) 
      EQION=10.**SPLIN(TLG,TIFF,XLIFF,NIN,NINTEP)
      EQION=MAX(EQION,ZER)
C      eqion=0.295*exp(-1.*(tlg-6.2)**2/(2.*0.1**2))
C      WRITE(*,*),'EQION,TLG,TIFF,XLIFF,NIN,NINTEP',EQION,TLG,TIFF,XLIFF,NIN,NINTEP
      RETURN
      END


