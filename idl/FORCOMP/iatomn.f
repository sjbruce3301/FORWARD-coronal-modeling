      INTEGER FUNCTION IATOMN(STRING)
C
C  ATOMN 94-02-22  NEW ROUTINE: (PHILIP JUDGE)
C  GIVES ATOMIC NUMBER OF ARELEMENT IF STRING IS A STRING CONTAINING
C  THE NAME OF THE ARELEMENT, E.G. IF INPUT  IS 'H ' IT WILL RETURN 1
C        
      PARAMETER (NDATA=28)
      CHARACTER*2 ARELEM(NDATA)
      CHARACTER *(*) STRING
      DATA ARELEM /'H ','HE','LI','BE','B ','C ','N ','O ','F ','NE',
     * 'NA','MG','AL','SI','P ','S ','CL','AR',
     * 'K ','CA','SC','TI','V ','CR','MN','FE','CO','NI'/
      IATOMN=-1
      DO 1 I=1,NDATA
        IF(STRING .EQ. ARELEM(I)) THEN
          IATOMN=I
          RETURN
        ENDIF
    1 CONTINUE
      END