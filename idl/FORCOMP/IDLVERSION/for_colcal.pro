pro for_colcal
;
;  general routine for computing collisional rates 
;
; purpose: compute simple inelastic collision rates between bound levels only
;
; inputs:
;
; outputs:
;     c(i,j)    inelastic collision rates between levels i and j
;
; common:
;        catom coll catmos catmo2 cconst cinput clu
;
; CALLED BY FOR_M1SYNTH
;
; comments: october 6, 1999, p. judge
;
; modifications: pgj january 11, 2006. corrected relationship between 
;                upward and downward rates for cpmm data.
;                pgj january 18, 2006, removed cp and ch keywords (not needed).
;
;
; CALLED BY FOR_M1SYNTH
;
;  initialize collisional rates to 0.d0
;
@include.icle
@include.icle.input
@include.icle.atom
@include.icle.thermal
@include.icle.corona

;message,'TO BE THOROUGHLY CHECKED',/inf

c*=0.d0
cmm*=0.d0
ncol=n_elements(col.ihi)
tlg=alog10(temp)
FOR ICOL=0,NCOL-1 do begin 
;
;  IDENTIFY THE UPPER AND LOWER LEVELS:
;
   KEY=Col[icol].key
   
;
;  OMEGAS ARE GIVEN (+VE IONS)
;
   IF(col[icol].KEY  EQ  'OHM') THEN begin 
      ILO= Col[icol].ILo < Col[icol].IHi 
      IHI= Col[icol].ILo >  Col[icol].IHi 
      nt=col[icol].nt
      tgrid=alog10(col[icol].temp[0:nt-1])
      cgrid=col[icol].data[0:nt-1] 

      ct=spl_interp(tgrid,cgrid,col[icol].y2,tlg,/double)
      cdn = 8.63e-06 * ct * ed / ( lvl[ihi].g*sqrt(temp) )
      cup = cdn * pstar(ihi) / pstar(ilo)
      c(ihi,ilo) = cdn + c(ihi,ilo)
      c(ilo,ihi) = cup + c(ilo,ihi)
;     
;     CPMM VALUES ARE GIVEN (M-M' COLLISIONS WITH PROTONS)
;     
   endif ELSE IF (KEY  EQ  'CPMM')THEN begin 
      ttp=temp*input.tp2te
      nt=col[icol].nt
      tgrid=alog10(col[icol].temp[0:nt-1])
      cgrid=col[icol].data[0:nt-1] 
      ct=spl_interp(tgrid,cgrid,col[icol].y2,tlg,/double)
      cdn = hd[1] * ct   ; use hydrogen density
      cdn = ed * ct 
      m=col[icol].mhi
      mp=col[icol].mlo
      cup= cdn * pstar(ihi)/lvl[ihi].g / (pstar(ilo)/lvl[ilo].g)
      cmm(ihi,ilo,m,mp) = cdn + cmm(ihi,ilo,m,mp)
      cmm(ilo,ihi,m,mp) = cup + cmm(ilo,ihi,m,mp)
;     
;     RECIPROCAL RELATION (EQ 7A OF LANDMAN 1975 A+A 43, 285)
;     
      cmm(ihi,ilo,-m,-mp) = cdn + cmm(ihi,ilo,-m,-mp)
      cmm(ilo,ihi,-m,-mp) = cup + cmm(ilo,ihi,-m,-mp)
   ENDIF
endfor
RETURN
END
      

