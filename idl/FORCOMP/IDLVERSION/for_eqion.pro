function for_eqion,temp
;
; purpose: calculates coronal ionization fraction
;
; inputs: temp
; 
; CALLED BY FOR_CORONA
;
@include.icle.atom
common eqion,atomold,ionold,tiff,xiff,y2
;     
;
ion = lvl[0].ion
if(n_elements(atomold) eq 0) then atomold = 'DUMMY'
if(n_elements(ionold) eq 0) then ionold = -999

if (atom.atomid ne atomold or ion ne ionold) then begin 
   print,'reading ionization equilibrium file'
   atomold=atom.atomid
   ionold=ion
;
;  find element and charg
;
   iz=for_atomn(atom.atomid)
   if (iz  lt 1 or iz gt 92) then message,'atomic number out of range'
   ispec=ion(0)
;
;  open file, read and fill data
;
   nin=0l
   openr,iunit,'IONEQ',/get_lun
   readf,iunit, nin
   tiff=dblarr(nin)  ; temperature grid
   xiff=tiff ; ionization fraction 
   readf,iunit, tiff
   iel=0l
   isp=0l
   while not eof(iunit) do begin 
      readf,iunit, iel,isp,xiff  ; read log10 temperature
      if( (iel eq iz) and (isp eq ispec)) then begin 
         free_lun,iunit ; all done reading
         goto, ok
      endif
   endwhile
   message,'element and ion not found in the ioneq file'
ok:
   pos=where(xiff gt 0., npos)
   if(npos eq 0) then message,' ionization fractions are all negative'
   mnp=min(pos) & mxp=max(pos)
;
;;  y = a x + b  solve for this at ends
;  y1 = a x1 + b
;  y2 = a x2 + b
;  a= (y2-y1) / (x2-x1)
;  b  = y1- a x1
;  lower temperature end
;
   z=where(xiff le 0.,nz)  ; small (zero) fractions
   if(nz ne 0) then begin  ; linear extrapolation
      il=mnp & ih=il+1
      lo=where(tiff lt tiff[mnp],nlo)
;
; lower boundary
;
      if(nlo gt 0) then begin 
         il=mnp & ih=1+mnp
         lxiff=alog10(xiff)
         a= (lxiff[ih] - lxiff[il]) / (tiff[ih] - tiff[il])
         b= lxiff[ih] - a* tiff[ih] 
         xiff(lo) = (a * tiff[lo] + b)
      endif
;
; upper boundary
;
      hi=where(tiff gt tiff[mxp],nhi)
      if(nhi gt 0) then begin 
         il=mxp-2 & ih=il+1
         lxiff=alog10(xiff)
         a= (lxiff[ih] - lxiff[il]) / (tiff[ih] - tiff[il])
         b= lxiff[ih] - a* tiff[ih] 
         xiff(hi) = (a * tiff[hi] + b)
      endif
   endif
;
;
; edited to exactly reproduce fortran version,
; although it may be less accurate than the extrapolation above
; to regain that version, uncomment next line, comment
; following three
;  
;   xiff[pos]=alog10(xiff[pos])

   xiffsave=xiff
   xiff=xiff*0.-30.
   xiff[pos]=alog10(xiffsave[pos])

   y2=spl_init(tiff,xiff,/double) ; store spline, allowing linear extrapolation at ends (2nd deriv=0)
endif
;
; interpolate in log space using spline
;
tlg=alog10(temp)
eqion=10.^spl_interp(tiff,xiff,y2,tlg,/double)
;eqion=0.295*exp(-1.*(tlg-6.2)^2/(2.*0.1^2))
;stop
return,eqion
end


