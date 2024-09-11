pro forcomp,compline,stokesline,r3D,theta3D,phi3D,Dens,Temp,Br3D,Bth3D,Bph3D,Vel3D,compbite,complbite,wavelengths,velocities,centwave,linewidth,lchisq

;
; PURPOSE: MAIN PROGRAM TO COMPUTE EMISSION COEFFS OF M1 CORONAL LINES
;          INTEGRATED ALONG THE LOS, FOR A SET OF LOS'S IN AN 
;          ASSIGNED GRID OF CARTESIAN COORDINATES.
;
; INPUTS:
;
; OUTPUTS:
;
; COMMON:
;
; COMMENTS: AUGUST 9, 2000 P. JUDGE
;   revised J. DOVE 2009?
;   converted to IDL S. Gibson FEB 2014
; 
;
;  MAIN PROGRAM
;
;  CALCULATES THE EMISSION COEFFICIENTS OF FORBIDDEN CORONAL
;  LINES, INTEGRATED ALONG THE LOS, FOR A SET OF LOS'S IN AN 
;  ASSIGNED GRID OF CARTESIAN COORDINATES.
;

@include.icle
@include.icle.input
@include.icle.atom
@include.icle.cse
@include.icle.emerge

Case 1 of 
      strupcase(compline) eq 'SI10COMP' or strupcase(compline) eq 'SICOMP': line='si10'
      strupcase(compline) eq 'SI9COMP': line='si9'
      strupcase(compline) eq 'FE11COMP': line='fe11'
      strupcase(compline) eq 'COMP' or strupcase(compline) eq 'OTHERCOMP' or strupcase(compline)  eq 'WAVECOMP': line='fe13'
      strupcase(compline) eq 'CORMAG': line='fe14'
endcase

      if strupcase(compline) eq 'OTHERCOMP' then useline=1 else useline=0
      uselinekount=useline

      for_iclestart

      nline=atom.nline

      wlmin=input.wlmin
      wlmax=input.wlmax

      kount=0
      for nl=0,nline-1 do begin
       wwcheck=for_convl(trn[nl].ALAMB)
       if wwcheck ge wlmin and wwcheck le wlmax then begin
        kount=kount+1
       endif else begin
        if useline eq nl then uselinekount = useline+1
       endelse
      endfor

      qnorm=input.qnorm
      cc=const.cc

;
; wavelengths and weights for integrating over wavelength
;

      nwavelength=max(trn.NQ)
      dlambda=dblarr(nwavelength)
      wavelengths=dblarr(nwavelength)

;
;  delta lambda in Doppler width velocity units
;  convert to angstroms
;
      Q=trn[uselinekount].Q(0:nwavelength-1)
      QN=QNORM*1.E5/CC

      for ny=0,nwavelength-1 do dlambda(ny)=-1.*trn[uselinekount].ALAMB*Q(NY)*QN

; this is actually delta wavelength
; now switch to wavelength in angstroms
;	not delta
;
;  center wavelength needs to be converted vacuum to air
;
      oldcentwave=for_convl(trn[uselinekount].ALAMB)
;
; outu.f calls this WW the first time that is calculated
; (although note, WW gets redefined later to be ALAMB*QN/(1+QN)
; its the central wavelength

      wavelengths=double(dlambda+oldcentwave)
      
      velgrid=dlambda*3.d5/double(oldcentwave)
;
; set up sign for integrating Stokes V below
;

      sign=dblarr(nwavelength) + 1.
      signtest=where(q lt 0)
      sign(signtest)=-1.
;
; WQ seems to be quadrature weights
; WW is units conversion
;

      WQ=trn[uselinekount].WQ(0:nwavelength-1)
      WW=trn[uselinekount].ALAMB*QN/(1.0+QN)
      weight=WQ*WW

      npos = n_elements(r3D[0,*])

      compbite=dblarr(6,npos) 
      velocities=dblarr(npos)
      complbite=dblarr(5,npos,nwavelength)

      centwave=dblarr(npos) 
      linewidth=dblarr(npos)
      icent=dblarr(npos)
      lchisq=dblarr(npos)
;
;  loop over POS coordinate 
;

      for i = 0L,npos-1 do begin
       for_m1synth,line,float(r3D[*,i]),float(theta3D[*,i]),float(phi3D[*,i]),float(Br3D[*,i]),float(Bth3D[*,i]),float(Bph3D[*,i]),float(Dens[*,i]),float(Temp[*,i]),float(Vel3D[*,i])

       stokes=dblarr(kount,5,nwavelength)
       kount=0
       for nl=0,nline-1 do begin
        wwcheck=for_convl(trn[nl].ALAMB)
        if wwcheck ge wlmin and wwcheck le wlmax then begin
         stokes(kount,*,*)=emerge.emerge(nl,*,0:nwavelength-1)
         kount=kount+1
        endif
       endfor
;
;emerge={emiss:dblarr(atom.nline,5,param.mq),emerge:dblarr(atom.nline,5,param.mq)}
;
       for j = 0L,4 do begin
        for ny = 0,nwavelength-1 do begin
         sign=1.
         if j ge 3 and Q(ny) lt 0 then sign=-1.
;
; compbite is the unsigned integral over wavelength
;
         compbite(j,i)=compbite(j,i)+sign*stokes(useline,j,ny)*weight(ny)
        endfor
       endfor

       nchange=where(stokes(useline,0,*) eq max(stokes(useline,0,*)))
       compbite(5,i)=stokes(useline,0,nchange)
       centwave(i)=min(wavelengths(nchange))

       complbite(*,i,*)=stokes(useline,*,*)
;
; maybe not need to use quadrature weights
; and units conversion cancel anyways
; 1/2 1 1 1 1 1 ... 1/2
; trapezoid - just integration
;
; stokes v screwed up by velocity
; wont be centered on the middle
;
       velocities(i)=total(velgrid*complbite(0,i,*)*weight)
       velocities(i)=velocities(i)/total(complbite(0,i,*)*weight)
;
; change central wavelength if shifted by velocity
; also central intensity
;
;
; fit a Gaussian to I
;
       y=fltarr(nwavelength)
       for j = 0L,nwavelength-1L do y(j)=stokes(useline,0,j)
       dontrun=0
       if min(y) eq 0. and max(y) eq 0. then dontrun=1
       if dontrun eq 0 then begin
             fit=gaussfit(wavelengths,y,a,nterms=3,sigma=sigma,chisq=chisq)
             centwave(i)=a(1)
             icent(i)=a(0)
             linewidth(i)=2.*sqrt(2.*alog(2))*a(2)
             lchisq(i)=chisq
       endif
;


       for stokespram=0,4 do begin
;
; but do we need the quadrature here?
; def. need the units conversion
;
        if stokespram lt 3 then compbite(stokespram,i)=total(stokes(useline,stokespram,*)*weight(*)) $
         else compbite(stokespram,i)=total(stokes(useline,stokespram,*)*weight(*)*sign(*)) 
       endfor

      endfor

END
      

