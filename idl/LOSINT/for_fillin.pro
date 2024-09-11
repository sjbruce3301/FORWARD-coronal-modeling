;************************************************************************
     pro for_fillin,ObsPramsStruct,LosPramsStruct,nst,nfin,r3D,x2,tau2,StokesUV_I,StokesUV_Q,StokesUV_U,rbite,radiobite,compbite,complbite,wavelengths,wlspecbite,velbite,centbite,linebite,lchisqbite,intensbite,nunwrap,intens,stokesred,stokeslred,wlspec,velocities,centwave,linewidth,lchisq,Qprime,Uprime

;+
; Name: FOR_FILLIN
;
; Purpose:  fill in the bites into the arrays
;
; INPUTS	ObsPramsStruct,LosPramsStruct,nst,nfin,StokesUV_I,StokesUV_Q,
;		r3D,x2,tau2
;		StokesUV_U,radiobite,compbite,complbite,wavelengths,wlspecbite,velbite,centbite,
;		linebite,lchisqbite,intensbite,nunwrap
;
;
; OUTPUTS updated intens,stokesred,stokeslred,velocities,centwave,linewidth,lchisq,Qprime,Uprime
;
; Called by: FOR_INTENSINT
;
; Calls: FOR_CHANGEREF
;
; HISTORY: 
;	Written by Sarah Gibson 2015
;	   Feb 2016 - added NUNWRAP as Keyword
;	   Jan 2017 - fixed bug in Faraday rotation/angle
;		where frequency was not multiplied by 1d6
;	  early 2019 - fixed bug in integration of StokesUV
;       April 2020 -- BUG FIX --
;               integrating over x for the TAU case caused problems when limits of integration got
;               too close to +/- pi/2 - possibly due to asymmetries associated with elongation
;               -- changed to integrating over tau
;               Also removed dlos from call to this program because not needed
;	August 2020 -- bug fix
;		forgot to set intmult for UV for xlos
;	March 2022 -- allowed TLOS option
;			also replaced variable losuse with losp
;			to avoid using same name for different concepts
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;       Nov 2023 -- ignored NaNs caused by PSIMAS/AWSOM overly broad transition region
;	Aug 2024 -- added hooks for WLRAT
;-

;
; ALL BUT COMP and WLRAT
;
          if strpos(strupcase(ObsPramsStruct.Instrument),'OMP') lt 0 and strupcase(ObsPramsStruct.Instrument) ne 'CORMAG' then intens(nst:nfin)=intensbite

;
; RADIO
;
          if strupcase(ObsPramsStruct.Instrument) eq 'RADIO' then begin
            for ird=0,1 do begin
             stokesred[ird,nst:nfin]=radiobite[ird,*]
            endfor
          endif

        ; do this for Faraday - but not Stokes I and V, but RM and FR
          if strupcase(ObsPramsStruct.Instrument) eq 'FARADAY' then begin
	    if strupcase(ObsPramsStruct.LineName) eq 'RM' then begin
               stokesred[0,nst:nfin]=intensbite
	       freq=double(ObsPramsStruct.Frequency_MHz)*1d6
               stokesred[1,nst:nfin]=intensbite*((3.0e8/freq)^2)
	    endif else begin
               stokesred[1,nst:nfin]=intensbite
	       freq=double(ObsPramsStruct.Frequency_MHz)*1d6
               stokesred[0,nst:nfin]=intensbite/((3.0e8/freq)^2)
	    endelse
          endif

;
; UV SPECTPOL
;
          if strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS' then begin

;
; determine if chromorad needs to be added on disk
;  only if StokesI is involved and chromorad is positive
;
                        SpectStructCheck=ObsPramsStruct.SpecPrams
                        chromorad=SpectStructCheck.ChromoRad
; units of photons cm^-2 s^-1 sr^-1 
                        wavelength=ObsPramsStruct.Wavelength_Ang
                        frequency=2.9979d10/(wavelength*1d-8) ; units of s^-1
                        h=6.6262d-27 ; ergs*s
                        photon_energy = h*frequency ; ergs
        
                        addchromorad=0.*rbite
                        chromoraduse=abs(chromorad)*photon_energy
        ; units of ergs s^-1 cm^-2 sr^-1
        ; multiplied by Rsun -- like rest of integrands
                        ondisk = where(rbite le 1.0,c_ondisk)
                        if chromorad gt 0 and c_ondisk ne 0 then addchromorad(ondisk)=chromoraduse

;
;  other places -- for_eisratio (legacy D. Schmit code), response function stuff
;  various forcomp things (including total*weight in for_readcompresults)
;
; TOTAL THIS WAS INTRODUCING A BUG -- NEED to INTEGRATE PROPERLY
;
;            stokesred[0,nst:nfin]=total(StokesUV_I,1)
;            stokesred[1,nst:nfin]=total(StokesUV_Q,1)
;            stokesred[2,nst:nfin]=total(StokesUV_U,1)

	     IFillarr=intensbite*0.
	     QFillarr=intensbite*0.
	     UFillarr=intensbite*0.

             if strupcase(LosPramsStruct.LosUse) eq 'TAU' then begin
              los=tau2
              intmult=r3D/cos(tau2)
             endif

             if strupcase(LosPramsStruct.LosUse) eq 'XLOS' $
               or strupcase(LosPramsStruct.LosUse) eq 'TLOS' $
		then begin
	       los=x2
	       intmult=r3D*0.d0 + 1.
	     endif

;
; ignore NaNs in integrands -- e.g. they may be PSIMAS/AWSOM transition region
;
             testnani=where(StokesUV_I*0. ne 0.,n1)
             if n1 ne 0 then StokesUV_I[testnani] = 0. 
             testnanq=where(StokesUV_Q*0. ne 0.,n2)
             if n2 ne 0 then StokesUV_Q[testnanq] = 0. 
             testnanu=where(StokesUV_U*0. ne 0.,n3)
             if n3 ne 0 then StokesUV_U[testnanu] = 0. 

             badlos1=0
             badlos2=0
             badlos3=0
             npos = n_elements(r3D[0,*])
             for i = 0,npos-1 do begin
              testnear=where(r3D[*,i] lt 1000.,near)
              if near gt 0 then begin
               SIuse=intmult[testnear,i]*StokesUV_I[testnear,i]
               SQuse=intmult[testnear,i]*StokesUV_Q[testnear,i]
               SUuse=intmult[testnear,i]*StokesUV_U[testnear,i]
               losp=los[testnear,i]
              endif else begin
               SIuse=intmult[*,i]*StokesUV_I[*,i]
               SQuse=intmult[*,i]*StokesUV_Q[*,i]
               SUuse=intmult[*,i]*StokesUV_U[*,i]
               losp=los[*,i]
              endelse
; int_tabulated doesn't like repeated x values, make sure there aren't any
              ix=uniq(reform(losp))
              if (n_elements(ix) gt 1) then begin
               IFill=int_tabulated(reform(losp[ix]),reform(SIuse[ix]))
               QFill=int_tabulated(reform(losp[ix]),reform(SQuse[ix]))
               UFill=int_tabulated(reform(losp[ix]),reform(SUuse[ix]))
;
; make sure a negative(positive) sign has not been introduced because of int_tabulated integrated noise level data
;
               if min(sIuse) gt 0 and IFill lt 0 then IFill=-1.*IFill
               if max(sIuse) lt 0 and IFill gt 0 then IFill=-1.*IFill
               if min(sQuse) gt 0 and QFill lt 0 then QFill=-1.*QFill
               if max(sQuse) lt 0 and QFill gt 0 then QFill=-1.*QFill
               if min(sUuse) gt 0 and UFill lt 0 then UFill=-1.*UFill
               if max(sUuse) lt 0 and UFill gt 0 then UFill=-1.*UFill
  	       IFillarr[i]=IFill
  	       QFillarr[i]=QFill
  	       UFillarr[i]=UFill
              endif else badlos1=badlos1+1
 	     endfor
             if (badlos1 gt 0) then print,'Skipped '+strtrim(string(badlos1),2)+' lines of sight for insufficient data (1).'

             stokesred[0,nst:nfin]=IFillarr+addchromorad
             stokesred[1,nst:nfin]=QFillarr
             stokesred[2,nst:nfin]=UFillarr
          endif

;
; VISIBLE/IR SPECTPOL (NO FTRAN)

          if (strpos(strupcase(ObsPramsStruct.Instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG') and ObsPramsStruct.FCompPrams.noftran eq 1 then begin
            if nst eq 0 then begin
               stokeslred=dblarr(5,nunwrap,n_elements(wavelengths))
               velocities=dblarr(nunwrap)
               centwave=dblarr(nunwrap)
               linewidth=dblarr(nunwrap)
               lchisq=dblarr(nunwrap)
            endif
            velocities[nst:nfin]=velbite
            centwave[nst:nfin]=centbite
            linewidth[nst:nfin]=linebite
            lchisq[nst:nfin]=lchisqbite
            for ird=0,4 do begin
             stokesred[ird,nst:nfin]=compbite[ird,*]
             stokeslred[ird,nst:nfin,*]=complbite[ird,*,*]
            endfor
            stokesred[5,nst:nfin]=compbite[5,*]
	  endif

; WLRAT -- CODEX like white light spectropolarimetry

	if strupcase(ObsPramsStruct.LineName) eq 'WLRAT' then begin
	  nwavelengths=n_elements(wavelengths)
          if nst eq 0 then $
	    wlspec=dblarr(nwavelengths,nunwrap)
          wlspec[*,nst:nfin]=wlspecbite
        endif
end
