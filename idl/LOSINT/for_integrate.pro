;************************************************************************
     pro for_integrate,r3D,sumB,sumpB,r_pos,ObsPramsStruct,LosPramsStruct,ModSolStruct,GridPramsStruct,x2,tau2,rbite,thbite,emint,pop2emint,StokesUV_I,StokesUV_Q,StokesUV_U,radiobite,testnearuse,intensbite,working_dir=working_dir,bg_input=bg_input,bg_output=bg_output

;+
; Name: FOR_INTEGRATE
;
; Purpose:  integrate over LOS
;
; INPUTS  r3D,sumB,sumpB,ObsPramsStruct,LosPramsStruct,ModSolStruct,x2,tau2,rbite,thbite,emint,pop2emint,StokesUV_I,StokesUV_Q,StokesUV_U,radiobite,testnearuse
;  	Note -- r3D, sumB, sumpB, x2, tau2,emint,pop2emint,raidobite,Stokes*  are all in dimensions [nlos,nactualbite]
;	  rbite,thbite are POS variables in dimension [nactualbite]  
;	but variables in  ModSolStruct may be 1D -- this info is stored in testnearuse
;
;       NEW INPUT -- R_POS -- plane of sky r, used to establish points on disk
;		for UV specpol to add chromorad (if set positive)
;
;       BG_INPUT,BG_OUTPUT -- a way of providing
;                 background images to subtract off
;                 in particular, if bg_output is set to a named file
;                 integrated pB,B will be saved in it
;                 conversely -- if bg_input is set it will be subtracted off
;                 below from pB, B before calculating POL
; 		TESTING: will check memory bites not taken
;		  so that file is complete, and ignore otherwise
;

; OUTPUTS intensbite
;
; Called by: FOR_INTENSINT
;
; Calls: FCORPOL_KL,FCOR_KL, FOR_PINPOINT
;
;
; HISTORY: 
;	Written by Sarah Gibson,Terry Kucera 2015
;-
;	Feb 5 2016: BUG FIX -- Dlos for Tau was not incorporated correctly in int_tabulated
; 	Oct 2017: BUG FIX -- UV Spectropolarimeter I, Q, U not calculated
; 	Jan 2018: Cleaned up conditionals if X then Y to be if X eq 1 then Y
;	Feb 2018: BUG FIX -- int_tabulated sometimes returns a negative number if integrating ultrasmall (positive) values
;	July 2018 - added benergy, etc
; 	April 2020 -- BUG FIX -- 
;		integrating over x for the TAU case caused problems when limits of integration got 
;		too close to +/- pi/2 - possibly due to asymmetries associated with elongation 
;		-- changed to integrating over tau
;		Also removed dlos from call to this program because not needed
;	October 2020 -- added XPOLF, XPOLB, XPOLG
;	November 2020-- added comments to clarify TB, pB relation to Bt, Br
;		also passed through testnearuse and dealt with possible 1D arrays for far field points
;	September 2021 -- expanded cnditional test for STOKESQOI etc
;	December 2021 -- passed through GridPramsStruct to get distobs
; January 2022 -- added TPOLF, TPOLB, TPOLG angular distance from TS functionality
;		also fixed some cases where no strupcase, and condensed some lines
; February 2022 -- testing background subtraction for pB,B,p
; March 2022 -- changed to TPOLU, TPOLS
;		also added hooks for TLOS
;		also changed LOSUSE to LOSP because LOSUSE has another meaning
;		also updated treatment of testnear
; December 2022 -- fixed bug where not calculating p but rather filling with pB
;  March 2023 -- put in check for 0 in denominator for atan
;  July 2023-- fixed bug where divide by zero was creatinng all NaN LOS integrations
;  July 2023 -- added warning if trying to do polarization with f corona above 115 Rsun
;  Sept 2023 -- passed through r_pos to allow adding chromorad to UV Specpol if set
;  Oct 2023 -- ignore nans (e.g. from AWSOM/PSIMAS below transition region)
;			updated c and h
;  Nov 2023 -- added absolute value on chromoraduse
;  Jun 2024 -- added BPOS column variables
;  Jul 2024 -- passed through ulimb
;		added PR
; Aug 2024 -- added WLRAT
;

        slash=path_sep()
;
;  useful variables
        mdtor=!dpi/180d0
	distobs=GridPramsStruct.distobs

	integrand1=0.*r3D
	integrand2=0.*r3D
	integrand3=0.*r3D

	npos = n_elements(r3D[0,*])
	intensbite=dblarr(npos)
	intensbite1=dblarr(npos)
	intensbite2=dblarr(npos)
	intensbite3=dblarr(npos)

        if tag_exist(ModSolStruct,'Pop2Dens') then pop2dens=ModSolStruct.Pop2Dens else pop2dens=ModSolStruct.Dens
        if tag_exist(ModSolStruct,'Br') then begin
          Br=ModSolStruct.Br 
          Bth=ModSolStruct.Bth 
          Bph=ModSolStruct.Bph 
  	endif else begin
 	  Br=0.
 	  Bth=0.
 	  Bph=0.
        endelse

	dens=ModSolStruct.Dens

        if min(testnearuse) ge 0 then begin
             Densreal=r3D*0.
             Pop2Densreal=r3D*0.
             Brreal=r3D*0.
             Bthreal=r3D*0.
             Bphreal=r3D*0.
             Densreal[testnearuse]=Dens
             Pop2Densreal[testnearuse]=Pop2Dens
             Brreal[testnearuse]=Br
             Bthreal[testnearuse]=Bth
             Bphreal[testnearuse]=Bph
             Dens=Densreal
             Pop2Dens=Pop2Densreal
             Br=Brreal
             Bth=Bthreal
             Bph=Bphreal
        endif

	densnotzero=where(Dens ne 0.,gooddens)
;
; NOTE all integrands have been multiplied by Rsun=6.9570d10, since integration is in Rsun units
;
        case 1 of
             (strupcase(ObsPramsStruct.IClass) eq 'EUV/XRAY IMAGERS' or strupcase(ObsPramsStruct.IClass) eq 'UV/EUV SPECTROMETERS'):  integrand1=sumB
             strupcase(ObsPramsStruct.LineName) eq 'PB' or $
              strupcase(ObsPramsStruct.LineName) eq 'TB' or $
	      strupcase(ObsPramsStruct.LineName) eq 'P' or $
	      strupcase(ObsPramsStruct.LineName) eq 'XPOLF' or $
	      strupcase(ObsPramsStruct.LineName) eq 'XPOLB' or $
	      strupcase(ObsPramsStruct.LineName) eq 'XPOLG' or $
	      strupcase(ObsPramsStruct.LineName) eq 'PR' or $
	      strupcase(ObsPramsStruct.LineName) eq 'TPOLU' or $
	      strupcase(ObsPramsStruct.LineName) eq 'TPOLS' or $
	      strupcase(ObsPramsStruct.LineName) eq 'TPOLG': begin
; note -- sumpB is the difference between tangential and radially polarized components
; note -- sumB is 2*tangentially polarized component, so sumB-sumpB= 2Bt-(Bt-Br)=Bt+Br
                  integrand1 = sumpB
	          integrand2 = sumB - sumpB
	     end
             strupcase(ObsPramsStruct.IClass) eq 'EISRATIO': intensbite = sumB
             strupcase(ObsPramsStruct.IClass) eq 'VISIBLE SPECTROMETERS': begin
                integrand1 = sumB
                message,/info,'Need to make sure we are doing the right thing for the Visible/IR '+$
                      'spectograph intensities. Then remove this line'
               end
             (strupcase(ObsPramsStruct.Instrument) eq 'LOSEM'): integrand1=emint
             (strupcase(ObsPramsStruct.Instrument) eq 'COLDEN'): begin
		if gooddens gt 0 then $ 
		integrand1[densnotzero]=emint[densnotzero]/Dens[densnotzero]
               end
             (strupcase(ObsPramsStruct.Instrument) eq 'B_INT'): begin
		if gooddens gt 0 then $
		integrand1[densnotzero]=emint[densnotzero]*sqrt((Br[densnotzero]^2$
		+Bth[densnotzero]^2+Bph[densnotzero]^2))/Dens[densnotzero]/Dens[densnotzero]
               end
             (strupcase(ObsPramsStruct.Instrument) eq 'B_POS_INT'): begin
		if gooddens gt 0 then $
		integrand1[densnotzero]=emint[densnotzero]*sqrt(($
		+Bth[densnotzero]^2+Bph[densnotzero]^2))/Dens[densnotzero]/Dens[densnotzero]
               end
             (strupcase(ObsPramsStruct.Instrument) eq 'B_DENS_INT'): begin
		if gooddens gt 0 then $
		integrand1[densnotzero]=emint[densnotzero]*sqrt((Br[densnotzero]^2$
		+Bth[densnotzero]^2+Bph[densnotzero]^2))/Dens[densnotzero]
               end
             (strupcase(ObsPramsStruct.Instrument) eq 'B_POS_DENS_INT'): begin
		if gooddens gt 0 then $
		integrand1[densnotzero]=emint[densnotzero]*sqrt(($
		+Bth[densnotzero]^2+Bph[densnotzero]^2))/Dens[densnotzero]
               end
             (strupcase(ObsPramsStruct.Instrument) eq 'BENERGY'): begin
		if gooddens gt 0 then $
		integrand1[densnotzero]=emint[densnotzero]*((Br[densnotzero]^2$
		+Bth[densnotzero]^2+Bph[densnotzero]^2))/$
		Dens[densnotzero]/Dens[densnotzero]/8.d0/!dpi
               end
             (strupcase(ObsPramsStruct.Instrument) eq 'BEN_DENS_INT'): begin
		if gooddens gt 0 then $
		integrand1[densnotzero]=emint[densnotzero]*((Br[densnotzero]^2$
		+Bth[densnotzero]^2+Bph[densnotzero]^2))/$
		Dens[densnotzero]/8.d0/!dpi
               end
             (strupcase(ObsPramsStruct.Instrument) eq 'POP2LOSEM'): integrand1=pop2emint
             (strupcase(ObsPramsStruct.Instrument) eq 'POP2COLDEN'): integrand1=pop2emint/pop2dens

             (strpos(strupcase(ObsPramsStruct.Instrument),'OMP') gt 0 or strupcase(ObsPramsStruct.Instrument) eq 'CORMAG'):           
             (strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS'): begin
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

			addchromorad1=0.*r_pos
			addchromorad2=0.*r_pos
			addchromorad3=0.*r_pos
			chromoraduse=abs(chromorad)*photon_energy
        ; units of ergs s^-1 cm^-2 sr^-1
	; multiplied by Rsun -- like rest of integrands
			ondisk = where(r_pos le 1.0,c_ondisk)
                        if strupcase(ObsPramsStruct.LineName) eq 'STOKESI' then begin
				integrand1=StokesUV_I
				if chromorad gt 0 and c_ondisk ne 0 then addchromorad1(ondisk)=chromoraduse
		 	endif
                        if strupcase(ObsPramsStruct.LineName) eq 'STOKESQ' then integrand1=StokesUV_Q
                        if strupcase(ObsPramsStruct.LineName) eq 'STOKESU' then integrand1=StokesUV_U
                        if strupcase(ObsPramsStruct.LineName) eq 'STOKESL' then begin
				integrand1=StokesUV_U
				integrand2=StokesUV_Q
			endif
                        if strpos(strupcase(ObsPramsStruct.LineName),'LOI') ge 0 then begin 
				integrand1=StokesUV_U
				integrand2=StokesUV_Q
				integrand3=StokesUV_I
				if chromorad gt 0 and c_ondisk ne 0 then addchromorad3(ondisk)=chromoraduse
			endif

                        if strpos(strupcase(ObsPramsStruct.LineName),'QOI') ge 0 then begin 
				integrand1=StokesUV_Q
				integrand2=StokesUV_I
				if chromorad gt 0 and c_ondisk ne 0 then addchromorad2(ondisk)=chromoraduse
			endif
                        if strpos(strupcase(ObsPramsStruct.LineName),'UOI') ge 0 then begin 
				integrand1=StokesUV_U
				integrand2=StokesUV_I
				if chromorad gt 0 and c_ondisk ne 0 then addchromorad2(ondisk)=chromoraduse
			endif
                        if strupcase(ObsPramsStruct.LineName) eq 'STOKESAZ' then begin
				integrand1=StokesUV_U
				integrand2=StokesUV_Q
			endif
             end

             (strupcase(ObsPramsStruct.Instrument) eq 'RADIO'): begin
                        if strupcase(ObsPramsStruct.LineName) eq 'STOKESI' then intensbite[*]=radiobite[0,*]
                        if strupcase(ObsPramsStruct.LineName) eq 'STOKESV' then intensbite[*]=radiobite[1,*]
                        if strpos(strupcase(ObsPramsStruct.LineName),'VOI') ge 0 then intensbite[*]=radiobite[1,*]/radiobite[0,*]
            end
             (strupcase(ObsPramsStruct.Instrument) eq 'FARADAY'): begin
			nlos = n_elements(radiobite[0,*,0])
			npos = n_elements(radiobite[0,0,*])
			integrand1=dblarr(nlos,npos)
                        if strupcase(ObsPramsStruct.LineName) eq 'RM' then integrand1[*,*]=radiobite[0,*,*]
                        if strupcase(ObsPramsStruct.LineName) eq 'FR' then integrand1[*,*]=radiobite[1,*,*]
             end
	     else:
        endcase

 	if strupcase(LosPramsStruct.LosUse) eq 'TAU' then begin
         los=tau2
;
; int_tabulated wants F(tau), tau 
;     but integrand needs to be multiplied by dl/dtau
;       l = rp2 tan(tau); dl/dtau = rp2 sec^2(tau); rp2=r3D cos(tau) 
;	  rp2 closest approach TS radial intersector -- see for_intlos.pro
;	  so multiplier is r3D/cos(tau)
;
; in order to change to integral over F(los), los
;   comment los=tau2 above, and the multipliers below on integrand
;         los=r3D*sin(tau2)
;
; and then only for 1D, can do this to try total(integrand*dlarr)
;
;	  dlarr=shift(los, -1)-los & dlarr[n_elements(los)-1]=dlarr[n_elements(los)-2]

         integrand1=r3D*integrand1/cos(tau2)
	 if n_elements(integrand2) gt 1 then integrand2=r3D*integrand2/cos(tau2)
	 if n_elements(integrand3) gt 1 then integrand3=r3D*integrand3/cos(tau2)
        endif
	if strupcase(LosPramsStruct.LosUse) eq 'XLOS' or $
	 strupcase(LosPramsStruct.LosUse) eq 'TLOS' $
		then los=x2

        if strupcase(ObsPramsStruct.Instrument) ne 'RADIO' and strupcase(ObsPramsStruct.IClass) ne 'EISRATIO' $
            and strupcase(ObsPramsStruct.LineName) ne 'WLRAT' $
            and strpos(strupcase(ObsPramsStruct.Instrument),'OMP') le 0 $
            and strupcase(ObsPramsStruct.Instrument) ne 'CORMAG' then begin
;
; ignore NaNs in integrands -- e.g. they may be PSIMAS/AWSOM transition region
;
	     testnan1=where(integrand1*0. ne 0.,n1)
	     testnan2=where(integrand2*0. ne 0.,n2)
	     testnan3=where(integrand3*0. ne 0.,n3)
	     if n1 ne 0 then integrand1[testnan1] = 0. 
	     if n2 ne 0 then integrand2[testnan2] = 0. 
	     if n3 ne 0 then integrand3[testnan3] = 0. 
	     badlos1=0
	     badlos2=0
	     badlos3=0
	     for i = 0,npos-1 do begin
	      testnear=where(abs(r3D[*,i]) lt 1000.,near)
              if near gt 0 then begin
               integrand1use=integrand1[testnear,i]
               losp=los[testnear,i]
              endif else begin
;
; this should not happen because
; testnearuse = -2 in for_intensint will not call this program
;
               integrand1use=integrand1[*,i]
               losp=los[*,i]
              endelse
; int_tabulated doesn't like repeated x values, make sure there aren't any
              ix=uniq(reform(losp))
 	      if (n_elements(ix) gt 1) then begin
      		intensbite1[i]=int_tabulated(reform(losp[ix]),reform(integrand1use[ix]))
;
; make sure a negative(positive) sign has not been introduced because of int_tabulated integrated noise level data
; this should only happen for small values
; it doesnt' fix all errors, e.g if there is a spiked function with a long tail
;
		if min(integrand1use) ge 0 and intensbite1[i] lt 0 then intensbite1[i]=-1.*intensbite1[i]
		if max(integrand1use) le 0 and intensbite1[i] gt 0 then intensbite1[i]=-1.*intensbite1[i]
	      endif else badlos1=badlos1+1
;
; note this formulation requires at least two elements to do an integral
; for integrand2 and integrand3-- should be ok
;
	      if n_elements(integrand2) gt 1 then begin 
                if near gt 0 then integrand2use=integrand2[testnear,i] else integrand2use=integrand2[*,i]
 	        if (n_elements(ix) gt 1) then begin
      	 	 intensbite2[i]=int_tabulated(reform(losp[ix]),reform(integrand2use[ix]))
;
; make sure a negative(positive) sign has not been introduced because of int_tabulated integrated noise level data
;
		if min(integrand2use) ge 0 and intensbite2[i] lt 0 then intensbite2[i]=-1.*intensbite2[i]
		if max(integrand2use) le 0 and intensbite2[i] gt 0 then intensbite2[i]=-1.*intensbite2[i]
	        endif else badlos2=badlos2+1
              endif
	      if n_elements(integrand3) gt 1 then begin 
                if near gt 0 then integrand3use=integrand3[testnear,i] else integrand3use=integrand3[*,i]
 	        if (n_elements(ix) gt 1) then begin
      		 intensbite3[i]=int_tabulated(reform(losp[ix]),reform(integrand3use[ix]))
;
; make sure a negative(positive) sign has not been introduced because of int_tabulated integrated noise level data
;
		if min(integrand3use) ge 0 and intensbite3[i] lt 0 then intensbite3[i]=-1.*intensbite3[i]
		if max(integrand3use) le 0 and intensbite3[i] gt 0 then intensbite3[i]=-1.*intensbite3[i]
	        endif else badlos3=badlos3+1
              endif
	     endfor
             if (badlos1 gt 0) then print,'Skipped '+strtrim(string(badlos1),2)+' lines of sight for insufficient data (1).'
             if (badlos2 gt 0) then print,'Skipped '+strtrim(string(badlos2),2)+' lines of sight for insufficient data (2).'
             if (badlos3 gt 0) then print,'Skipped '+strtrim(string(badlos3),2)+' lines of sight for insufficient data (3).'

	     intensbite=intensbite1
;
; fix the special cases
;
             case 1 of
              strupcase(ObsPramsStruct.LineName) eq 'PB' or $
              strupcase(ObsPramsStruct.LineName) eq 'TB' or $
              strupcase(ObsPramsStruct.LineName) eq 'P' or $
	      strupcase(ObsPramsStruct.LineName) eq 'XPOLF' or $
	      strupcase(ObsPramsStruct.LineName) eq 'XPOLB' or $
	      strupcase(ObsPramsStruct.LineName) eq 'XPOLG' or $
	      strupcase(ObsPramsStruct.LineName) eq 'PR' or $
	      strupcase(ObsPramsStruct.LineName) eq 'TPOLU' or $
	      strupcase(ObsPramsStruct.LineName) eq 'TPOLS' or $
	      strupcase(ObsPramsStruct.LineName) eq 'TPOLG': begin
		  pB=intensbite1
		  TB=intensbite2
                  if ObsPramsStruct.FCor eq 1 then begin
                         pB=intensbite1+fcorpol_kl(rbite)*fcor_kl(rbite,thbite)*1d8
			 if max(rbite) gt 115. and strupcase(ObsPramsStruct.LineName) ne 'TB' then begin
			   print,'WARNING! FCOR polarization model invalid above 115 Rsun)'
			   print,'Turn off FCOR or use only for total brightness (TB)'
			   stop
			 endif
                         TB=intensbite2+fcor_kl(rbite,thbite)*1d8
		  endif
;
; Special (TESTING) case pB, B background file input/output
;    will only work if memory=0 or array small enough not broken up
;
        	  if keyword_set(bg_input) or keyword_set(bg_output) then begin
        	   if n_elements(r3d) eq n_elements(sumB) then begin
                    if n_elements(working_dir) eq 1 then begin
                     if working_dir ne '' then frontmatter=working_dir+slash $
                      else frontmatter=''
                    endif else frontmatter=''
        	    if keyword_set(bg_input) then begin
                     pBnew=pB
                     TBnew=TB
		     print,'reading file '+bg_input
                     restore,filename=bg_input
                     pB=pBnew-pB
                     TB=TBnew-TB
                    endif
        	    if keyword_set(bg_output) then begin
  		      save,filename=frontmatter+bg_output+'.sav',pB,TB
		      print,'saved file '+frontmatter+bg_output+'.sav'
                    endif
        	   endif else print,"set memory=0 to remove pB/B background"
        	  endif 
;
		  pol=pB/TB

;
; in case subtraction introduced negatives
; (and because TB=0 will give infinite pol)
;
	 	  testneg=where(pB le 0 or TB le 0)
		  if min(testneg) ne -1 then begin
                     TB[testneg]=-8888.
		     pB[testneg]=-8888.
		     pol[testneg]=-8888.
		  endif
;
; int_tabulated sometimes ends up with pB > TB because of noise -- these are bad points
		  test=where(pol gt 1)
		  if min(test) ne -1 then pol[test] = -8888.
;
		  if strupcase(ObsPramsStruct.LineName) eq 'PB' then intensbite=pB
		  if strupcase(ObsPramsStruct.LineName) eq 'TB' then intensbite=TB
		  if strupcase(ObsPramsStruct.LineName) eq 'P' then intensbite=pol

		  if strupcase(ObsPramsStruct.LineName) eq 'XPOLF' or $
		   strupcase(ObsPramsStruct.LineName) eq 'XPOLB' or $
		   strupcase(ObsPramsStruct.LineName) eq 'XPOLG' or $
		   strupcase(ObsPramsStruct.LineName) eq 'TPOLU' or $
	           strupcase(ObsPramsStruct.LineName) eq 'PR' or $
		   strupcase(ObsPramsStruct.LineName) eq 'TPOLS' or $
		   strupcase(ObsPramsStruct.LineName) eq 'TPOLG' then begin
;
; these should only be done with TLOS or TAU
;
	            if strupcase(LosPramsStruct.LosUse) eq 'XLOS' then begin
			print,'something is wrong -- polarization diagnostics should not use LOSINT=XLOS'
			stop
		    endif

		    if tag_exist(ObsPramsStruct,'ULimb') then ulimb=ObsPramsStruct.Ulimb else ulimb=0.63
		    for_pinpoint,pB,pol,r3D,tau2,Dens,rbite,distobs,xx_plus,xx_minus,xx_ground,tau_plus,tau_minus,tau_ground,ulimb
		    if strupcase(ObsPramsStruct.LineName) eq 'XPOLF' then intensbite=xx_plus
		    if strupcase(ObsPramsStruct.LineName) eq 'XPOLB' then intensbite=xx_minus
		    if strupcase(ObsPramsStruct.LineName) eq 'XPOLG' then intensbite=xx_ground
		    if strupcase(ObsPramsStruct.LineName) eq 'TPOLU' then intensbite=tau_plus
	            if strupcase(ObsPramsStruct.LineName) eq 'PR' then intensbite=(1-pol)/(1+pol)
;
; note this next is a place holder because we haven't figured out how to do signed Tau
;
;		    if strupcase(ObsPramsStruct.LineName) eq 'TPOLS' then intensbite=tau_minus
		    if strupcase(ObsPramsStruct.LineName) eq 'TPOLG' then intensbite=tau_ground
		  endif
               end
              (strupcase(ObsPramsStruct.IClass) eq 'UV SPECTROPOLARIMETERS'): begin
               case 1 of
                strupcase(ObsPramsStruct.LineName) eq 'STOKESL': intensbite=sqrt(intensbite1^2+intensbite2^2)
                strpos(strupcase(ObsPramsStruct.LineName), 'LOI') ge 0: intensbite=sqrt(intensbite1^2+intensbite2^2)/(intensbite3+addchromorad3)
                strpos(strupcase(ObsPramsStruct.LineName), 'QOI') ge 0: intensbite=intensbite1/(intensbite2+addchromorad2)
                strpos(strupcase(ObsPramsStruct.LineName), 'UOI') ge 0: intensbite=intensbite1/(intensbite2+addchromorad2)
                strupcase(ObsPramsStruct.LineName) eq 'STOKESAZ': begin
                  uv_alpha = 0.5*atan(intensbite1,intensbite2)
                  test=where(intensbite2 eq 0. and intensbite1 eq 0.)
		  if min(test) ne -1 then uv_alpha[test] = sqrt(-1)
                  intensbite= uv_alpha/mdtor
                  intensbite=intensbite mod 180.d0
                  uvintens=intensbite
                  test=where(uvintens lt 0.)
                  if min(test) ne -1 then uvintens[test]=uvintens[test]+180.d0
                  intensbite=uvintens
                 end
                else: intensbite=(intensbite1+addchromorad1)
               endcase
               end
	      else:
	     endcase
        endif
;
; warn if end points not going to zero
;  This is not really what we want though because
;  for lines of sight far away from sun center
;  will fail, because the integrand is flat
;  will revisit later, commenting out for now
;
;	int1size=size(integrand1)
;	if int1size[0] ne 0 then begin
;	  endsize=int1size[1]
; 	  endpoint1=integrand1[0,*]/max(integrand1,dimension=1)
; 	  endpoint2=integrand1[endsize-1,*]/max(integrand1,dimension=1)
;	  if abs(max(endpoint1)) gt 1d-2 or abs(max(endpoint2)) gt 1d-2 then print,'Endpoints fractional value max='+strtrim(string(max([max(endpoint1),max(endpoint2)])),2),' Try using larger abs(LOSMIN) and increasing NLOS'
; 	endif
;	int2size=size(integrand2)
;	if int2size[0] ne 0 then begin
;	  endsize=int2size[1]
; 	  endpoint1=integrand2[0,*]/max(integrand2,dimension=1)
; 	  endpoint2=integrand2[endsize-1,*]/max(integrand2,dimension=1)
;	  if abs(max(endpoint1)) gt 1d-2 or abs(max(endpoint2)) gt 1d-2 then print,'Endpoints fractional value max='+strtrim(string(max([max(endpoint1),max(endpoint2)])),2),' Try using larger abs(LOSMIN) and increasing NLOS'
; 	endif
;	int3size=size(integrand3)
;	if int3size[0] ne 0 then begin
;	  endsize=int3size[1]
; 	  endpoint1=integrand3[0,*]/max(integrand3,dimension=1)
; 	  endpoint2=integrand3[endsize-1,*]/max(integrand3,dimension=1)
;	  if abs(max(endpoint1)) gt 1d-2 or abs(max(endpoint2)) gt 1d-2 then print,'Endpoints fractional value max='+strtrim(string(max([max(endpoint1),max(endpoint2)])),2),' Try using larger abs(LOSMIN) and increasing NLOS'
; 	endif

;
end
