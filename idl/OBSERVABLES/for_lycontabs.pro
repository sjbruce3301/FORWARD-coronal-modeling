function for_LyContAbs,ObsPramsStruct,DLos,CEDens,fvol=fvol,fpos=fpos,reinit=reinit,Habund=Habund
;+
; Name: FOR_LYCONTABS
;
; Purpose: Calculate the absorption factor, alpha, associated with Lyman absorption
;			I= If + alpha*Ib where
;			alpha = (exp(-int(n*sigma*ds)*fpos) + (1-fpos)
;			See Kucera, Gilbert & Karpen 2014
;
; Inputs
;	ObsPramsStruct - Structure  containing observational parameters. Needed for wavelength and abundance
;	DLos - size of integration element in RSun.
;   CEDens - number density of  electrons associated with cool (chromospheric temperature) plasma. 
;
; Input keyword
;	reinit - if run for the first time recalculate the cross 
;		sections for this particular wavelength.
; 	Habund - Hydrogen fractional abundance. Overrides abundance in ObsPramsStruct, and 
;					is also used to calculate He abundance (=1-Habund)
;	fvol - volume filling factor of the cool material.  Default = 1. Current plan is that 
;				the filling factor is multiplied to dlos in for_intensint, so this will not be used.
;	fpos- filling factor in the plane of sky. Default=1
;
; Output:
;	Alpha - the absorption factor as a function of location along the line of sight.
;
; Called by FOR_INTENSINT
;
; Common Blocks:
;	LyContComm - contains the cross-sections for the relevant wavelength, and the H and He fractional abundance
;
; History
;	Created 13-Jan 2015,  T.A. Kucera
;	Modified:
;	22-Oct-2015, Fixed response for cases for which the cross 
;		section is zero and/or no wavelength is given. T. Kucera
;   Feb 2016: removed reset of REINIT=0 at bottom since
;		redundant with reset in FOR_INTENSINT
;   22-Feb-2016: Various changes T. Kucera
;	5-Jul-2023: check for abundance file in the chianti 
;		abundance/archive if it is not in main abundance directory. TAK
; 	8-Sep-2023: SEG
;               added else to check for need for archive
;  	Oct-2023- updated Rsun
;-
;	

common LyContComm,wave0,sigmaH0,SigmaHe0,sigmaHe1,H_abund,He_abund


				;extract wavelength from ObsPramStruct
wave=0
if  ObsPramsStruct.Wavelength_Ang ne 0 then Wave=ObsPramsStruct.Wavelength_Ang else begin
		tmp=strmid(ObsPramsStruct.LineName,0,strpos(ObsPramsStruct.LineName,' '))
   if is_number(tmp) then wave=float(tmp)
endelse

if wave eq 0 or wave ge 912 then return,1

if reinit eq 1 or  n_elements(wave0) eq 0 then begin 
;		print,'Reinitializing LYMAN....'
				;read in Keddy & Kilcrease 2000 Lyman Continuum Cross-section values and interpolate. 
		FORWARD_DB=for_get_forward_db()
		restgen,file=concat_dir(FORWARD_DB,'Keddy_Kilcrease_LyCont_CrossSections.genx'),XSect_wave,SigH_18,sigHe0_18,sigHe1_18
		sigmaH0  = interpol(SigH_18,XSect_wave,wave)*1e-18
		sigmaHe0=interpol(SigHe0_18,XSect_wave,wave)*1e-18
		sigmaHe1=interpol(SigHe1_18,XSect_wave,wave)*1e-18
	
	if keyword_set(HAbund) then begin
		H_abund=Habund
		He_abund=1-H_abund
	endif else begin
		;get abundances for H and He from abundance file
		ffexist=file_exist(form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance')))          
		if ffexist then $
			abund_file=form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance')) else  $
			;if abundance file is not in main abundance directory, assume it is in 
			;abundance/archive
			abund_file=form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance/archive'))
		read_abund,abund_file,abundances,ref
		H_abund=abundances[0]/total(abundances)
		He_abund=abundances[1]/total(abundances)
	endelse
    wave0=wave


endif  else if n_elements(wave0) eq 1 and wave ne wave0 then message,'Wavelength has changed since last reinit'

default,fvol,1.	
; assume the plane of sky filling factor is 1. (not even an input at this point)
default,fpos,1. 

Rsun2cm=6.957e10      ;Solar Radius in cm

;Ionization fractions. 

pop2ionfrac_fname=ObsPramsStruct.SpecPrams.Pop2IonFrac 
if file_exist(pop2ionfrac_fname) eq 0 then message, 'no file :'+pop2ionfrac_fname

restgen,file=pop2ionfrac_fname,pop2ionfrac

IonFrac=pop2ionfrac
H0_frac=IonFrac.H			          ;fraction of H that is neutral
H1_frac=1-IonFrac.H    	          ;fraction H ionized 
He0_frac=IonFrac.He[0]	          ;fraction Ne neutral
He1_frac=IonFrac.He[1]             ;fraction of He once ionized
He2_frac=1-total(IonFrac.He)    ;fraction of He twice ionized 

													;Convert electron density to particle density 
;NCPart=N_H+N_He           			; total number of ions			
													;electron density							
;CEDens=NCPart*H_abund*H1_frac+ NCPart*He_abund*He1_frac+ NCPart*He_abund*2*He2_frac)
 NCPart   = CEDens/(H_abund*H1_frac+ He_abund*He1_frac+ He_abund*2*He2_frac)

N_H=NCPart*H_abund
N_He=NCPart*He_abund
N_H0=N_H*H0_frac
N_He0=N_He*He0_frac
N_He1=N_He*He1_frac
N_Sigma=fvol*(N_H0*sigmaH0 + N_He0*sigmaHe0 + N_He1*sigmaHe1)   ;N*sigma

;How much absorption is in front of each pixel along the line of sight?
;The output array should contain the total amount of absorption applied to 
;each pixel along the line of sight - basically a total of all the absorbing material 
;in all pixels between that one and the observer _ half the absorbing material in that pixel.
;The observer is at high numbered pixels so 
;alpha close to the observer should equal 1 (no absorption), and go towards zero away from the observer (more absorption).

alpha0=0.*dlos
ASize=size(alpha0)

tau=N_Sigma*Rsun2cm*dlos

alpha0[ASize[1]-1,*]=exp(-1*.5*tau[ASize[1]-1,*])
for i=0,ASize[1]-2 do $
		alpha0[i,*]=exp(-1*( total(tau[i+1:*,*],1)+reform(.5*tau[i,*]) ))

alpha=alpha0*fpos + (1-fpos)

return, alpha


end



