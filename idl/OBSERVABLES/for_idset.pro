;**************************************************************************
;+
pro for_idset,ObsPramsStruct,LogT,Dens,IonDens,ChromoDens,$
	nowidgmess=nowidgmess
;
; Name: FOR_IDSET
;
; Inputs
;	ObspramsStruct - contains:
;		line - string specifying EIS line to be calculated. 
;			;Format example: 'Fe10_185.2'
;		SpecPrams - parameters related to spectra		
;	LogT - Array of Log Temperature (K) values. Default 6.2
;	Dens - Array of electron density values (cm^-3). Default 10^9 cm^-3
;
;Keyword Inputs
;	nowidgmess-- don't use widget text output
;
;Output:
;	ion density 
;	chromospheric density determined from chromorad which lives
;	  in SpecPrams in ObsPramsStruct

; Called by FOR_IONDENS
;
; Calls FOR_UNPACKLINENAME, READ_ABUND, READ_IONEQ
;
;History:
;	Created Jul 2023 S Gibson
;	modified from for_line_mflux
; Oct 2023 -- updated to calculate chromodens
;	and for consistency throughout FORWARD-CHIANTI interface
; Dec 2023 -- added links to updates rates file for NeVIII and OVI
; Feb 2024 -- added links to rates file for MgIX
;-

;
; test for PC
;

usewindows=0
if strupcase(!version.os_family) eq 'WINDOWS' then usewindows=1
slash=path_sep()

SpecPrams=ObsPramsStruct.SpecPrams
line=ObsPramsStruct.LineName
abundance=SpecPrams.Abundance
chromorad=SpecPrams.ChromoRad

for_unpacklinename,line,element,ionnum,wavelength,chianti_ion,iz
;Outputs
;       element - elements (e.g., FE)
;       ionnum - ion number, (e.g., 12)
;       wavelength - line wavelength (e.g., 195.1 A)
;       chianti_ion - ion in format element_ionnum (e.g., fe_12)
;	iz - atomic number (e.g., 26)

;elemental abundance in corona
;for_specdefaults has set most up to date coronal abundance file
ffexist=file_exist(form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance')))        
if ffexist then $
	 abund_file=form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance')) else $
        ;if abundance file is not in main abundance directory, assume it is in 
        ;abundance/archive
	 abund_file=form_filename(abundance,'.abund',dir=concat_dir(!xuvtop,'abundance/archive'))
read_abund,abund_file,abundances,ref
Element_abund=(abundances(iz-1))[0]

;ion equilibrium for ion fraction

ioneq_file=form_filename(SpecPrams.IonEq,'.ioneq',dir=concat_dir(!xuvtop,'ioneq')) 
read_ioneq,ioneq_file,ioneq_temp,ion_fractions,ref

ion_frac=interpol(ion_fractions[*,iz-1,ionnum-1],ioneq_temp,LogT)

iondens=Element_abund*ion_frac*Dens
; units of cm^-3
 
; Now calculate chromodens ONLY FOR FOR_UVMODEL LINES
;
if strupcase(chianti_ion) eq 'NE_8' or strupcase(chianti_ion) eq 'O_6' or strupcase(chianti_ion) eq 'MG_9' or strupcase(chianti_ion) eq 'H_1' then begin

; ionization for chromosphere (independent of coronal model, based on temperature of formation)

 if strupcase(chianti_ion) eq 'NE_8' then chromotemp=10^5.9
 if strupcase(chianti_ion) eq 'MG_9' then chromotemp=10^5.99
 if strupcase(chianti_ion) eq 'O_6' then chromotemp=10^5.5
 if strupcase(chianti_ion) eq 'H_1' then chromotemp=10^4.25
;? from notes, Lya 4-5.5 logT
 ion_chromo=interpol(ion_fractions[*,iz-1,ionnum-1],ioneq_temp,alog10(chromotemp))

 frequency=2.9979d10/(wavelength*1d-8) ; units of s^-1
 h=6.6262d-27 ; ergs*s
 photon_energy = h*frequency ; ergs
 RSun_cm = 6.95700d+10  ;solar radius in cm

 normchromorad=chromorad*photon_energy ; units of ergs s^-1 cm^-2 sr^-1
 normchromorad=normchromorad/Rsun_cm ; units of ergs s^-1 cm^-3 sr^-1
; (note this is equivalent to units of emission of Stokes I in for_uvmodel)
;  chromorad is the line emission from sumer on the disk and varies with solar cycle and ion
; so now we have to back out the ion density associated with chromorad,
; assuming it dominated from collisions
; **note "chromorad" is a misnomer for lines like Ne8 - it should be thought of as
; ** line emission from plasma at the line formation temperature of the line
; **ignore collision and resonance scattering**
; **it is the boundary condition on the resonance scattering used in for_uvmodel
; **uniform billiard ball, with anisotropy based on height. By this assumption,
; **it lies below everything that has a coronal structure. This is probbly a good assumption for Ly-alpha
; **but we also add it onto the disk in for_integrate

 colldir=concat_dir(GET_ENVIRON('FORWARD'),'DEFAULTS')
 if strupcase(chianti_ion) eq 'O_6'and fix(wavelength) eq 1032 then collfile=concat_dir(colldir,'OVI1032_rates.txt')
 if strupcase(chianti_ion) eq 'O_6'and fix(wavelength) eq 1037 then collfile=concat_dir(colldir,'OVI1037_rates.txt')
 if strupcase(chianti_ion) eq 'H_1' then collfile=concat_dir(colldir,'LYA_rates.txt')
 if strupcase(chianti_ion) eq 'NE_8'and fix(wavelength) eq 770 then collfile=concat_dir(colldir,'NEVIII770_rates.txt')
 if strupcase(chianti_ion) eq 'NE_8'and fix(wavelength) eq 780 then collfile=concat_dir(colldir,'NEVIII780_rates.txt')
 if strupcase(chianti_ion) eq 'MG_9'and fix(wavelength) eq 706 then collfile=concat_dir(colldir,'MGIX706_rates.txt')

 if usewindows eq 1 then collfile=str_replace(collfile,'/','\')
      readcol,collfile,t,c,/silent
      crates_chromo=interpol(c,t,alog10(chromotemp)) 
   ; units of cm^3 s^-1
; ****CHECK THIS IS HOW TO DEAL WITH CHROMO RATES
; ****And whether element abundance for corona works for all of these

; from for_uvmodel:
; collemiss=photon_energy*0.83*[elabund*ionization*Dens]*Dens*crates*fill/(4.*!pi) 
;  where bracketed quantity is ion density
;  and we can then assume electron Dens =iondens/elabund/ionization
; and ignore filling factor 
; so collemiss=normchromorad=constant*iondens^2/elabund/ionization
; where:

 constant=photon_energy*.83*crates_chromo/4./!pi ; units of erg cm^3 s^-1 sr^-1

; normchromorad units are ergs s^-1 cm^-3 sr^-1

 if chromorad gt 0. then chromodens=sqrt(normchromorad*Element_abund*ion_chromo/constant) $
  else chromodens=0.
; units of cm^-3
;print,'chromorad=',chromorad
;print,'chromodens=',chromodens

endif else chromodens=0.


end


;**************************************************************************
