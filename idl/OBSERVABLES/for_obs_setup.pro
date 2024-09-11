;************************************************************************
;+
pro for_obs_setup,magmod,modelname,$
	all_inst=all_inst,all_betterinst=all_betterinst,all_types=all_types,all_lines=all_lines,all_names=all_names,all_nums=all_nums,all_labels=all_labels,all_defaults=all_defaults,phys_params=phys_params,phys_labels=phys_labels,model_params=model_params,model_labels=model_labels,pop2on=pop2on,working_dir=working_dir

;
;Name: FOR_OBS_SETUP
;
;Purpose: To set up structures containing all possible 
; observables and physical parameters and labelling etc.
;
; Called by FOR_OBS_CHOOSE, FOR_DRIVE, FOR_OBSDEFAULTS
;
;History:
; Written Sarah Gibson and Blake Forland
;
;29-Apr-2014 Added colden. TAK
;13-Jan-2015 - added pop2colden and pop2losem
;
; Version 2.0 July 2014
; 6-Feb-2016- Added POP2 LOSEM, COLDEN - 
;	also POP2 FILLFACT, DENS, TEMP to Physical Diagnostics
;	keyword POP2ON pass through
;       Added options THMOD,PHMOD,THOBS,PHOBS instead of THETA, PHI
;         these allow seeing angles in observer's frame (for POS
;         plot, these should always look like standard theta phi)
;         vs model, or absolute heliographic coordinate system
;         (these are rotated by Bang,Cmer,Thetao)
;            March 9 2016 SEG
;	Cleaned up conditionals, Added benergy, etc.
;		June 2018 SEG
;		June 2019 added Bhor
;		August 2020 added IONDENS 
;			for now that is only if have UV codes in WORKING
;			may generalize this later
;		Sept 2020 - don't display velocities for CAVMORPH and CROISSANT models
;		Oct 2020 -- added WL variables XPOLF,XPOLB, XPOLG
;		Nov 2020 -- added comment clarifying why SWSS is not in all_inst list (not implemented)
;		Nov 2020/Jan 2021 -- added hooks for TOMO 
;		Sep 2021 -- added hooks for DATA magmod 
;		Dec 2021 -- moved UVCODES into main FORWARD distribution
; January 2022 -- added TPOLF, TPOLB, TPOLG angular distance from TS functionality
;	March 2022 -- changed to TPOLU, TPOLS -- and commented out TPOLS for now
;		allowed density only magmod=2 -- for TURBHY
;	June 2022 -- changed magmod=2 to only be WL
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;	Nov 2022 -- commented out iondens because still being tested
;			fixed bug where indexing was wrong in PHYS_PARAM names
;	Dec 2022 -- added NEVIII
;       Jul 2023 -- uncommented iondens 
;		added as an option on EUV/UV SPECTROMETER
;	Sep 2023 -- added AWSOM
; 	Jan 2024 -- added OPEN
;       Feb 2024 -- added MGIX
;	Jun 2024 -- added BPOS column variables
;	Jul 2024 -- added hooks for CIRTOY
;			added PR
;	Aug 2024 -- added hooks for WLRAT

;
; check for UV in WORKING
;  --- changed to keep within main FORWWARD distribution
;

;if n_elements(working_dir) eq 1 then begin
;    checkuv=concat_dir(working_dir,'UVCODES')
;endif else checkuv='UVCODES'
; note this is not really needed any more because if the user has this code version
;  they should also have UVCODES directory set up. Leaving for now.
checkuv=get_environ('FORWARD')+'/OBSERVABLES/UVCODES'

;print,checkuv

if not keyword_set(pop2on) then pop2on=0

;
; first set up list of instruments that we always use
;


;all_inst_frontend=['Observables','xrt','trace','eit','euvia','euvib','aia','swap','eis','swss','cds','iris','myspect','iondens','kcor','wl']  

;note -- swss has not really been implemented so by removing it from this list will be removed from widget
; but it can be asked for on command line -- but will get an error

if magmod ne 2 then all_inst_frontend=['Observables','xrt','trace','eit','euvia','euvib','aia','swap','eis','cds','iris','myspect','iondens','kcor','wl']   $
  else all_inst_frontend=['Observables','wl']  

all_inst_middlemag=['comp','othercomp','fe11comp','si9comp','si10comp','cormag']  
;
; add UV if set

if file_test(checkuv) eq 1 then all_inst_middlemag=[all_inst_middlemag,'ovi1032','ovi1037','lya','neviii770','neviii780','mgix706']
all_inst_middlemag=[all_inst_middlemag,'faraday']
if magmod eq 0 or magmod eq -2 or magmod eq 2 then all_inst_middlemag=[]

if magmod ne 2 then all_inst_backend=['radio','losem','colden'] $
  else all_inst_backend=['losem','colden']

if abs(magmod) eq 1 then all_inst_backend=[all_inst_backend,'b_int','benergy','b_dens_int','ben_dens_int','b_pos_int','b_pos_dens_int']

;
; add pop2 if set

if pop2on eq 1 then all_inst_backend=[all_inst_backend,'pop2losem','pop2colden']


all_inst=[all_inst_frontend,all_inst_middlemag,all_inst_backend]

;
; get rid of awkward SI10COMP type names at least for widget
; also BENERGY, etc.
;

all_betterinst=all_inst
test=where(strupcase(all_inst) eq 'COMP')
if min(test) ne -1 then all_betterinst[test]='fe13_10747'
test=where(strupcase(all_inst) eq 'OTHERCOMP')
if min(test) ne -1 then all_betterinst[test]='fe13_10798'
test=where(strupcase(all_inst) eq 'FE11COMP')
if min(test) ne -1 then all_betterinst[test]='fe11_7892'
test=where(strupcase(all_inst) eq 'SI9COMP')
if min(test) ne -1 then all_betterinst[test]='si9_39267'
test=where(strupcase(all_inst) eq 'SI10COMP')
if min(test) ne -1 then all_betterinst[test]='si10_14302'
test=where(strupcase(all_inst) eq 'CORMAG')
if min(test) ne -1 then all_betterinst[test]='fe14_5303'
test=where(strupcase(all_inst) eq 'BENERGY')
if min(test) ne -1 then all_betterinst[test]='Column_magnetic_energy_density'
test=where(strupcase(all_inst) eq 'B_INT')
if min(test) ne -1 then all_betterinst[test]='Column_magnetic_magnitude'
test=where(strupcase(all_inst) eq 'B_DENS_INT')
if min(test) ne -1 then all_betterinst[test]='Column_magnetic_magnitude_times_density'
test=where(strupcase(all_inst) eq 'B_POS_INT')
if min(test) ne -1 then all_betterinst[test]='Column_magnetic_pos_magnitude'
test=where(strupcase(all_inst) eq 'B_POS_DENS_INT')
if min(test) ne -1 then all_betterinst[test]='Column_magnetic_pos_magnitude_times_density'
test=where(strupcase(all_inst) eq 'BEN_DENS_INT')
if min(test) ne -1 then all_betterinst[test]='Column_magnetic_energy_density_times_density'

; RADIO

if abs(magmod) eq 1 then begin
  radio_lines=['I','V','VoI']
  radio_names=['StokesI','StokesV','StokesVoI']
  radio_labels='Radio '
  radio_nums=[999,998,997]
endif else begin
  radio_lines=['I']
  radio_names=['StokesI']
  radio_labels='Radio '
  radio_nums=[999]
endelse
radio_type='Radio'
radio_default=0

; FARADAY ROTATION

if abs(magmod) eq 1 then begin
  faraday_lines=['RM','FR']
  faraday_names=['Rotation Measure','Rotation (angle)']
  faraday_labels='Faraday '
  faraday_nums=[999,998]
endif
faraday_type='Radio'
faraday_default=0

;LYA

if abs(magmod) eq 1 then begin
  lya_lines=['I','Q','U','L','QoI','UoI','LoI','Az']
  lya_names=lya_lines
  for i = 0,n_elements(lya_lines)-1 do $
    lya_names[i]='Stokes'+lya_lines[i]
  lya_labels='Ly-alpha Stokes '
  lya_nums=999-intarr(n_elements(lya_lines))
endif else begin
  lya_lines=['I']
  lya_names=['StokesI']
  lya_labels='Ly-alpha Stokes '
  lya_nums=[999]
endelse
lya_type='UV Spectropolarimeters'
lya_default=0

;NEVIII770

if abs(magmod) eq 1 then begin
  neviii770_lines=['I','Q','U','L','QoI','UoI','LoI','Az']
  neviii770_names=neviii770_lines
  for i = 0,n_elements(neviii770_lines)-1 do $
    neviii770_names[i]='Stokes'+neviii770_lines[i]
  neviii770_labels='NEVIII 770 Stokes '
  neviii770_nums=999-intarr(n_elements(neviii770_lines))
endif else begin
  neviii770_lines=['I']
  neviii770_names=['StokesI']
  neviii770_labels='NEVIII 770Stokes '
  neviii770_nums=[999]
endelse
neviii770_type='UV Spectropolarimeters'
neviii770_default=0

;NEVIII780

if abs(magmod) eq 1 then begin
;  neviii780_lines=['I','Q','U','L','QoI','UoI','LoI','Az']
  neviii780_lines=['I']
  neviii780_names=neviii780_lines
  for i = 0,n_elements(neviii780_lines)-1 do $
    neviii780_names[i]='Stokes'+neviii780_lines[i]
  neviii780_labels='NEVIII 780 Stokes '
  neviii780_nums=999-intarr(n_elements(neviii780_lines))
endif else begin
  neviii780_lines=['I']
  neviii780_names=['StokesI']
  neviii780_labels='NEVIII 780Stokes '
  neviii780_nums=[999]
endelse
neviii780_type='UV Spectropolarimeters'
neviii780_default=0

;MGIX706

if abs(magmod) eq 1 then begin
  mgix706_lines=['I','Q','U','L','QoI','UoI','LoI','Az']
  mgix706_names=mgix706_lines
  for i = 0,n_elements(mgix706_lines)-1 do $
     mgix706_names[i]='Stokes'+mgix706_lines[i]
  mgix706_labels='MGIX 706 Stokes '
  mgix706_nums=999-intarr(n_elements(mgix706_lines))
endif else begin
  mgix706_lines=['I']
  mgix706_names=['StokesI']
  mgix706_labels='MGIX 706Stokes '
  mgix706_nums=[999]
endelse
mgix706_type='UV Spectropolarimeters'
mgix706_default=0


;OVI1037 

if abs(magmod) eq 1 then begin
;  ovi1037_lines=['I','Q','U','L','QoI','UoI','LoI','Az']
  ovi1037_lines=['I']
  ovi1037_names=ovi1037_lines
  for i = 0,n_elements(ovi1037_lines)-1 do $
    ovi1037_names[i]='Stokes'+ovi1037_lines[i]
  ovi1037_labels='OVI 1037 Stokes '
  ovi1037_nums=999-intarr(n_elements(ovi1037_lines))
endif else begin
  ovi1037_lines=['I']
  ovi1037_names=['StokesI']
  ovi1037_labels='OVI 1037 Stokes '
  ovi1037_nums=[999]
endelse
ovi1037_type='UV Spectropolarimeters'
ovi1037_default=0

;OVI1032 

if abs(magmod) eq 1 then begin
  ovi1032_lines=['I','Q','U','L','QoI','UoI','LoI','Az']
  ovi1032_names=ovi1032_lines
  for i = 0,n_elements(ovi1032_lines)-1 do $
    ovi1032_names[i]='Stokes'+ovi1032_lines[i]
  ovi1032_labels='OVI 1032 Stokes '
  ovi1032_nums=999-intarr(n_elements(ovi1032_lines))
endif else begin
  ovi1032_lines=['I']
  ovi1032_names=['StokesI']
  ovi1032_labels='OVI 1032 Stokes '
  ovi1032_nums=[999]
endelse
ovi1032_type='UV Spectropolarimeters'
ovi1032_default=0

; XRT

xrt_lines = ['almesh','alpoly','cpoly','tipoly','bethin','bemed','almed','althick','bethick','alpolyalmesh','alpolytipoly','alpolyalthick','alpolybethick','cpolytipoly','cpolyalthick']
xrt_names=['Al-mesh','Al-poly','C-poly','Ti-poly','Be-thin',$
           'Be-med','Al-med','Al-thick','Be-thick','Al-poly/Al-mesh',$
           'Al-poly/Ti-poly','Al-poly/Al-thick','Al-poly/Be-thick',$
           'C-poly/Ti-poly','C-poly/Al-thick']
xrt_nums=findgen(n_elements(xrt_names))
xrt_labels='Hinode/XRT '
xrt_type='EUV/Xray Imagers'
xrt_default=4

; TRACE

trace_lines = ['171','195','284']
trace_names = ['171 A','195 A','284 A']
trace_nums = [171,195,284]
trace_labels='TRACE '
trace_type='EUV/Xray Imagers'
trace_default=1

; EIT

eit_lines = ['171','195','284','304']
eit_names = ['171 A','195 A','284 A','304 A']
eit_nums = [171,195,284,304]
eit_labels='SOHO/EIT '
eit_type='EUV/Xray Imagers'
eit_default=1

; EUVIA

euvia_lines = ['304','171','195','284']
euvia_names = ['304 A','171 A','195 A','284 A']
euvia_nums = [304,171,195,284]
euvia_labels='STEREO/EUVIA '
euvia_type='EUV/Xray Imagers'
euvia_default=2

; EUVIB

euvib_lines = ['304','171','195','284']
euvib_names = ['304 A','171 A','195 A','284 A']
euvib_nums = [304,171,195,284]
euvib_labels='STEREO/EUVIB '
euvib_type='EUV/Xray Imagers'
euvib_default=2

; AIA

aia_lines = ['94','131','171','193','211','304','335']
aia_names = ['94 A','131 A','171 A','193 A','211 A','304 A','335 A']
aia_nums = [94,131,171,193,211,304,335]
aia_labels='SDO/AIA '
aia_type='EUV/Xray Imagers'
aia_default=3

; SWAP

swap_lines = ['174']
swap_names = ['174 A']
swap_nums = [174]
swap_labels=['PROBA2/SWAP ']
swap_type='EUV/Xray Imagers'
swap_default=0

; EIS


;elem=['h','he','c','n','o','ne','na','mg','al', 'si','p','s','cl','ar','k','ca','ti','cr','mn','fe','co','ni','zn']
;ions=            [1,   2,  3,     4,  5,  8,          8,      10,   11,    8,      11,    11,     12,    13,    14 ,10,12,13,9,12,17,12,20]
;waves=[ 1215.67,303.78,1175.74,765.15,629.73,770.42,789.78, 624.94,550.03,319.839,317.181 ,285.588,202.571,248.697,$
;                   175.704,574.011,349.9300, 267.731,191.63,195.119,247.5410 ,152.15,256.37]
;
; removed H and He because optically thick

elem=['c','n','o','ne','na','mg','al','si','p','s','cl','ar','k','ca','ti','cr','mn','fe','co','ni','zn']

ions= [3,4,5,8,8,10,11,4,11,11,12,13,14,10,12,13,9,12,17,12,20]
waves=[1175.74,765.15,629.73,770.42,789.78, 624.94,550.03,1393.8,317.181 ,285.588,202.571,248.697,$
                   175.704,574.011,349.9300, 267.731,191.63,195.119,247.5410,152.15,256.37]

; 
; note I did not include 'li','be','b' 'v' 'f' 'sc' and 'cu'  because I don't see lines in Version 7.1.3 Chianti
; http://www.chiantidatabase.org/chianti_direct_data.html
;

eis_lines=strarr(n_elements(elem))
for i = 0,n_elements(elem)-1 do eis_lines[i]=strupcase(elem[i])+strtrim(string(ions[i]),2)+'_'+strtrim(string(waves[i]),2)
eis_names=strupcase(elem)
eis_nums=findgen(n_elements(eis_names))

eis_labels='Hinode/EIS '
eis_type='UV/EUV Spectrometers'
; make Fe default
eis_default=19

; CDS

cds_lines=strarr(n_elements(elem))
for i = 0,n_elements(elem)-1 do cds_lines[i]=strupcase(elem[i])+strtrim(string(ions[i]),2)+'_'+strtrim(string(waves[i]),2)
cds_names=strupcase(elem)
cds_nums=findgen(n_elements(cds_names))

cds_labels='SOHO/CDS '
cds_type='UV/EUV Spectrometers'
cds_default=2

; IRIS

iris_lines=strarr(n_elements(elem))
for i = 0,n_elements(elem)-1 do iris_lines[i]=strupcase(elem[i])+strtrim(string(ions[i]),2)+'_'+strtrim(string(waves[i]),2)
iris_names=strupcase(elem)
iris_nums=findgen(n_elements(iris_names))

iris_labels='IRIS--not slitjaw '
iris_type='UV/EUV Spectrometers'
iris_default=7

; MYSPECT

myspect_lines=strarr(n_elements(elem))
for i = 0,n_elements(elem)-1 do myspect_lines[i]=strupcase(elem[i])+strtrim(string(ions[i]),2)+'_'+strtrim(string(waves[i]),2)
myspect_names=strupcase(elem)
myspect_nums=findgen(n_elements(myspect_names))

myspect_labels='Custom spectrometer '
myspect_type='UV/EUV Spectrometers'
myspect_default=19

; IONDENS

ion_elem=['h','he','c','n','o','ne','na','mg','al', 'si','p','s','cl','ar','k','ca','ti','cr','mn','fe','co','ni','zn']
ion_ions=            [1,   2,  3,     4,  5,  8,          8,      10,   11,    8,      11,    11,     12,    13,    14 ,10,12,13,9,12,17,12,20]
ion_waves=[ 1215.67,303.78,1175.74,765.15,629.73,770.42,789.78, 624.94,550.03,319.839,317.181 ,285.588,202.571,248.697,$
                   175.704,574.011,349.9300, 267.731,191.63,195.119,247.5410 ,152.15,256.37]

iondens_lines=strarr(n_elements(ion_elem))
;for i = 0,n_elements(ion_elem)-1 do iondens_lines[i]='IONDENS_'+strupcase(ion_elem[i])+strtrim(string(ion_ions[i]),2)+'_'+strtrim(string(ion_waves[i]),2)
for i = 0,n_elements(ion_elem)-1 do iondens_lines[i]=strupcase(ion_elem[i])+strtrim(string(ion_ions[i]),2)+'_'+strtrim(string(ion_waves[i]),2)
;iondens_names='IONDENS_'+strupcase(ion_elem)
iondens_names=strupcase(ion_elem)
iondens_nums=findgen(n_elements(iondens_names))

iondens_labels='Ion Density ***TESTING***'
iondens_type='UV/EUV Spectrometers'
iondens_default=5

; SWSS

SWSS_Elements=[replicate('Fe',5),'Ni']
SWSS_IonNum=[9,10,11,13,14,15]
swss_lines=SWSS_Elements+trim(SWSS_IonNum)
swss_names=swss_lines
swss_nums=[4359,6374,7892,10747,5303,6702.]
swss_labels='U Hawaii/SWSS '
;
; we need the ion number not the wavelength number for check
;
swss_nums=SWSS_IonNum
swss_type='Visible Spectrometers'
swss_default=5

; WL

;wl_lines = ['PB','TB','P','PR','XPOLF','XPOLB','XPOLG','TPOLU','TPOLS','TPOLG']
;wl_nums=[-999,-999,-999,-999,-999,-999,-999,-999,-999,-999]

; check for CODEX in working directory
;
 
if n_elements(working_dir) eq 1 then begin
            checkcodex=concat_dir(working_dir,'CODEX')
endif else checkcodex='CODEX'

if file_test(checkcodex) ne 0 then begin
 wl_lines = ['PB','TB','P','PR','XPOLF','XPOLB','XPOLG','TPOLU','TPOLG','WLRAT']
 wl_nums=[-999,-999,-999,-999,-999,-999,-999,-999,-999,-999]
endif else begin
 wl_lines = ['PB','TB','P','PR','XPOLF','XPOLB','XPOLG','TPOLU','TPOLG']
 wl_nums=[-999,-999,-999,-999,-999,-999,-999,-999,-999]
endelse
wl_names = wl_lines
wl_labels='White light '
wl_type='White light coronagraphs'
wl_default=0

; CORMAG

cormag_lines=['I','Q','U','V','L','QoI','UoI','LoI','VoI','Vlos','LineWidth','Az']
cormag_nums=intarr(n_elements(cormag_lines))-999
cormag_names = 'Stokes'+cormag_lines
cormag_labels='Green line Stokes '
cormag_type='Coronal polarimeters'
cormag_default=0

; KCOR

kcor_lines = ['PB']
kcor_nums=[-999]
kcor_names = kcor_lines
kcor_labels='White light '
kcor_type='White light coronagraphs'
kcor_default=0

; LOSEM

losem_lines = ['LOSEM']
losem_nums=[-999]
losem_names = ['LOSEM']
losem_labels = 'cm^-5 '
losem_type='Line-of-sight integrated diagnostics'
losem_default=0

; COLDEN

colden_lines = ['COLDEN']
colden_nums=[-999]
colden_names = ['COLDEN']
colden_labels = 'cm^-2 '
colden_type='Line-of-sight integrated diagnostics'
colden_default=0

; B_DENS_INT

b_dens_int_lines = ['B_DENS_INT']
b_dens_int_nums=[-999]
b_dens_int_names = ['B_DENS_INT']
b_dens_int_labels = 'Gauss*cm^-2 '
b_dens_int_type='Line-of-sight integrated diagnostics'
b_dens_int_default=0

; B_INT

b_int_lines = ['B_INT']
b_int_nums=[-999]
b_int_names = ['B_INT']
b_int_labels = 'Gauss*cm '
b_int_type='Line-of-sight integrated diagnostics'
b_int_default=0

; B_POS_DENS_INT

b_pos_dens_int_lines = ['B_POS_DENS_INT']
b_pos_dens_int_nums=[-999]
b_pos_dens_int_names = ['B_POS_DENS_INT']
b_pos_dens_int_labels = 'Gauss*cm^-2 '
b_pos_dens_int_type='Line-of-sight integrated diagnostics'
b_pos_dens_int_default=0

; B_POS_INT

b_pos_int_lines = ['B_POS_INT']
b_pos_int_nums=[-999]
b_pos_int_names = ['B_POS_INT']
b_pos_int_labels = 'Gauss*cm '
b_pos_int_type='Line-of-sight integrated diagnostics'
b_pos_int_default=0

; BENERGY

benergy_lines = ['BENERGY']
benergy_nums=[-999]
benergy_names = ['BENERGY']
benergy_labels = 'Gauss^2*cm '
benergy_type='Line-of-sight integrated diagnostics'
benergy_default=0

; BEN_DENS_INT

ben_dens_int_lines = ['BEN_DENS_INT']
ben_dens_int_nums=[-999]
ben_dens_int_names = ['BEN_DENS_INT']
ben_dens_int_labels = 'Gauss^2*cm^-2 '
ben_dens_int_type='Line-of-sight integrated diagnostics'
ben_dens_int_default=0

; POP2LOSEM

pop2losem_lines = ['POP2LOSEM']
pop2losem_nums=[-999]
pop2losem_names = ['POP2LOSEM']
pop2losem_labels = 'cm^-5 '
pop2losem_type='Line-of-sight integrated diagnostics'
pop2losem_default=0

; POP2COLDEN

pop2colden_lines = ['POP2COLDEN']
pop2colden_nums=[-999]
pop2colden_names = ['POP2COLDEN']
pop2colden_labels = 'cm^-2 '
pop2colden_type='Line-of-sight integrated diagnostics'
pop2colden_default=0

; COMP

comp_lines=['I','Q','U','V','L','QoI','UoI','LoI','VoI','Vlos','LineWidth','Az']
othercomp_lines=['I','Q','U','V','L','QoI','UoI','LoI','VoI','Vlos','LineWidth','Az']
fe11comp_lines=['I','Q','U','V','L','QoI','UoI','LoI','VoI','Vlos','LineWidth','Az']
si9comp_lines=['I','Q','U','V','L','QoI','UoI','LoI','VoI','Vlos','LineWidth','Az']
si10comp_lines=['I','Q','U','V','L','QoI','UoI','LoI','VoI','Vlos','LineWidth','Az']
;greencomp_lines=['I','Q','U','V','L','QoI','UoI','LoI','VoI','Vlos','LineWidth','Az']
wavecomp_lines=['I','Q','U','V','L','QoI','UoI','LoI','VoI','Vlos','LineWidth','Az']
comp_nums=intarr(n_elements(comp_lines))-999
othercomp_nums=intarr(n_elements(comp_lines))-999
fe11comp_nums=intarr(n_elements(fe11comp_lines))-999
si9comp_nums=intarr(n_elements(si9comp_lines))-999
si10comp_nums=intarr(n_elements(si10comp_lines))-999
wavecomp_nums=intarr(n_elements(wavecomp_lines))-999
;greencomp_nums=intarr(n_elements(greencomp_lines))-999
comp_names=comp_lines
othercomp_names=comp_lines
fe11comp_names=comp_lines
si9comp_names=comp_lines
si10comp_names=comp_lines
;greencomp_names=comp_lines
wavecomp_names=wavecomp_lines
comp_labels='10747_Stokes '
othercomp_labels='10798_Stokes '
fe11comp_labels='FeXI_Stokes '
si9comp_labels='SiIX_Stokes '
si10comp_labels='SiX_Stokes '
;greencomp_labels='Green_Stokes '
wavecomp_labels='Wavelengths_Stokes '
for i = 0,n_elements(comp_lines)-1 do begin
  comp_names[i]='Stokes'+comp_lines[i]
  othercomp_names[i]='Stokes'+comp_lines[i]
  fe11comp_names[i]='Stokes'+comp_lines[i]
  si9comp_names[i]='Stokes'+comp_lines[i]
  si10comp_names[i]='Stokes'+comp_lines[i]
  ;greencomp_names[i]='Stokes'+comp_lines[i]
  wavecomp_names[i]='Stokes'+comp_lines[i]
endfor
comp_names[i-3]='Doppler'+comp_lines[i-3]
comp_names[i-2]=comp_lines[i-2]
othercomp_names[i-3]='Doppler'+othercomp_lines[i-3]
;greencomp_names[i-3]='Doppler'+greencomp_lines[i-3]
fe11comp_names[i-3]='Doppler'+fe11comp_lines[i-3]
si9comp_names[i-3]='Doppler'+si9comp_lines[i-3]
si10comp_names[i-3]='Doppler'+si10comp_lines[i-3]
wavecomp_names[i-3]='Doppler'+wavecomp_lines[i-3]
othercomp_names[i-2]=othercomp_lines[i-2]
;greencomp_names[i-2]=greencomp_lines[i-2]
fe11comp_names[i-2]=fe11comp_lines[i-2]
si9comp_names[i-2]=si9comp_lines[i-2]
si10comp_names[i-2]=si10comp_lines[i-2]
wavecomp_names[i-2]=wavecomp_lines[i-2]
ucomp_type='Coronal polarimeters'
comp_type='Coronal polarimeters'
othercomp_type='Coronal polarimeters'
fe11comp_type='Coronal polarimeters'
si9comp_type='Coronal polarimeters'
si10comp_type='Coronal polarimeters'
;greencomp_type='Coronal polarimeters'
wavecomp_type='Coronal polarimeters'
comp_default=0
othercomp_default=0
fe11comp_default=0
si9comp_default=0
si10comp_default=0
;greencomp_default=0
wavecomp_default=0

create_struct,all_lines,'',['dummytag'],'d2'
create_struct,all_names,'',['dummytag'],'d2'
create_struct,all_nums,'',['dummytag'],'d2'
create_struct,all_labels,'',['dummytag'],'d2'
create_struct,all_types,'',['dummytag'],'d2'
create_struct,all_defaults,'',['dummytag'],'d2'

for i = 1,n_elements(all_inst)-1 do begin

  instlines=all_inst[i]+'_lines'
  instnames=all_inst[i]+'_names'
  instnums=all_inst[i]+'_nums'
  instlabels=all_inst[i]+'_labels'
  insttype=all_inst[i]+'_type'
  instdefault=all_inst[i]+'_default'
  try=execute('temp = '+instlines)
  all_lines=add_tag(all_lines,temp,instlines)
  try=execute('temp = '+instnames)
  all_names=add_tag(all_names,temp,instnames)
  try=execute('temp = '+instnums)
  all_nums=add_tag(all_nums,temp,instnums)
  try=execute('temp = '+instlabels)
  all_labels=add_tag(all_labels,temp,instlabels)
  try=execute('temp = '+insttype)
  all_types=add_tag(all_types,temp,insttype)
  try=execute('temp = '+instdefault)
  all_defaults=add_tag(all_defaults,temp,instdefault)

endfor
all_lines=rem_tag(all_lines,'dummytag')
all_names=rem_tag(all_names,'dummytag')
all_nums=rem_tag(all_nums,'dummytag')
all_labels=rem_tag(all_labels,'dummytag')
all_types=rem_tag(all_types,'dummytag')
all_defaults=rem_tag(all_defaults,'dummytag')

;
; first set up list of physical parameters that we always use
;

if magmod ne 2 then begin
 phys_params_frontend = ['Physical Diagnostics','dens','temp','pres','vx','vy','vz','vr','vth','vph']
 phys_labels_frontend = ['Density (cm^-3)','Temperature (Degrees Kelvin)','Thermal Pressure (dyne cm^-2)','Vx (km/sec)','Vy (km/sec)','Vz (km/sec)','Vr (km/sec)','Vth (km/sec)','Vph (km/sec)']
endif else begin
 phys_params_frontend = ['Physical Diagnostics','dens']
 phys_labels_frontend = ['Density (cm^-3)']
endelse

if strupcase(modelname) eq 'CAVMORPH' or strupcase(modelname) eq 'CROISSANT' or strupcase(modelname) eq 'CIRTOY' then begin
 phys_params_frontend = ['Physical Diagnostics','dens','temp','pres'] 
 phys_labels_frontend = ['Density (cm^-3)','Temperature (Degrees Kelvin)','Thermal Pressure (dyne cm^-2)']
endif

if strupcase(modelname) eq 'TOMO' then begin
 phys_params_frontend = ['Physical Diagnostics','dens','temp','pres','vr','vx','vy','vz'] 
 phys_labels_frontend = ['Density (cm^-3)','Temperature (Degrees Kelvin)','Thermal Pressure (dyne cm^-2)','Vr (km/sec)','Vx (km/sec)','Vy (km/sec)', 'Vz (km/sec)']
endif

phys_params_mag = ['br','bth','bph','bx','by','bz','bmag','bpos','bhor','bxdivbmag','ptot','beta','open'] 
phys_labels_mag = ['Br (Gauss)','Bth (Gauss)','Bph (Gauss)','Bx (Gauss)','By (Gauss)','Bz (Gauss)','Bmag (Gauss)','B plane-of-sky (Gauss)','Bhorizontal (Gauss)','Bx/Bmag',$
'Total Pressure (dyne cm^-2)','Plasma Beta','1=open,0=closed field']

if strupcase(modelname) eq 'NUMCUBE' or strupcase(modelname) eq 'ADAPTCUBE' or strupcase(modelname) eq 'PSIMAS' or strupcase(modelname) eq 'AWSOM' or strupcase(modelname) eq 'PFSSMOD' then begin
 phys_params_mag=[phys_params_mag,'expfac','ninst']
 phys_labels_mag=[phys_labels_mag,'Expansion Factor','radial magnetic dropoff index']
endif

if strupcase(modelname) eq 'GIBBAGLOW' then begin
 phys_params_mag=[phys_params_mag,'Stream']
 phys_labels_mag=[phys_labels_mag,'Stream function']
endif

phys_params_backend = ['r','thmod','phmod','thobs','phobs'] 
phys_labels_backend=['R (Rsun)','Theta model (degrees)','Phi model (degrees)','Theta observer (degrees)','Phi observer (degrees)']

phys_params_pop2=[]
phys_labels_pop2=[]

if strupcase(modelname) eq 'NUMCUBE' or strupcase(modelname) eq 'ADAPTCUBE' or strupcase(modelname) eq 'PSIMAS' or strupcase(modelname) eq 'AWSOM' or strupcase(modelname) eq 'CAVMORPH' then begin
  phys_params_backend=['fillfact',phys_params_backend]
  phys_labels_backend=['Filling Factor',phys_labels_backend]
  phys_params_pop2 = ['pop2dens','pop2temp','pop2fillfact']
  phys_labels_pop2 = ['Population 2 Density (cm^-3)','Population 2 Temperature (Degrees Kelvin)','Population 2 Filling Factor']
endif

phys_params_ion=[]
phys_labels_ion=[]
;
; this is now inconsistent with the way we treat
; iondens-- by choosing ion for model dens
; note if sim, will check to see if there is a preset iondens
; and use that. Just in case there is some other aspect of ions
; from sim we want to use (temperature, velocity), leaving this in 
; with an empty array
;
;if file_test(checkuv) eq 1 then begin
; phys_params_ion=['iondens']
; phys_labels_ion=['Ion Density (cm^-3)']
;endif

if magmod eq 0 or magmod eq 2 or magmod eq -2 then begin
 phys_params_mag=[]
 phys_labels_mag=[]
endif

phys_params = [phys_params_frontend,phys_params_ion,phys_params_mag,phys_params_pop2,phys_params_backend]
phys_labels = [phys_labels_frontend,phys_labels_ion,phys_labels_mag,phys_labels_pop2,phys_labels_backend]

model_params = ['Model Diagnostics','cavheight','cavwidth','cavtopr','cavtopt','thcs','gamma']
model_labels=['CAVHEIGHT (RSUN)', 'CAVWIDTH (RSUN)','CAVTOPR (RSUN)','CAVTOP ANGLE (DEGREES)','CENTRAL STREAMER/CAVITY ANGLE (DEGREES)','CAVITY TILT ANGLE (DEGREES)']

end

