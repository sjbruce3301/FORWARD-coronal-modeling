function for_eisratio_forward,densin,dlos,linenum

; Note, this is a much faster routine
; which is optimized for line ratio, where
; temperature sensitivity should drop out,
; thus allowing us to use one lookup table
;
; the lookup table is made using emiss_calc
; for temperature 6.1, abundances coronal
; ionization equilibrium standard chianti
;
; for comparison to for_eis (diagnostic purposes)
; we allow the option of returning 195 
; but note it is important then to force a temperature of 6.1
;
; Called by FOR_INTENSINT for special cases FE12DE, FE12RATIO
; NOT OPTIONS IN WIDGET - COMMAND LINE ONLY
; 
; Written by Don Schmit, Modified by Terry Kucera, Sarah Gibson
;
;  Version 2.0 July 2014
;
;   Jun 1 2019  Removed usewindows, used slash for PC compatibility
;   Oct 2023 - updated Rsun

Rsun_cm= 6.95700d+10

;
; test for PC
;

slash=path_sep()

usedir=file_dirname(GET_ENVIRON('FORWARD'))+slash+file_basename(GET_ENVIRON('FORWARD'))+slash+'OBSERVABLES'+slash+'emissiv_ratio.sav')
restore,usedir

; lookup table of density vs emission
;it contains : dens --log(cm^-3)[-4,10], em195 -- erg s^-1 cm^3 (total
;        of CHIANTI lines 873-875), em186 --ditto (total of lines
;        749-750)
;We can use these emissivities for interpolation
;To go from alog10(ModSolStruct.dens) to 1d use

dims=size(densin)
len=dims[1]*long(dims[2])
long_1d=reform(densin,len,1)

em_195=reform(interpol(em195, 10.^dens, long_1d),dims[1], dims[2])*dlos*Rsun_cm*densin

em_186=reform(interpol(em186, 10.^dens, long_1d),dims[1], dims[2])*dlos*Rsun_cm*densin

f195=total(em_195,1)
f186=total(em_186,1)
ratio=f186/f195

if linenum eq 16 then return,f195
if linenum eq 17 then return,ratio

end

