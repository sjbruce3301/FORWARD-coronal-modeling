;  
;  written/modified by Stephen White - 
;	updated Sarah Gibson, Patrick McCauley
;
; Version 1.0 April 2015 revised November 2015
;		revised December 2018
;	Oct 2023- updated Rsun
;---------------------------------------------------------------------------

function for_radiomodel, ObsPramsStruct,dlos,$  
                           r3D,theta3D,phi3D,Brobs,Bthobs,Bphobs,$  
                           Densobs,Tempobs,freq=freq, taur=taur, taul=taul
  
;
; Called by FOR_RADIOCALC
;
; Calls FOR_GYRORES, FOR_COLDPL
;

; inputs should all be double, 
;  cgs units so freq in Hz  
;
;  returns 2 x npos array with I,V
;
; optional supply of freq for testing  

if not keyword_set(freq) then freq=double(ObsPramsStruct.Frequency_MHz)*1d6  
 
Rsun2cm=6.957d10

; /DOGYRO does gyrores calculation

dogyro=ObsPramsStruct.dogyro

; print,dogyro

; get dimensions  

npos = n_elements(r3D[0,*])  
nlos = n_elements(r3D[*,0])  
iv = dblarr(2,npos)
  
;radio_debug=dblarr(nlos,npos)

; line-of-sight component of B should be Bx - ? where x is sin(thet)cos(phi)  
; for spherical bx=br*sth*cph+bth*cth*cph+bph*sph,  
; by=br*sth*sph+bth*sth*sph+bph*cph, bz=br*cth-bth*sth  
; generate array of x coords  

x3d=r3d*sin(theta3d)*cos(phi3d)  

; generate arrays of btot, blos: los is in first dimension 

btot=sqrt(reform(brobs[*,*])^2 + reform(bthobs[*,*])^2 + reform(bphobs[*,*])^2)
blos = sin(theta3d[*,*])*cos(phi3d[*,*])*brobs[*,*]+$
       cos(theta3d[*,*])*cos(phi3d[*,*])*bthobs[*,*]-$
       sin(phi3d[*,*])*bphobs[*,*]

; arrays of opacity and brightness temperature to be filled in by
; integration from behind the Sun forwards, allow R, L for x,o modes

taur=dblarr(npos)*0.0d0 & taul=taur

; TB starts at 3 K cosmic background

tbr=taur+3.0d0 & tbl=tbr

; radiative transfer as discrete steps 
; only do if density less than threshold based on frequency

dens_thres = for_freq_to_dens(freq) ;density corresponding to plasma frequency
   
;densities <= plasma frequency contribute
;so we will set up a new plasma temperature array
;which is set to 6000 K for all points along a line of sight
;that lie behind high density 

tempobs_fixed=tempobs

for k=0L,npos-1L do begin
 lowdens=where(densobs[*,k] le dens_thres, complement=highdens)
 if max(highdens) ne -1 then tempobs_fixed[0:max(highdens),k]=6000.d0
endfor

; parameters only needed for gyroresonance
if keyword_set(dogyro) then begin
   btheta=acos(blos/btot)*180./!dpi
   ; need scale length of Blos: Blos/(dBlos/dlos[*,*])
   lenlos = blos
   ; convert solar radius to cm for calculation
   for k=0L,npos-1L do lenlos[*,k] = $
      abs(blos[*,k]/deriv(x3d[*,k],blos[*,k])) * Rsun2cm
   ; identify pixels containing harmonic layers
   bg = freq/2.8d6    ; B field corresp to obs frequency in Hz
   bharm = fix(bg/btot) ; reduces B to harmonic, now find where it changes
   harmpix = bharm*0
   ; hope that harmonic never changes by more than 1, can't assume it
   ; always decreases
   harmpix[1L:nlos-1L,*]=abs(bharm[1L:nlos-1L,*]-bharm[0L:nlos-2L,*])<1
   ncross=0L
endif

; progress along lines of sight one step at a time

; for j=nlos-1,0,-1 do begin
for j=0,nlos-1 do begin
   fac=2.8d6*reform(blos[j,*])/freq
   dtau=0.2d0*(reform(densobs[j,*])^2)*(reform(tempobs_fixed[j,*])^(-1.5d0))/(freq^2)*(dlos[j,*]*Rsun2cm)
   dtaur=dtau/(1.0d0-fac)^2
   dtaul=dtau/(1.0d0+fac)^2
   ; fac=2.d0*2.8d6*reform(blos[j,*])/freq
   ; dtau=0.2d0*(reform(densobs[j,*])^2)*(reform(tempobs_fixed[j,*])^(-1.5d0))/(freq^2)*(dlos[j,*]*Rsun2cm)
   ; dtaur=dtau*(1.0d0+fac)
   ; dtaul=dtau*(1.0d0-fac)
   taur=taur+dtaur
   taul=taul+dtaul

   if keyword_set(dogyro) then begin
      ; gyroresonance calculation: multiply everything by harmpix which
      ; should be 1 in harmonic pixels, 0 otherwise
      ; means we do calculations we don't need - waste of time?
      ; yes because we have to run cold plasma for each one
      hx = where((harmpix[j,*] ne 0) and (bharm[j,*] lt 6) and (bharm[j,*] gt 0), nhx)
      ; gyrores not currently vectorizable
      if (nhx gt 0) then begin
         ; print,bharm[j,hx]
         ncross = ncross + nhx
         for hh=0,nhx-1 do begin
            for_gyrores,f=freq,n=reform(densobs[j,hx[hh]]),temp=reform(tempobs_fixed[j,hx[hh]]),theta=reform(btheta[j,hx[hh]]),l=reform(lenlos[j,hx[hh]]),s=reform(bharm[j,hx[hh]]),taux=tx,tauo=to
            ; make sure RCP matches x mode if Blos>0., if not switch
            if (blos[j,hx[hh]] lt 0.0) then begin
               tx1=to & to=tx & tx=tx1
            endif
            taur[hx[hh]] = taur[hx[hh]] + tx
            taul[hx[hh]] = taul[hx[hh]] + to
            dtaur[hx[hh]] = dtaur[hx[hh]] + tx
            dtaul[hx[hh]] = dtaul[hx[hh]] + to
            ; print,bharm[j,hx[hh]],tx,to
         end
      endif
   endif

   etau=exp(-dtaur)
   tbr=tbr*etau+reform(tempobs_fixed[j,*])*(1.d0-etau) 
   etau=exp(-dtaul)
   tbl=tbl*etau+reform(tempobs_fixed[j,*])*(1.d0-etau) 
   
;   radio_debug[j,*] = tbr

endfor

if keyword_set(dogyro) then begin
   print,'-----------------------------------------------------------'
   if (ncross eq 0) then print,'Note: no harmonic layer crossings' $
   else $
   print,'Processed '+strtrim(string(ncross),2)+' harmonic-pixel crossings.'
   print,'-----------------------------------------------------------'
endif

; brightness temperature maps 
iv[0,*] = (tbr + tbl)/2. 
iv[1,*] = (tbr - tbl)/2. 

; save,filename='idlsave.radiovar',/var

return, iv  

end  
