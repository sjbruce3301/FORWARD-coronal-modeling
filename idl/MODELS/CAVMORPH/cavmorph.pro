;**********************************************************************
pro cavmorph,r,th,ph,ModPramsSTruct,ModSolStruct

common pramsstream,thcs,phcs,alpha,m,awidth,bwidth,wmult
common cavprop,semmin,semmaj,ziti

;+
;Name: CAVMORPH
;
;Purpose:  Global sun with streamer(s) and cavity(s)
; 		morphological model (not a physical solution)
;		specifies a density and temperature distribution in 3D
;  		that can be fit to observables
;
; Inputs:
;          r, th, ph -- position in 3D space where model is to be evaluated
;			r in units of RSUN, th ph in RADIANS
;
;          ModPramsStruct - structure associated with model, containing
;                           model name (CAVMORPH), model parameters
;			    set up in cavmorphprams.pro
;
; Outputs: ModSolStruct - Solution of model, containing density and temperature
;
; Comments: The streamer and cavity will be centered on thcs,phcs
;		default will be thcs=90, phcs=90, equator West limb when cmer=0
;	
;
; Common Blocks: pramstemp, pramsdens, pramsstream, pramscav
;
; Calls: pivot, getcav, getbound
;
; Written: S. Gibson
; Modifications: 
;	17-Dec-2009 added keywords.  TAK
;	9-Feb-2010 adapted for FORWARD tree structure.  SEG
;	13-May-2010 modified for cavity output  SEG
;	20-May-2010 modified for phcs and incorporating Don's changes. SEG
;	 	modified for POWER/EXPONENT/EXPDEL choices of radial functions
;	??	added NEWKIRK option SEG
;	10-09-10 added NOUGAT option SEG
;	12-Dec-2010	added ZITI option SEG
;	28-Feb-2011 added TCPar and TRPar temperature parameters for cavity and streamer TAK
;	8-Mar-2011 Temperature function for TCPar and TRPar currently a
;		polynomial centered on the limb. Only used if these parameter arrays 
;		set and non zero. TAK
;	25-Apr-2011	added Tunnel keyword and option SEG
;	9-May-2011 added TunSlope (slope for tunnel) TAK
;	28-Jul-2011 Playing with Temperature functions. TAK
;	7-Oct-2011 Continued changes in the streamer and cavity temperature calculations, 
;		also change in nougat temperature calculation. If TPar and TCav set tnoug is 
;		now a multiplicative factor. TAK 
;   27 Oct-2011 Added filling factor to be applied to cavity, streamer TAK 
;	13 Apr 2012 - zeroed out cavity parameters outside cavity
;	Jan 31 2013 - changed floats to double SEG
;	Feb 1 2013 -fixed bug where it checked n_elements(TPar) for making Tnoug
;		multiplicative -- always set, so need to check against it being ;		an array of zeros
;	Feb 2013 - fixed temperature to have minimum not go negative for
;		polynomial fits
;	Mar 2013 - made newkirk apply to streamer as well as hole
;
; Version 2.0 July 2014
;
;	15-Jan-2015
;		Adding parameters for cool material 
;			Inputs: CDens, CFF_Streamscales, CFF_Cavscales, CFF_Nougscales
;			Outputs
;			pop2dens - density of pop2 electrons
;			pop2fillingfactor - Fill Factor of cool particles. 
;  2-Feb-2016 Updated Population 2, and added possibility of coronal pop2
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;
;-
;
; define minimum physical temperature for 
; polynomial fits

 Tempmin=1.d4

;  Read in Parameters:

 Tcav=ModPramsStruct.TCav 
 Trim=ModPramsStruct.TRim 
 Tnoug=ModPramsStruct.TNoug
 Trgam=ModPramsStruct.TRGam
 Tcgam=ModPramsStruct.TCGam
 Tngam=ModPramsStruct.TNGam
 TCPar=ModPramsStruct.TCPar 
 TRPar=ModPramsStruct.TRPar
 TMod=ModPramsStruct.TMod
 usemod=strupcase(ModPramsStruct.UseMod)
 csheight=strupcase(ModPramsStruct.CsHeight)
 thcs=ModPramsStruct.ThCs
 nougthcs=ModPramsStruct.NougThCs
 nougphcs=ModPramsStruct.NougPhCs
 noug_m=ModPramsStruct.Noug_M
 phcs=ModPramsStruct.PhCs
 alpha=ModPramsStruct.Alpha
 m=ModPramsStruct.M
 awidth=ModPramsStruct.Awidth
 bwidth=ModPramsStruct.Bwidth
 wmult=ModPramsStruct.WMult
 depbase=ModPramsStruct.DepBase
 depslope1=ModPramsStruct.DepSlope1
 depslope2=ModPramsStruct.DepSlope2
 rdepmid=ModPramsStruct.RDepMid
 cavheight=ModPramsStruct.CavHeight
 cavbot=ModPramsStruct.CavBot
 cavwidth=ModPramsStruct.CavWidth
 gamma=ModPramsStruct.Gamma
 nougmult=ModPramsStruct.Nougmult
 nougat=ModPramsStruct.Nougat
 nougheight=ModPramsStruct.NougHeight
 nougbot=ModPramsStruct.NougBot
 nougwidth=ModPramsStruct.NougWidth
 gammanoug=ModPramsStruct.GammaNoug
 zitipram=ModPramsStruct.ZitiPram
 fraclength=ModPramsStruct.FracLength
 nfraclength=ModPramsStruct.NFracLength
 nofmax=ModPramsStruct.NoFMax
 H = fltarr(6)
 S = fltarr(6)
 C = fltarr(6)
 N = fltarr(6)
 H[0]=ModPramsStruct.H0
 H[1]=ModPramsStruct.H1
 H[2]=ModPramsStruct.H2
 H[3]=ModPramsStruct.H3
 H[4]=ModPramsStruct.H4
 H[5]=ModPramsStruct.H5
 S[0]=ModPramsStruct.S0
 S[1]=ModPramsStruct.S1
 S[2]=ModPramsStruct.S2
 S[3]=ModPramsStruct.S3
 S[4]=ModPramsStruct.S4
 S[5]=ModPramsStruct.S5
 C[0]=ModPramsStruct.C0
 C[1]=ModPramsStruct.C1
 C[2]=ModPramsStruct.C2
 C[3]=ModPramsStruct.C3
 C[4]=ModPramsStruct.C4
 C[5]=ModPramsStruct.C5
 N[0]=ModPramsStruct.N0
 N[1]=ModPramsStruct.N1
 N[2]=ModPramsStruct.N2
 N[3]=ModPramsStruct.N3
 N[4]=ModPramsStruct.N4
 N[5]=ModPramsStruct.N5
 newkirk=ModPramsStruct.Newkirk
 tunnel=ModPramsStruct.Tunnel
 tunslope=ModPramsStruct.TunSlope

 FFPar=ModPramsStruct.FFPar
 CDens=ModPramsStruct.CDens
 pop2hole=ModPramsStruct.Pop2Hole
 pop2cav=ModPramsStruct.Pop2Cav
 pop2noug=ModPramsStruct.Pop2Noug

;
; not sure why ModPramsStruct broke arrays into individual elements above
; might need to do so for these three if there was a good reason
;

 CFF_Nougscales=ModPramsStruct.CFF_Nougscales
 CFF_Streamscales=ModPramsStruct.CFF_Streamscales
 CFF_Cavscales=ModPramsStruct.CFF_Cavscales


;
; NOW CALCULATE DENSITY
;
; first set up coronal hole spherically symmetric radial falloff
;

if usemod eq 'POWER' then begin
	 Nhole = (H[0]/(r^H[1])+H[2]/(r^H[3])+H[4]/(r^H[5]))*1d5
	 Nbasehole = (H[0]+H[2]+H[4])*1d5
endif

if usemod eq 'EXPONENT' or usemod eq 'EXPDEL' then begin
	XX=1/r
	Nhole=(H[0]*exp(H[1]*XX+H[2]*XX^2)*(1.d0+H[3]*XX+H[4]*XX^2+H[5]*XX^3)*XX^2)
	Nbasehole=H[0]*exp(H[1]+H[2])*(1.d0+H[3]+H[4]+H[5])
endif

if newkirk eq 1 then begin
    Nhole=H[0]*10^(H[1]/r)
    Nbasehole=H[0]*10^(H[1])
endif

; CREATE 3D STREAMER
; start by pivoting streamer for nonradiality

pivot,r,th,ph,rpiv,thpiv,phpiv,thcs,phcs,alpha,m

; calculate latitude gaussian half width variation with height

wa=awidth+bwidth*rpiv

;  make the width stay constant for r gt csheight

testcs = where(rpiv gt csheight) 
if min(testcs) ne -1 then wa[testcs]=awidth + bwidth*csheight

; calculate longitudinal width

wb=1.d0/(wa*wmult)

; put in a background level to truncate the Gaussian at
; for streamers along neutral lines not parallel to equator
; and/or for nonzero streamer density at a pole 
; note this needs to be evaluated at photosphere where it 
; will have the biggest effect, or else there will be
; different values of fmax (dc offset) for each height

fmax=0.d0
if nofmax eq 0. then getbound,1.,awidth+bwidth,wmult*(awidth+bwidth),fmax

;if max(fmax) gt 5d-2 then print,'truncate value min,max',min(fmax),max(fmax)

; calculate angular dependence for streamer 

streamfn=exp(-(thpiv/wa)^2-(phpiv*wb)^2)-fmax

; truncate at fmax 

testtrunc=where(streamfn lt 0.d0)
if min(testtrunc) ne -1 then streamfn[testtrunc]=0.d0


if usemod eq 'EXPONENT' or usemod eq 'EXPDEL' then begin 
		Xp=1/rpiv
;
; see Gibson et al 2004 for explanation of terms and form of this equation
;  in that model, it was as for EXPDEL,
; effectively we are fitting the parameters to a delta above the coronal hole
; background, not the streamer itself, because that helps if one wants to
; generalize to multiple streamers and avoids streamer densities
; lower than the background at any height
;
;  However, for fitting a single streamer/cavity, it is more straightforward
;  to separate streamer and coronal hole fit, as in EXPONENT below
;
;
	if usemod eq 'EXPONENT' then begin
	  Nstream=S[0]*exp(S[1]*Xp+S[2]*Xp^2)*(1.d0+S[3]*Xp+S[4]*Xp^2+S[5]*Xp^3)*Xp^2
	  Nbase=S[0]*exp(S[1]+S[2])*(1.d0+S[3]+S[4]+S[5])	
	endif
	if usemod eq 'EXPDEL' then begin
	  Nstream=Nhole+S[0]*exp(S[1]*Xp+S[2]*Xp^2)*(1.d0+S[3]*Xp+S[4]*Xp^2+S[5]*Xp^3)*Xp^2
	  Nbase= Nbasehole+ S[0]*exp(S[1]+S[2])*(1.d0+S[3]+S[4]+S[5])
	endif

;
; make filling factor for population 2 material
;

        CFFstream=Cff_Streamscales[0]*exp(CFF_Streamscales[1]*Xp+$
	       CFF_Streamscales[2]*Xp^2)*(1.d0+CFF_Streamscales[3]*Xp+$
               CFF_Streamscales[4]*Xp^2+CFF_streamscales[5]*Xp^3)*Xp^2
endif

;
; note however, for the WSM fit for POWER law, the fit was to the streamer
; and hole independently.  I have kept it that way to be able to use those fits,
; but, if one wants to use this code in future with multiple streamers, one might want
; to allow a version equivalent to EXPDEL
;

if usemod eq 'POWER' then begin
	Nstream = (S[0]/(rpiv^S[1])+S[2]/(rpiv^S[3])+S[4]/(rpiv^S[5]))*1d8
	Nbase = (S[0]+S[2]+S[4])*1d8
;
; make filling factor for population 2 material
;

        CFFstream=(CFF_Streamscales[0]/(rpiv^CFF_Streamscales[1])+$
		CFF_Streamscales[2]/(rpiv^CFF_Streamscales[3])+$
		CFF_Streamscales[4]/(rpiv^CFF_Streamscales[5]))
endif

;
; but if newkirk set, just use that
;

if newkirk eq 1 then begin
    Nstream=S[0]*10^(S[1]/rpiv)
    Nbase=S[0]*10^(S[1])
    CFFstream=CFFStreamscales[0]*10^(CFFStreamscales[1]/rpiv)
endif

;
; now full function

if pop2hole eq 0 then begin
  Dens= Nhole + (Nstream-Nhole)*streamfn  
  Pop2Dens= Dens*0.
endif else begin
  Dens= (Nstream-Nhole)*streamfn  
  Pop2Dens= Nhole
endelse

;
; filling factor for population 2 needs angular dependence
; so maximum in streamer, minimum in hole
;

CFFstream=CFFstream*streamfn
if max(CFFstream) gt 1 then message,'Cool Material filling factor > 1 in streamer.'

pop2fillfact=CFFstream

;  
; CAVITY
;

;
; note we need to send an unwarped by alpha set
; of coords to getcav
;

pivot,r,th,ph,rpiv2,thpiv2,phpiv2,thcs,phcs,0.d0,m

; note rpiv2=r

ziti=0

getcav,rpiv2,thpiv2,phpiv2,cavfn,cavheight,cavwidth,cavbot,fraclength,gamma,$
	tunnel=tunnel,tunslope=tunslope

;  incorporate density depletion
;  
;  two choices-- a depletion scaled to the streamer core profile,
;  with possibility of specific inflection point (e.g. axis)
;  or defined by cavscale profiles 
;  abs(Depbase) > 1 makes it cavscale
;  Depbase > 1 will actually result in enhancement scaled to streamer core profile
;  Depbase < -1 will allow user-inputted values for cavscale
;
;  Note:  the depletion scaled to the streamer will have less depletion-
;  even possible enhancement relative to the rim, since it  is multiplied
;  by the value of the streamer at its middle where it is brightest (gaussian
;  falloff towards rims)
;  In future, I may change it so that for a given r, the theta of the cavity
;  boundary is determined, the depletion is applied to that streamer density,
;  and then it is depleted relative to the rim.
;  For now, need to overestimate depletion to get the depletion you want relative
;  to the rim.
;  

if abs(depbase) le 1. and trim ne 0. and tcav ne 0. and tnoug ne 0. then begin

	 depletion=r*0.d0
	 testlow = where(r lt rdepmid)
	 if min(testlow) ne -1 then depletion[testlow] = depbase+r[testlow]*depslope1
	 testhigh = where(r ge rdepmid)
	 if min(testhigh) ne -1 then begin
		  depmid=depbase+rdepmid*(depslope1-depslope2)
		  depletion[testhigh] = depmid + r[testhigh]*depslope2
	  endif

;
; note this is a change-- got rid of the dependence on streamfn
; because it was putting a bump in the middle of the cavity
;

	 Ncav = depletion*Nstream
	 Nbasecav = depbase*Nbase
endif

if abs(depbase) gt 1. or trim eq 0 or tcav eq 0 or tnoug eq 0 then begin
  Xp2=1/r
  if usemod eq 'EXPONENT' then begin
	Ncav=C[0]*exp(C[1]*Xp2+C[2]*Xp2^2)*(1.d0+C[3]*Xp2+C[4]*Xp2^2+C[5]*Xp2^3)*Xp2^2
	Nbasecav=C[0]*exp(C[1]+C[2])*(1.d0+C[3]+C[4]+C[5])
  endif

  if usemod eq 'EXPDEL' then begin
	Ncav=Nhole+C[0]*exp(C[1]*Xp2+C[2]*Xp2^2)*(1.d0+C[3]*Xp2+C[4]*Xp2^2+C[5]*Xp2^3)*Xp2^2
	Nbasecav=Nbasehole+C[0]*exp(C[1]+C[2])*(1.d0+C[3]+C[4]+C[5])
  endif

  if usemod eq 'POWER' then begin
	Ncav= (C[0]/(r^C[1])+C[2]/(r^C[3])+C[4]/(r^C[5]))*1d8
	Nbasecav= (C[0]+C[2]+C[4])*1d8
  endif
endif

;
; now do cavity cool filling factor if set
;

if total(CFF_cavscales) ne 0.0 then begin
 Xp2=1/r
 if usemod eq 'EXPONENT' then begin
        CFFcav=CFF_cavscales[0]*exp(CFF_cavscales[1]*Xp2+$
	       CFF_cavscales[2]*Xp2^2)*(1.d0+CFF_cavscales[3]*Xp2+$
	       CFF_cavscales[4]*Xp2^2+CFF_cavscales[5]*Xp2^3)*Xp2^2
 endif

 if usemod eq 'EXPDEL' then begin
        CFFcav=CFF_cavscales[0]*exp(CFF_cavscales[1]*Xp2+$
	       CFF_cavscales[2]*Xp2^2)*(1.d0+CFF_cavscales[3]*Xp2+$
	       CFF_cavscales[4]*Xp2^2+CFF_cavscales[5]*Xp2^3)*Xp2^2
 endif

 if usemod eq 'POWER' then begin
	CFFcav= (CFF_cavscales[0]/(r^CFF_cavscales[1])+$
	        CFF_cavscales[2]/(r^CFF_cavscales[3])+$
	        CFF_cavscales[4]/(r^CFF_cavscales[5]))
 endif
 if max(CFFcav) gt 1 then message,'Cool Material filling factor > 1 in cavity.'
endif else CFFcav=r*0.


testcav=where(cavfn eq 0.)
if min(testcav) ne -1 then begin
   if pop2cav eq 0 then begin
    Dens[testcav]=Ncav[testcav] 
    Pop2Dens[testcav] = 0.d0 
   endif else begin
    Pop2Dens[testcav]=Ncav[testcav]
    Dens[testcav]=0.d0
   endelse
   pop2fillfact[testcav]=CFFcav[testcav]
endif

;if min(Dens) lt 0. then message,'min(Dens) lt 0',/info

; Define cavity axes here so they dont get overwritten by nougat
; will use later to output cavity properties

ch=2.*semmaj
cw=2.*semmin

;  
; NOUGAT
;

if nougat eq 1 then begin

;
; note we need to send an unwarped by alpha set
; of coords to getcav
; and have them centered on nougthcs,nougphcs, with slope noug_m
;

 pivot,r,th,ph,rpiv3,thpiv3,phpiv3,nougthcs,nougphcs,0.,noug_m

; note rpiv3=r

 ziti = zitipram

 getcav,rpiv3,thpiv3,phpiv3,nougfn,nougheight,nougwidth,nougbot,nfraclength,gammanoug,$
 	tunnel=tunnel,tunslope=tunslope

;  
;  two choices for density -- nougmult times cavity
;  or for nougmult less than zero, use user-defined N
; (which default to cavity profile C)


 if nougmult ge 0. then begin
	Nnoug=nougmult*Ncav
	Nbasenoug=nougmult*Nbasecav
	CFFnoug=(CFF_nougscales[0]/(r^CFF_nougscales[1])+$
	        CFF_nougscales[2]/(r^CFF_nougscales[3])+$
	        CFF_nougscales[4]/(r^CFF_nougscales[5]))
 endif
 
 if nougmult lt 0. then begin
  Xp2=1/r
  if usemod eq 'EXPONENT' then begin
	Nnoug=N[0]*exp(N[1]*Xp2+N[2]*Xp2^2)*(1.d0+N[3]*Xp2+N[4]*Xp2^2+N[5]*Xp2^3)*Xp2^2
	Nbasenoug=N[0]*exp(N[1]+N[2])*(1.d0+N[3]+N[4]+N[5])
        CFFnoug=CFF_nougscales[0]*exp(CFF_nougscales[1]*Xp2+$
	       CFF_nougscales[2]*Xp2^2)*(1.d0+CFF_nougscales[3]*Xp2+$
	       CFF_nougscales[4]*Xp2^2+CFF_nougscales[5]*Xp2^3)*Xp2^2
  endif

  if usemod eq 'EXPDEL' then begin
	Nnoug=Nhole+N[0]*exp(N[1]*Xp2+N[2]*Xp2^2)*(1.d0+N[3]*Xp2+N[4]*Xp2^2+N[5]*Xp2^3)*Xp2^2
	Nbasenoug=Nbasehole+N[0]*exp(N[1]+N[2])*(1.d0+N[3]+N[4]+N[5])
        CFFnoug=CFF_nougscales[0]*exp(CFF_nougscales[1]*Xp2+$
	       CFF_nougscales[2]*Xp2^2)*(1.d0+CFF_nougscales[3]*Xp2+$
	       CFF_nougscales[4]*Xp2^2+CFF_nougscales[5]*Xp2^3)*Xp2^2
  endif

  if usemod eq 'POWER' then begin
	Nnoug= (N[0]/(r^N[1])+N[2]/(r^N[3])+N[4]/(r^N[5]))*1d8
	Nbasenoug= (N[0]+N[2]+N[4])*1d8
	CFFnoug=(CFF_nougscales[0]/(r^CFF_nougscales[1])+$
	        CFF_nougscales[2]/(r^CFF_nougscales[3])+$
	        CFF_nougscales[4]/(r^CFF_nougscales[5]))
  endif
 endif

 if max(CFFnoug) gt 1 then message,'Cool Material filling factor > 1 in nougat.'

 testnoug=where(nougfn eq 0.)
 if min(testnoug) ne -1 then begin
   if pop2noug eq 0 then begin
    Dens[testnoug]=Nnoug[testnoug] 
    Pop2Dens[testnoug] = 0.d0 
   endif else begin
    Pop2Dens[testnoug]=Nnoug[testnoug]
    Dens[testnoug]=0.d0
   endelse
   pop2fillfact[testnoug]=CFFnoug[testnoug]
 endif

; if min(Dens) lt 0. then message,'min(Dens) lt 0',/info

endif

; 
; TEMPERATURE AND PRESSURE
;

g=6.67d-8 ;Newton
ms=1.99d33 ;grams in sun
mp=1.67d-24 ; grams in proton
k=1.38d-16; boltzmann
rs=6.9570d10 ;cm solar rad

; assume helium abundance of 0.1
;

al_he=0.1d0

; turn off for debugging
;al_he=0.d0

; NOTE WE SHOULD BE SURE ABUNDANCES
; OF POPULATIONS ARE CONSISTENT WITH THIS!
;

; set up temperature/pressure in the rim
; I am going to define density and temperature
; in terms of a total CORONAL density
; (this may need reworking)
;

TotalDens=Dens+Pop2Dens

;
; first if we proscribe isothermal/polytropic temperature,
; pressure falls out of ideal gas
;

if Trim ne 0 and Tcav ne 0 and Tnoug ne 0 then begin
   Temp = Trim*(Nbase/TotalDens)^(Trgam-1.d0)
   Pres = (2.d0+3.d0*al_he)/(1.d0+2.d0*al_he)*TotalDens*k*Temp
endif

;
; Alternatively, if we force radial hydrostatic balance
; then pressure is determined, and temperature falls out
; of ideal gas
; Remember, I have 'POWER'
;

if Trim eq 0 or Tcav eq 0 or Tnoug eq 0 then begin

   StreamerTerm=((S[0]/(S[1]+1.d0))*rpiv^(-S[1]-1.d0)+(S[2]/(S[3]+1.d0))*rpiv^(-S[3]-1.d0)$
		+(S[4]/((S[5]+1.d0))*rpiv^(-S[5]-1.d0)))*1d8
   HoleTerm= ((H[0]/(H[1]+1.d0))*r^(-H[1]-1.d0)+(H[2]/(H[3]+1.d0))*r^(-H[3]-1.d0)$
		+(H[4]/((H[5]+1.d0))*r^(-H[5]-1.d0)))*1d5

   Pres= ((1.d0+4.d0*al_he)/(1.d0+2.d0*al_he)*(g*ms*mp)/rs)*$
		(HoleTerm + (StreamerTerm-HoleTerm)*streamfn)
   Temp = ((1.d0+2.d0*al_he)/(2.d0+3.d0*al_he))*Pres/TotalDens/k

endif

;If we want the temparature to be a freely variable function then 
; (making sure we don't drop to unrealistic values)
;
if total(TRPar) ne 0 then begin
 for sh=0,1 do begin 
   if sh eq 0 then ruse=rpiv
   if sh eq 1 then ruse=r 
   case TMod of 
   		'POLY': Temp=Poly(ruse-1.d0,TRPar)
   		'POWER': begin
   			TRPar1=fltarr(6)
   			TRPAR1[0:n_elements(TRPar)-1]=TRPAR
   			Temp = (TRPar1[0]*ruse^TRPar1[1]+TRPar1[2]*ruse^TRPar1[3]+TRPar1[4]*ruse^TRPar1[5])
   			end
   		'POW1': begin;
			NPar = n_elements(TRPar)
			Case NPar of 
				1: Temp=TRPar[0]
				2: Temp= TRPar[0]*(ruse-1.d0)^TRPar[1]
				3: Temp=TRPar[0]+TRPar[1]*(ruse-1.d0)^TRPar[2]
				4: Temp= TRPar[0]*(ruse-1.d0)^TRPar[1] + TRPar[2]*(ruse-1.d0)^TRPar[3]
				5: Temp=TRPar[0]+TRPar[1]*(ruse-1.d0)^TRPar[2]+$
					TRPar[3]*(ruse-1.d0)^TRPar[4]
				6: Temp= TRPar[0]*(ruse-1.d0)^TRPar[1] + TRPar[2]*(ruse-1.d0)^TRPar[3]+$
					TRPar[4]*(ruse-1.d0)^TRPar[5]
				7: Temp=TRPar[0]+TRPar[1]*(ruse-1.d0)^TRPar[2]+$
					TRPar[3]*(ruse-1.d0)^TRPar[4]+TRPar[5]*(ruse-1.d0)^TRPar[6]
				8: Temp= TRPar[0]*(ruse-1.d0)^TRPar[1] + TRPar[2]*(ruse-1.d0)^TRPar[3]+$
						TRPar[4]*(ruse-1.d0)^TRPar[5]+TRPar[6]*(ruse-1.d0)^TRPar[7]
					
   				else: message,'Number of parameters should be 1- 8'
   				endcase ;  NPAR case
   			end ;end 'pow1'
   		else: begin
   			NPar = n_elements(TRPar)
			Case NPar of 
				1: Temp=TRPar[0]
				2: Temp= TRPar[0]*ruse^TRPar[1]
				3: Temp=TRPar[0]+TRPar[1]*ruse^TRPar[2]
				4: Temp= TRPar[0]*ruse^TRPar[1] + TRPar[2]*ruse^TRPar[3]
				5: Temp=TRPar[0]+TRPar[1]*ruse^TRPar[2]+$
					TRPar[3]*ruse^TRPar[4]
				6: Temp= TRPar[0]*ruse^TRPar[1] + TRPar[2]*ruse^TRPar[3]+$
					TRPar[4]*ruse^TRPar[5]
				7: Temp=TRPar[0]+TRPar[1]*ruse^TRPar[2]+$
					TRPar[3]*ruse^TRPar[4]+TRPar[5]*ruse^TRPar[6]
				8: Temp= TRPar[0]*ruse^TRPar[1] + TRPar[2]*ruse^TRPar[3]+$
						TRPar[4]*ruse^TRPar[5]+TRPar[6]*ruse^TRPar[7]
					
   				else: message,'Number of parameters should be 1- 8'
   				endcase ;  NPAR case
   			  end ;endelse
   	endcase
 	Temp=Temp*1d6
	Temp=Temp > Tempmin
     if sh eq 0 then StreamerTerm=Temp
     if sh eq 1 then HoleTerm=Temp
   endfor
   Temp=HoleTerm + (StreamerTerm-HoleTerm)*streamfn
   Pres = (2.d0+3.d0*al_he)/(1.d0+2.d0*al_he)*TotalDens*k*Temp
endif

; set up temperature/pressure in the cavity

if min(testcav) ne -1 then begin

 if Tcav ne 0 and Trim ne 0 and Tnoug ne 0 then begin
    Temp[testcav] = Tcav*(Nbasecav/TotalDens[testcav])^(Tcgam-1.)
    Pres[testcav] = (2.d0+3.d0*al_he)/(1.d0+2.d0*al_he)*TotalDens[testcav]*k*Temp[testcav]
 endif

; Force radial hydrostatic balance

 if Tcav eq 0 or Trim eq 0 or Tnoug eq 0 then begin
  CavTerm= ((C[0]/(C[1]+1.d0))*r[testcav]^(-C[1]-1.d0)$
		+(C[2]/(C[3]+1.d0))*r[testcav]^(-C[3]-1.d0)$
		+(C[4]/(C[5]+1.d0))*r[testcav]^(-C[5]-1.d0))*1d8

  Pres[testcav]= ((1.d0+4.d0*al_he)/(1.d0+2.d0*al_he)*(g*ms*mp)/rs)*CavTerm
  Temp[testcav] = ((1.d0+2.d0*al_he)/(2.d0+3.d0*al_he))*Pres[testcav]/TotalDens[testcav]/k
 endif

;If we want the temparature to be a freely variable function then 
 if total(TCPar) ne 0 then begin
   case TMod of 
   		'POLY': Temp[testcav]=Poly(r[testcav]-1.d0,TCPar)
   		'POWER': begin
   		        TCPAR1=fltarr(6)
   		        TCPAR1[0:N_elements(TCPAR)-1]=TCPAR
   				Temp[testcav] = (TCPar1[0]*r[testcav]^TCPar1[1]+$
   						TCPar1[2]*r[testcav]^TCPar1[3]+TCPar1[4]*r[testcav] ^TCPar1[5])
   			end
   		'POW1': begin
				NPar = n_elements(TCPar)
				Case NPar of 
					1: Temp[testcav]=TCPar[0] 
					2: Temp[testcav]= TCPar[0]*(r[testcav]-1.d0)^TCPar[1]
					3: Temp[testcav]= TCPar[0] + TCPar[1]*(r[testcav]-1.d0)^TCPar[2]
					4: Temp[testcav]= TCPar[0]*(r[testcav]-1.d0)^TCPar[1] + TCPar[2]*(r[testcav]-1.d0)^TCPar[3]
					5: Temp[testcav]= TCPar[0] + TCPar[1]*(r[testcav]-1.d0)^TCPar[2]+$
										TCPar[3]*(r[testcav]-1.d0)^TCPar[4]
					6: Temp[testcav]= TCPar[0]*(r[testcav]-1.d0)^TCPar[1] + TCPar[2]*(r[testcav]-1.d0)^TCPar[3]+$
									 TCPar[4]*(r[testcav]-1.d0)^TCPar[5]
					7: Temp[testcav]= TCPar[0] + TCPar[1]*(r[testcav]-1.d0)^TCPar[2]+$
										TCPar[3]*(r[testcav]-1.d0)^TCPar[4]+TCPar[5]*(r[testcav]-1.d0)^TCPar[6]
					8: Temp[testcav]= TCPar[0]*(r[testcav]-1.d0)^TCPar[1] + TCPar[2]*(r[testcav]-1.d0)^TCPar[3]+$
							TCPar[4]*(r[testcav]-1.d0)^TCPar[5]+TCPar[6]*(r[testcav]-1.d0)^TCPar[7]
					else: message,'Number of parameters should be 1- 8'
				endcase
				end
   		else: begin		
				NPar = n_elements(TCPar)
				Case NPar of 
					1: Temp[testcav]=TCPar[0] 
					2: Temp[testcav]= TCPar[0]*r[testcav]^TCPar[1]
					3: Temp[testcav]= TCPar[0] + TCPar[1]*r[testcav]^TCPar[2]
					4: Temp[testcav]= TCPar[0]*r[testcav]^TCPar[1] + TCPar[2]*r[testcav]^TCPar[3]
					5: Temp[testcav]= TCPar[0] + TCPar[1]*r[testcav]^TCPar[2]+$
										TCPar[3]*r[testcav]^TCPar[4]
					6: Temp[testcav]= TCPar[0]*r[testcav]^TCPar[1] + TCPar[2]*r[testcav]^TCPar[3]+$
									 TCPar[4]*r[testcav]^TCPar[5]
					7: Temp[testcav]= TCPar[0] + TCPar[1]*r[testcav]^TCPar[2]+$
										TCPar[3]*r[testcav]^TCPar[4]+TCPar[5]*r[testcav]^TCPar[6]
					8: Temp[testcav]= TCPar[0]*r[testcav]^TCPar[1] + TCPar[2]*r[testcav]^TCPar[3]+$
							TCPar[4]*r[testcav]^TCPar[5]+TCPar[6]*r[testcav]^TCPar[7]
					else: message,'Number of parameters should be 1- 8'
				endcase ;npar case
   			end  ;end else
   	endcase
	Temp[testcav]=Temp[testcav]*1d6
	Temp[testcav]=Temp[testcav] > Tempmin

   Pres[testcav] = (2.d0+3.d0*al_he)/(1.d0+2.d0*al_he)*TotalDens[testcav]*k*Temp[testcav]
 endif
endif

; set up temperature/pressure in the cavity

if nougat eq 1 then begin

 if min(testnoug) ne -1 then begin
  if total(TCPar) ne 0 then begin
   	Temp[testnoug]=Temp[testnoug]*Tnoug 	;- using tnough as a multiplicative factor here.
  	 Pres[testnoug] = (2.d0+3.d0*al_he)/(1.d0+2.d0*al_he)*TotalDens[testnoug]*k*Temp[testnoug]
  endif else begin
	  if Tcav ne 0 and Trim ne 0 and Tnoug ne 0 then begin
		Temp[testnoug] = Tnoug*(Nbasenoug/TotalDens[testnoug])^(Tngam-1.)
		Pres[testnoug] = (2.d0+3.d0*al_he)/(1.d0+2.d0*al_he)*TotalDens[testnoug]*k*Temp[testnoug]
	  endif
	
	; Force radial hydrostatic balance
	
	  if Tcav eq 0 or Trim eq 0 or Tnoug eq 0 then begin
	   NougTerm= ((N[0]/(N[1]+1.d0))*r[testnoug]^(-N[1]-1.d0)$
			+(N[2]/(N[3]+1.d0))*r[testnoug]^(-N[3]-1.d0)$
			+(N[4]/(N[5]+1.d0))*r[testnoug]^(-N[5]-1.d0))*1d8
	
	   Pres[testnoug]= ((1.d0+4.d0*al_he)/(1.d0+2.d0*al_he)*(g*ms*mp)/rs)*NougTerm
	   Temp[testnoug] = ((1.d0+2.d0*al_he)/(2.d0+3.d0*al_he))*Pres[testnoug]/TotalDens[testnoug]/k
	  endif
  endelse
 endif ;end testnoug defined
endif ;end nougat=1

;Filling Factor
NFFP=n_elements(FFPar) 
case NFFP of
	0: fillfact=1.  ;no filling factor used
	1: fillfact=FFPar
	2: fillfact=FFPar[0]+FFPar[1]*rpiv
	3: fillfact=FFPar[0]+FFPar[1]*rpiv+FFPar[2]*rpiv^2
	else: message,'Currently a max of three parameters allowed in FFPar'
endcase
fillfact=fillfact<1.

maxpop2fillfact=max(pop2fillfact)
if maxpop2fillfact gt 1 then message,'The max pop2fillfact, '+trim(maxpop2fillfact)+', > 1'

;
; if filling factor not explicitly set for Pop1, then make Pop1 + Pop2= 1
;

if min(fillfact) eq 1. and max(fillfact) eq 1. then fillfact=1.-pop2fillfact

maxfillfact=max(fillfact+pop2fillfact) 
	;Maybe this should result in a full stop, or a reduction in fillfact, but for now it is just a warning.
if maxfillfact gt 1 then message,/info,'The maximum combined filling factor, '+trim(maxfillfact)+', >1!'


;
; now output variables requested
; added outputs of cavity properties, purely a function of phipiv2
;

tcs=thcs - phpiv2*m/sqrt(1.d0+m*m)

; note this is actually theta_nl in the original form of the paper
; that is, right down the core of the streamer along its length
; remember, phpiv2 is unwarped by alpha, and theta_mg = 0 along nl

; cavbot is same for all phi
; so is cavity tilt angle gamma

dist=ch-1.d0+cavbot

fun1=sin(gamma)*dist
fun2=1.d0+cos(gamma)*dist

ctr=sqrt(fun1*fun1 + fun2*fun2)
cttp=atan(fun1,fun2)
ctt=tcs+cttp
testnocav=where(cavfn eq 1.)
if min(testnocav) ne -1 then begin
	ch[testnocav] = 0.0d0
	cw[testnocav] = 0.0d0
	ctr[testnocav] = 0.0d0
	ctt[testnocav] = 0.0d0
	tcs[testnocav] = 0.0d0
endif

mdtor=!dpi/180d0

ModSolStruct={Pres:Pres,Dens:TotalDens,Temp:Temp,$
	CavHeight:ch,CavWidth:cw,CavTopR:ctr,CavTopT:ctt/mdtor,ThCs:tcs/mdtor,Gamma:gamma/mdtor}

if CDens ne 0 then begin
  ModSolStruct=add_tag(ModSolStruct,CDens,'Pop2Dens')
    ;cool particle density (scalar)
  ModSolStruct=add_tag(ModSolStruct,2,'RegimeForce')
  ModSolStruct=add_tag(ModSolStruct,pop2fillfact,'Pop2FillingFactor')
endif

;
; cool population trumps second hot
;
if CDens eq 0 and (pop2hole ne 0 or pop2cav ne 0 or pop2noug ne 0) then begin
  ModSolStruct=add_tag(ModSolStruct,Dens,'Pop1Dens')
  ModSolStruct=add_tag(ModSolStruct,Pop2Dens,'Pop2Dens')
  ModSolStruct=add_tag(ModSolStruct,1,'RegimeForce')
  ModSolStruct=add_tag(ModSolStruct,1.,'Pop2FillingFactor')
  ModSolStruct=add_tag(ModSolStruct,1.,'FillingFactor')
  fillfact=1.
endif 

if min(fillfact) ne max(fillfact) and min(fillfact) ne 1. then ModSolStruct=add_tag(ModSolStruct,fillfact,'FillingFactor')

end
