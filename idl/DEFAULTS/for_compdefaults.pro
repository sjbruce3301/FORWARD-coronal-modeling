function for_compdefaults,compprams0,compline,compval,tp2te=tp2te,for_wlmin=for_wlmin,for_wlmax=for_wlmax,smalln=smalln,qnorm=qnorm,cecoeff=cecoeff,isum=isum,icoll=icoll,isplin=isplin,iwatom=iwatom,iwline=iwline,iweqi=iweqi,idebug=idebug,fiwatmo=fiwatmo,noftran=noftran,seecomp=seecomp,crtn=crtn,vintchoice=vintchoice,alin=alin

common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops

;
; this program sets all defaults for the COMP-type observable calculation
;
;  Called by FOR_OBSDEFAULTS, FOR_DRIVE, FOR_PLOTFITS
;
;  Returns structure (FCOMPPRAMS) containing the keywords to follow
;       as tags; it is an output of this code that becomes a tag within OBSPRAMSSTRUCT,
;       and also within OBSINPUTS (used by FOR_WIDGET to populate call to FOR_DRIVE).
;       It is used to write the INPUT file for FORCOMP.
;
;  Inputs
;	COMPPRAMS0 - old version of structure - if called without any
;		keywords then this becomes default
;
;	COMPLINE - this enters FOR_DRIVE as keyword INSTRUMENT but for CoMP is actually line being observed by 
;		COMP, e.g. FE13
;	COMPVAL - this enters FOR_DRIVE as keyword LINE but for CoMP is actually Stokes vector or line width, shift etc
;
;  Keywords defined below
;
;  Written Sarah Gibson 2014
;
;  Version 2.0 July 2014
;  
;  Feb 2016: Changed default for SEECOMP - SEG
;  Jun 2022: uncommented QNORM 
;  Mar-May 2024: added "alin" keyword which 
;	is _not_ passed through all of FORWARD
;	but can be changed default below, and allows
;	changes to CLE INPUT file as communicated by
;	Alin Paraschiv in December 2022 
;   Also added CECOEFF for other lines  


obsstart=flag.obs

default,alin,1

if strpos(strupcase(compline),'OMP') lt 0 and strupcase(compline) ne 'CORMAG' then begin
  SeeComp=1
  TP2TE=0.
  for_wlmin=0. 
  for_wlmax=0. 
  smalln=0.
  qnorm=0.
  cecoeff=0.
  CeCoeffVal='nodisplay'
  isum=0
  icoll=0
  isplin=0
  iwatom=0
  iwline=0
  IWLineVal='nodisplay'
  iweqi=0
  idebug=0
  fiwatmo=0
  crtn=''
  vintchoice='3'
  vintchoiceval='nodisplay'
  noftran=0
  NoFtranVal='nodisplay'

endif else begin

; use updated inputs from P. Judge as transmitted to S. Gibson
; by A. Paraschiv on 12/21/23
   if keyword_set(alin) then ause=1 else ause=0

   if keyword_set(compprams0) gt 0 then begin
        in_tags=tag_names(compprams0)
        for i=0,n_elements(in_tags)-1 do begin
                res=execute('if n_elements('+in_tags[i]+') eq 0 then '+in_tags[i]+'=compprams0.'+in_tags[i])
        endfor
   endif


;
; toggle for widget
;

   default,SeeComp,1

;
;  CRTN
;
;  this set the B field models tested in original CLE implementation
;  it should not be changed because other models are currently
;  not supported by FORWARD. CUSER just means user inputs model
;  thus the models set up within FORWARD.
;  Note it will be forced equal to CUSER at the bottom of this routine
;  for now it will serve as a marker that the code is coming from 
;  a widget call where a prior widget call was not CoMP-like
; (and so force reset of the keywords from null values)
;
;
   default,crtn,'CUSER'

;
; currently being tested: IDL interface for FORCOMP codes
; not included with main FORWARD distribution
;

   default,noftran,0
   if crtn eq '' or obsstart eq 1 then noftran=0

;
;  TP2TE
;
; Ratio of proton to electron temperatures 
; assumption thermalized means set to 1
; not changable in widget but can play with
; as keyword to FOR_DRIVE from command line
;

   default,tp2te,1.0
   if crtn eq '' or obsstart eq 1 then tp2te=1.

;
;  FOR_WLMIN, FOR_WLMAX
;    wavelength range within which to look for line
;    not changable in widget but can be changed as
;    keyword to FOR_DRIVE from command line
;    but beware - could miss the line
;     that is why these are purposefully broad, but
;     if want to isolate a line could narrow them.
;


Case 1 of
 strupcase(compline) eq 'SI10COMP' or strupcase(compline) eq 'SICOMP': begin

   if ause eq 0 then begin
    wlminuse=13000.
    wlmaxuse=15000.
   endif else begin
; same
    wlminuse=13000.
    wlmaxuse=15000.
   endelse

   default,for_wlmin,wlminuse
   default,for_wlmax,wlmaxuse
;
   if crtn eq '' or obsstart eq 1 then for_wlmin=wlminuse
   if crtn eq '' or obsstart eq 1 then for_wlmax=wlmaxuse
;
; Only one SI10 line, 14302
;
 end
 strupcase(compline) eq 'SI9COMP': begin
   if ause eq 0 then begin
    wlminuse=36000.
    wlmaxuse=42000.
   endif else begin
    wlminuse=39000.
    wlmaxuse=40000.
   endelse

   default,for_wlmin,wlminuse
   default,for_wlmax,wlmaxuse
   if crtn eq '' or obsstart eq 1 then for_wlmin=wlminuse
   if crtn eq '' or obsstart eq 1 then for_wlmax=wlmaxuse
;
; with this range only the first of the SI9 lines, 39267 will be saved... 
;  if the others, 15584, or 25839 are desired, the range needs to be changed
;
 end
 strupcase(compline) eq 'FE11COMP': begin
   default,for_wlmin,7700.
   default,for_wlmax,8100.
   if crtn eq '' or obsstart eq 1 then for_wlmin=7700.
   if crtn eq '' or obsstart eq 1 then for_wlmax=8100.
;
; with this range only the first of the FE11 lines, 7892, will be saved... 
;  if the others, 60607m 6885 are desired, the range needs to be changed
;
 end
 strupcase(compline) eq 'COMP' or strupcase(compline) eq 'OTHERCOMP' or strupcase(compline)  eq 'WAVECOMP': begin
   default,for_wlmin,10700.
   default,for_wlmax,10900.
   if crtn eq '' or obsstart eq 1 then for_wlmin=10700.
   if crtn eq '' or obsstart eq 1 then for_wlmax=10900.
;
; this range will be big enough to grab both the other lines, 10747 and 10798,
; so in FOR_INTCOMPCLOSE the keyword USELINENUM is set to 0 (10747) or 1 (10798)
; to choose between them, based on selection of line (COMP vs OTHERCOMP; FE13_10747 vs FE13_10798)
;
; values from Alin's updates:
; a)10720-10770
; b)10765-10815
; (havent changed -- shouldnt affect running of code)
 end
 strupcase(compline) eq 'CORMAG': begin
   default,for_wlmin,5290.
   default,for_wlmax,5320.
   if crtn eq '' or obsstart eq 1 then for_wlmin=5290.
   if crtn eq '' or obsstart eq 1 then for_wlmax=5320.
; 
; Only one Fe14 line, 5303
;
; note this eliminates 5385.944 from consideration-- if
; this one is wanted, need to reset FOR_WLMIN, FOR_WLMAX to bracket it
 end
endcase

;
;  SMALLN
;     density below which not to do collisions
;
   
   default,smalln,1.E-5
   if crtn eq '' or obsstart eq 1 then smalln=1.E-5

;
;  QNORM
;    means of adjusting grid spacing
;    and conversion to velocity units
;    kernal of linear high density spacing
;    surrounded by log-spaced sparsely sampled wings
;    so, assuming velocities in the 10s of km/sec
;    default of 10 km/sec is reasonable
;    not changable from widget but can be played
;    with in command line - 
;
   default,qnorm,10.00
   if crtn eq '' or obsstart eq 1 then qnorm=10.00

;
;  CECOEFF
;
;      rate coefficient for elastic collisions
;      should only be set for Fe13
;
; ***but also Alin's showed for Si9, Si10!
;  **and was in Phil's Fe11
;  (not green?)
;
;      introduced in order to relax the alignment to mimic 
;      the effects of missing levels higher in the atomic ion
;      term diagrams.  
;
;  see
;	Judge, P. G., Low, B. C., and Casini, R.: 2006
;	Spectral Lines for Polarization 
;	Measurements of the Coronal Magnetic Field. 
;	IV. Stokes Signals in Current-carrying Fields
;	ApJ 651, 1229-1237
;
; 	CAVEAT EMPTOR (pj)
;
; note Alin Fe13 cecoeff=2e-9
;      Alin SI10 cecoeff=.8e-9
;      Alin SI9 cecoeff=5e-8

   if strupcase(compline) eq 'COMP' or strupcase(compline) eq 'OTHERCOMP' or strupcase(compline) eq 'FE11COMP' or strupcase(compline) eq 'SI10COMP' or strupcase(compline) eq 'SI9COMP' or strupcase(compline)  eq 'WAVECOMP' then begin
    if strupcase(compline) eq 'FE11COMP' then default,cecoeff,9.E-07
    if strupcase(compline) eq 'SI10COMP' then default,cecoeff,.8E-09
    if strupcase(compline) eq 'SI9COMP' then default,cecoeff,5.E-08
    if strupcase(compline) eq 'COMP' or strupcase(compline) eq 'OTHERCOMP' or strupcase(compline)  eq 'WAVECOMP' then $
; COMP is 10747; OTHERCOMP is 10798; WAVECOMP is also FE13
     if ause eq 1 then default,cecoeff,2.E-09 else default,cecoeff,9.E-09 
    if crtn eq '' or obsstart eq 1 then begin
     if strupcase(compline) eq 'FE11COMP' then cecoeff=9.E-07
     if strupcase(compline) eq 'SI10COMP' then cecoeff=.8E-09
     if strupcase(compline) eq 'SI9COMP' then cecoeff=5.E-08
     if strupcase(compline) eq 'COMP' or strupcase(compline) eq 'OTHERCOMP' or strupcase(compline)  eq 'WAVECOMP' then $
      if ause eq 1 then cecoeff=2.E-09 else cecoeff=9.E-09 
    endif
    cecoeffval='double'
  endif else begin
    cecoeff=0.
    cecoeffval='nodisplay'
  endelse

;print,cecoeff
;
;  ISUM
;
; level index to use for conservation equation
; ISUM=1 means that you are taking the first equation
; of the statistical equilibrium system and replacing with
; the population conservation condition
;
; UNLIKELY TO WANT TO CHANGE - may effect efficiency, error..
;    not changable from widget but can be played
;    with in command line - but be careful!
;

   default,isum,1
   if crtn eq '' or obsstart eq 1 then isum=1

;
;  ICOLL
;    turn collisions off/on
;

; note Alin icoll=4
   if ause eq 1 then icolluse=4 else icolluse=1
   default,icoll,icolluse
   if crtn eq '' or obsstart eq 1 then icoll=icolluse

;
; turn off if doing IDL version
; for testing purposes
;
;   if noftran eq 1 then icoll=0

;
;  ISPLIN
; 'INTERPOLATION: 2=LINEAR, 3....N= SPLINE 
;  
; UNLIKELY TO WANT TO CHANGE - may effect efficiency, error..
;    not changable from widget but can be played
;    with in command line - but be careful!
;  NOTE - isplin=3 default seems to give inaccurate results
;  when compared to IDL results using IDL spline function (which
; is independent of isplin),
; so changed to isplin=50.
; *but note in alin's input files, he used 3
; 
   if ause eq 0 then isplinuse=50 else isplinuse=3

   default,isplin,isplinuse
   if crtn eq '' or obsstart eq 1 then isplin=isplinuse

;
;  IWATOM
; OUTPUT ATOMIC PARAMETERS TO FILE 
; 
; various diagnostic files are made during run
; but these are removed in FORWARD when run is done
; turn it on and
; put in a stop in FOR_INTENSINT after call to
; FORCOMP to study this and others below
;
   default,iwatom,0
   if crtn eq '' or obsstart eq 1 then iwatom=0

;
;  IWLINE
;
;     whether or not to output full spectrum. 
;     this is forced for choice of Doppler V
;	or line width
;   

   default,iwline,1
   if obsstart ne 0 then iwline=1
   if strupcase(compval) eq 'DOPPLERVLOS' or strupcase(compline) eq 'WAVECOMP' then begin
     iwline=1
     IWLineVal='nodisplay'
   endif else IWLineVal='tog'

;
; IWEQI
; OUTPUT IONIZATION EQUILIBRIUM DATA    
; Diagnostic - see above 
;
   default,iweqi,0
   if crtn eq '' or obsstart eq 1 then iweqi=0

;
;  IDEBUG
; 
; gives debugging info
; 
   default,idebug,0
   if crtn eq '' or obsstart eq 1 then idebug=0

;
;  FIWATMO
;
; OUTPUT CORONAL DATA TO FILE ATMOS
; Diagnostic - see above
;
   default,fiwatmo,0

   if crtn eq '' or obsstart eq 1 then crtn='CUSER'

;NoFtranVal='tog'
   NoFtranVal='nodisplay'

;  VINTCHOICE -- how to represent Stokes V
;       set in FOR_COMPDEFAULTS (carried through in ObsPramsStruct.FCompPrams)
;               0: "classic"- use output of FORCOMP; here sign on V is changed at
;                       rest wavelength (ilambda). This gives an unsigned integral _if_
;                       Stokes V is symmetric about the rest wavelength, which it won't be
;                       if e.g. there is velocity. BETTER NOT TO USE.
;               1: "classic_check"same as 0, but calculated in this routine instead of FORCOMP -- should give
;                       same result as 0 (for debugging purposes). BETTER NOT TO USE.
;               2: "upgraded_classic" -- uses Icent instead of rest wavelength, calculated as 1
;                       this is better for dealing with velocity (Icent is shifted) -- but,
;                       still could be weirdness arising from LOS integral - that is, the zero
;                       crossing of (integrated) Stokes V might not be at the maximum of (integrated) Stokes I
;               3: "unsigned_integral" -- thus, integral is done over |V|, with a sign assigned
;                       based on the dominating sign to the left of ilambda
;                               ***DEFAULT***
;               4: "peak-to-peak/eff" -- this is the max(V) - min(V), multiplied by sign based on
;                       dominating sign to left of ilambda as in choice 2
;                       In order to keep the units straight and avoid too many other changes,
;                       this will be also be multiplied by I/Icent - effective line width
;                       so that V/I will actually be V(peak to peak)/)Icent
;               5:"peak-to-peak/lw" -- same as #3, but instead of multiplying by I/Icent
;                       multiplies by line width (calculated below via gaussian fit)
;      
;      
;   NOTE if IWLINE=0 then VINTCHOICE is forced to 0

    default,vintchoice,'3'
    vintchoiceval=["classic","classic_check","upgraded_classic","unsigned_integral","peak-to-peak/eff","peak-to-peak/lw"]
    if string(vintchoice) eq "classic" then vintchoice = '0'
    if string(vintchoice) eq "classic_check" then vintchoice = '1'
    if string(vintchoice) eq "upgraded_classic" then vintchoice = '2'
    if string(vintchoice) eq "unsigned_integral" then vintchoice = '3'
    if string(vintchoice) eq "peak-to-peak/eff" then vintchoice = '4'
    if string(vintchoice) eq "peak-to-peak/lw" then vintchoice = '5'
    if iwline eq 0 then begin
      vintchoice='0'
      vintchoiceval='nodisplay'
    endif
endelse

QNormVal='nodisplay'
TP2TEVal='nodisplay'
For_WLMinVal='nodisplay'
For_WLMaxVal='nodisplay'

;print,'wlmin=',for_wlmin
;print,'wlmax=',for_wlmax

;print,'cecoeff=',cecoeff

return,{SeeComp:SeeComp,SeeCompVal:'tog',TP2TE:TP2TE,TP2TEVal:TP2TEVal,For_WLMin:For_WLMin,For_WLMinVal:For_WLMinVal,For_WLMax:For_WLMax,For_WLMaxVal:For_WLMaxVal,SmallN:SmallN,SmallNVal:'double',QNorm:QNorm,QNormVal:QNormVal,CeCoeff:CeCoeff,CeCoeffVal:CeCoeffVal,ISum:ISum,ISumVal:'nodisplay',IColl:IColl,ICollVal:'tog',ISplin:ISplin,ISplinVal:'nodisplay',IWAtom:IWAtom,IWAtomVal:'nodisplay',IWLine:IWline,IWLineVal:IWLineVal,IWEqi:IWEqi,IWEqiVal:'nodisplay',IDebug:IDebug,IDebugval:'nodisplay',FIWAtmo:FIWAtmo,FIWAtmoVal:'nodisplay',CRTN:CRTN,CRTNVal:'nodisplay',NoFtran:NoFTran,NoFtranVal:NoFtranVal,vintchoice:vintchoice,vintchoiceval:vintchoiceval}

end
