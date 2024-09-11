;************************************************************************
;+
pro gibbaglowprams,outarray,date=date,working_dir=working_dir,$
		      apar=apar0,bpar=bpar0,$
                      hydro=hydro0,cdensprof=cdensprof,ct0=ct0,$
                      odensprof=odensprof,ot0=ot0,$
		      gamma1=gamma10,gamma3=gamma30,$
		      vscale=vscale0,$
		      saveprams=saveprams,readprams=readprams
;
;Name: GIBBAGLOWPRAMS
;
;Purpose: To create structure containing information concerning the
;         Gibson-Bagenal-Low CME model; To be called by driver routine and resulting
;         structure will be named ModPramsStruct (with ModPramsStruct.name='gibbaglow')
;
;  Called by FOR_MODELDEFAULTS
;
;Keyword Inputs:  
;		   
;         PHYSICAL PROPERTIES
;
;  APAR - (also affects MORPHOLOGY) just as in the Bogdan and Low model,
;                       this allows an expansion of the field and introduces
;                       currents. apar is the radial shift transformation used to make a
;                       tear-drop shape (r -> r + apar)
;                       NOTE APAR not equal to zero can introduce negative densities
;                       DEFAULT 0.0
;
;  BPAR - cusp height
;                       DEFAULT 2.78
;
;
;  GAMMA1, GAMMA3 - Dipole/Octopole terms (units Gauss)
;                       DEFAULT GBL:  -10. -1.2
;                         (note, original paper had 1.2 for the octopole
;                           so this is not an exact reproduction
;                           for now we are going to insist on same sign G1, G3 for GBL normalization
;                           because this introduces additional null at equator instead of pole)
;                       FOR TESTING ONLY -- uncomment further down where gamma1,gamma3,bpar set
;                         also in gibbaglow.pro where NORM1, NORM3 are set
;                         DEFAULT Low 1986: 45*b*b, -4
;                         (note, original paper had 3*b*b,4 - we believe this was a typo)
;                         (also note, to reproduce in normalization of GBL, use gamma1=45, gamma3=360)
;
; VSCALE - scale for radial velocity profile along the open field (varies with height)
;                           UNITS KM/SEC
;                           DEFAULT 0.d0
;
; 
; HYDRO, CDENSPROF, ODENSPROF, CT0, OT0:
;  how to handle the plasma throughout the corona.
;       DEFAULT HYDRO 4
;               Vasquez 2003
;
;       FOR CDENSPROF, ODENSPROF, CTO, OT0 DEFAULTS SEE NUMCUBE/FOR_HYDRODEFAULTS
;
;	BOOKKEEPING
;
;              	SAVEPRAMS - if keyword set, write parameters to filename
;                       (which is the keyword value saveprams)
;                       or if set to 1, then return pramarray
;
;              	READPRAMS - if keyword set, read in parameters from filename
;                       (which is the keyword value filename)
;                       or if a structure than read directly
;      
; 
;ModPramsStruct Outputs:  
;
;		As above, plus
;
;		NAME -- gibbaglow-- so that procedure can be called in intensint
;		LABEL -- Gibson, Bagenal and Low -- for plot label
;	  	MAGMOD -- 1 -- meaning it is a magnetized model
;		VELIMPOSE -- set to 0, because redundant with vscale
;               CDENSPROF, ODENSPROF -- may be converted to vectors by FOR_HYDRODEFAULTS
;               CT0, OT0 -- may be changed in for_hydrodefaults
;
;Output: outarray - structure containing keyword output model parameters 
;
;Common Blocks: None
;
; Author and history:
;       Written by Jie Zhao and Sarah Gibson June 2018
;        Sep 2018 adjusted hydrostatic model
;		Oct 2018 added hydro=3; Vasquez model option
;	June 2019 used slash for PC compatibility
;               made default OT0 1.5d6 for HYDRO=3
;	Sept 2019 -- added hydro=4
;	Feb 2022 -- cleaned up hydro etc
;		also simplified default statements
;		and changed DENSPROF, TE to CDENSPROF, CT0
;- 

;print,'hello prams'
slash=path_sep()

;
; set parameter defaults
;

if keyword_set(readprams) then begin
  ; read parameter file (a structure file or structure)
  case datatype(readprams) of
    'STR': restgen,inarray,file=readprams
    'STC': inarray=readprams
    else: message, 'must provide a named readprams file or a structure'
  endcase
  t=tag_names(inarray)
  for i=0,n_elements(t)-1 do void=execute(t[i]+'_rd=inarray.(i)')
endif

;Parameters
;If keyword set for a given parameter, then this is used.
;If keyword not specified, then use value from readparams if set. If readparams not set,
;uses the default values as listed in these following statements


;  Gibson, Bagenal & Low 1986
;  Case 1 (except, apar=0)
  
apar=n_elements(apar0) eq 0?(n_elements(apar_rd) eq 0?0.0:apar_rd):apar0
bpar=n_elements(bpar0) eq 0?(n_elements(bpar_rd) eq 0?2.78d0:bpar_rd):bpar0
gamma1=n_elements(gamma10) eq 0?(n_elements(gamma1_rd) eq 0?-10d0:gamma1_rd):gamma10
gamma3=n_elements(gamma30) eq 0?(n_elements(gamma3_rd) eq 0?-1.2d0:gamma3_rd):gamma30
vscale=n_elements(vscale0) eq 0?(n_elements(vscale_rd) eq 0?0.0:vscale_rd):vscale0

;
; for testing only -- comparing normalizations from original GBL paper
;  and original Low paper.
;  should also changed to commented values of norm1, norm2 in gigbaglow.pro
;    default,bpar,4.
;    default,gamma1,45d0*bpar*bpar
;    default,gamma3,-4d0

;Plasma parameters
hydro=n_elements(hydro0) eq 0?(n_elements(hydro_rd) eq 0?3:hydro_rd):hydro0

if hydro eq 0 then begin
  print,'hydro = 0 not allowed. Resetting to default hydro=3 (Vasquez profiles)'
  hydro=3
endif

if exist(cdensprof) then cdensprofsave=cdensprof
if exist(ct0) then ct0save=ct0
for_hydrodefaults,$
        hydro=hydro,cdensprof=cdensprof,ct0=ct0,$
        odensprof=odensprof,oT0=oT0,$
        rcdensprof=cdensprof_rd,rct0=ct0_rd,$
        rodensprof=odensprof_rd,rot0=oT0_rd,$
        vodensprof=vodensprof,vcdensprof=vcdensprof
;
; note the HYDRO = 4 requires rerunning for_hydrodefaults
;  with HYDRO=3 for the closed field regions
;

hydrosave=hydro
if hydro eq 4 then begin
  for_hydrodefaults,$
        hydro=3,cdensprof=cdensprofsave,ct0=ct0save,$
        rcdensprof=cdensprof_rd,rct0=ct0_rd,$
        vcdensprof=vcdensprof
  cdensprof=cdensprofsave
  ct0=ct0save
endif
hydro=hydrosave

; save parameters to a file (stored in file named saveprams if it is a string)

pramarray={Name:'gibbaglow',Apar:apar,Bpar:bpar,$
hydro:double(hydro),cdensprof:cdensprof,ct0:ct0,odensprof:odensprof,ot0:ot0,$
Gamma1:gamma1,Gamma3:gamma3,vscale:vscale} 

if keyword_set(saveprams) then begin
     savefilename=saveprams
     if n_elements(working_dir) eq 1 and datatype(saveprams) eq 'STR' then if working_dir ne '' then savefilename=working_dir+slash+saveprams

     case 1 of
         datatype(saveprams) eq 'STR': savegen,pramarray,file=savefilename,/replace
         else: saveprams=pramarray
     endcase
endif

; now calculate output structure values (the ones the model uses)

outarray={Name:'gibbaglow',Label:'Gibson, Bagenal and Low',Apar:apar,Bpar:bpar,$
hydro:double(hydro),cdensprof:VCdensprof,ct0:ct0,odensprof:VOdensprof,ot0:ot0,$
Gamma1:gamma1,Gamma3:gamma3,Magmod:1,Vscale:vscale,Velimpose:0} 

;print,'goodbye prams'
end

;**********************************************************************
