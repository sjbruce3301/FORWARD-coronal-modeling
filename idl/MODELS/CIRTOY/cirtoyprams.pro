PRO CIRTOYPRAMS,outarray,date=date,working_dir=working_dir,$
		   alfa=alfa0,bta=bta0,n_o=n_o0,wwidth=wwidth0,time=time0,$
	           phimin=phimin0,phimax=phimax0,vsw=vsw0,$
		   odensprof=odensprof,oT0=oT0,$
		   saveprams=saveprams,readprams=readprams
;+
;
;Name: CIRTOYPRAMS
;
;Purpose: To create structure containing information about a CIR density model;
; To be called by driver routine and resulting structure will be named 
; ModPramsStruct (with ModPramsStruct.name='cirtoy')
;
; Called by FOR_MODELDEFAULTS
;
;Keyword Inputs:
;
; PHYSICAL PROPERTIES
;
;
;  ALFA: 
;	controls steepness of spiral
;	alfa = value in units of
;           ratio of V_sw (Rsun/hour)/Omega(radians/hour)
;	    (Rsun/radian)
;       Omega=2.*!pi/28./24. = 0.00934998
;	V_sw default=400 km/sec
;	V_sw_rsunperhour=400.*3600./6.96d5 = 2.0689655
;       V_sw/Omega = 221.28024
;	alfadefault, 0.1 (few days flow)
;
;  BTA:
;	controls shape in plane perpendicular to equatorial plane
;	if its unity then shape of circle, higher powers make it more oblate (flatter)
;	default, 2 (unitless)
;
;  N_0:
;	controls density of CIR - multiplier on background
;	default, 100 (unitless)
;
;  WWIDTH:
;	Gaussian width, thickness of CIR
;	default, 5 Rsun
;  
;  TIME:
;	first guess, time proportional to phi
;	default,0 (units of hour)
;
;  PHIMIN, PHIMAX -- use a truncated piece of original spiral
;       default,phimin,pi/4
;       default,phimax,2*pi
;		full spiral
;
;  VSW:
;	default,400 (units of km/s)
;
; ODENSPROF, OT0:
;  how to handle the plasma throughout the heliosphere
;  Force HYDRO=5, single power law density dropoff
;	ODENSPROF*r^oT0
;       DEFAULT OT0=-2 (set in for_hydrodefaults)
;	DEFAULT ODENSPROF=1 (set in for_hydrodefaults)

;
; BOOKKEEPING
;
;
;
; SAVEPRAMS - if keyword set to a string, write parameters to filename
;                       (which is the keyword value saveprams)
;                       or if set to 1, then replace with pramarray
;
;
; READPRAMS - if keyword set, read in parameters from filename
;                       (which is the keyword value filename)
;                       or if a structure than read directly
;                       NOTE, flagged keywords will overwritten
;
; ModPramsStruct Outputs:
;
;               As above, plus
;
;               NAME -- cirtoy-- so that procedure can be called in intensint
;               LABEL -- CIR Toy Model -- for plot label
;               MAGMOD -- 0 -- meaning it is not a magnetized model
;               ODENSPROF -- multiplied by 1d7 in FOR_HYDRODEFAULTS

;
;Output: outarray - structure containing keyword output model parameters
;
;Common Blocks: None
;
; Author and history:
;	Written by Sarah Gibson July 2024
;-

slash=path_sep()

COMPILE_OPT IDL2 ;default long and square brackets for array subscripts

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

alfa=n_elements(alfa0) eq 0?(n_elements(alfa_rd) eq 0?0.1:alfa_rd):alfa0
;0.1

bta=n_elements(bta0) eq 0?(n_elements(bta_rd) eq 0?2.0:bta_rd):bta0
;2

n_o=n_elements(n_o0) eq 0?(n_elements(n_o_rd) eq 0?100.0:n_o_rd):n_o0
;100

wwidth=n_elements(wwidth0) eq 0?(n_elements(wwidth_rd) eq 0?5.0:wwidth_rd):wwidth0
;5

time=n_elements(time0) eq 0?(n_elements(time_rd) eq 0?0.0:time_rd):time0
;0

phimin=n_elements(phimin0) eq 0?(n_elements(phimin_rd) eq 0?!dpi/4.d0:phimin_rd):phimin0
;pi/4 

phimax=n_elements(phimax0) eq 0?(n_elements(phimax_rd) eq 0?2.*!dpi:phimax_rd):phimax0
;2pi

vsw=n_elements(vsw0) eq 0?(n_elements(vsw_rd) eq 0?400.0:vsw_rd):vsw0
;400

for_hydrodefaults,$
        hydro=5,odensprof=odensprof,ot0=ot0,$
        rodensprof=odensprof_rd,rot0=ot0_rd,$
        vodensprof=vodensprof

pramarray={name:'mydipole',alfa:alfa,bta:bta,n_o:n_o,wwidth:wwidth,time:time,phimin:phimin,phimax:phimax,vsw:vsw,$
	           odensprof:odensprof,ot0:ot0} 

; if requested, save input parameters to a file (stored in file named saveprams if it is a string)

if keyword_set(saveprams) then begin

     savefilename=saveprams
     if n_elements(working_dir) eq 1 and datatype(saveprams) eq 'STR' then begin
      if working_dir ne '' then savefilename=working_dir+slash+saveprams
     endif

     case 1 of
         datatype(saveprams) eq 'STR': savegen,pramarray,file=savefilename,/replace
         else: saveprams=pramarray
     endcase
endif


label='CIR Toy Model'
name='cirtoy'
outarray={Name:name,$
          Label:label,$
	  Alfa:alfa,$
	  Bta:bta,$
	  N_o:n_o,$
	  wwidth:wwidth,$
	  phimin:phimin,$
	  phimax:phimax,$
	  time:time,$
	  vsw:vsw,$
	  MagMod:0,$
	  ODensProf:VODensProf,$
          OT0:OT0}
END
