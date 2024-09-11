pro for_hydrodefaults,$
	hydro=hydro,$
	cdensprof=cdensprof,ct0=ct0,$
	odensprof=odensprof,ot0=ot0,$
	rcdensprof=cdensprof_rd,rct0=ct0_rd,$
	rodensprof=odensprof_rd,rot0=ot0_rd,$
	vcdensprof=vcdensprof,vodensprof=vodensprof

;
; Since multiple models use the hydrostatic option in various ways,
; it makes sense to drive them with one program
;
;
; INPUT KEYWORDS
;
;  HYDRO: hydrostatic atmospheric models
;
;    CALLED by models (within [model]*prams.pro):
;	CROISSANT (for spherically symmetric open field background)
;	MYDIPOLE (for both open and closed field)
;	GIBBAGLOW (for both open and closed field)
;	NUMCUBE (for spherically symmetric open field background "outside the box", 
;  	  and/or if keyword TOPOLOGY set, to set open vs closed field density profiles)
;	PFSSMOD (for spherically symmetric open field density,
;  	  or if keyword TOPOLOGY set, to set open vs closed field density profiles)
;	PSIMAS (for open field background "outside the box")
;
;	The options are:
;             HYDRO=0 - zero density 
;		not allowed for MYDIPOLE, GIBBAGLOW, or PFSSMOD --  replaced with hydro=3
;             HYDRO=1; exponential isothermal hydrostatic equilibrium
;             HYDRO=2; radial power law hydrostatic equilibrium
;             HYDRO=3; Vasquez et al 2003 density, electron temperature
;                       DEFAULT
;             HYDRO=4; Cranmer et al 1999 empirical model for coronal hole density, temperature
;		only allowed for open field profiles
;		if set, any closed fields will revert to HYDRO=3 representation
;             HYDRO=5; simpler version of HYDRO=2, with only one radial power law (for far field)
;	      HYDRO > 5 --> HYDRO=3
;
; KEYWORDS SET IN FOR_HYDRODEFAULTS
;
;  CDENSPROF,ODENSPROF -
;	HYDRO=0, CDENSPROF,ODENSPROF not used, set to 'NULL' and won't show up in widget
;       HYDRO=1, CDENSPROF, ODENSPROF represent density at coronal base in CGS units, 
;		CDENSPROF scaled by 1d9
;		ODENSPROF scaled by 1d8
;       HYDRO=2, CDENSPROF, ODENSPROF can be input array [A,B,C,D,E,F] (units in cgs)
;               or multipliers of array [densprof*A,B,densprof*C,D,densprof*E,F]
;                     (note for widget it has to be multiplier, so, scalar)
;	 	closed field array defaults to values of Gibson et al 1999 WSM streamer
;	      	open field array defaults to values of Guhathakurta et al 1999 WSM c. hole
;               dens = A*r^-B + C*r^-D + E*r^-F
;       HYDRO=3, CDENSPROF,ODENSPROF can be array [A1,A2,A3,A4,A5,aa,bb,alpha,beta]
;              or multipliers of A1
;              closed field array defaults to Vasquez/Sittler-Guhathakurta streamer values
;	       open field array defaults to Vasquez/Sittler-Guhathakurta polar
;              dens=A1*exp(A2/r)*r^-2*(1+A3/r+A4/r^2+A5/r^3)
;       HYDRO=4,ODENSPROF can be array [da,db,dc,dd,de,ta,tb,tc,td]
;              or multipliers of da
;              open field array defaults to Cranmer coronal hole values 
;              dens_he= da*1e5*(db*(1./r)^dc + dd*(1./r)^de)
;		(for HYDRO=4 CDENSPROF set to 'NULL' and for_hydrodefaults will 
;			   be called twice, the second time with HYDRO=3) 
;       HYDRO=5, ODENSPROF CDENSPROF are density at coronal base 
;              density=densprof*1d7r^T0
;
;        DEFAULT CDENSPROF=1 (CDENSPROF=6 for HYDRO=5)
;        DEFAULT ODENSPROF=1
;
;  CT0, OTO
;      HYDRO=0,1, isothermal temperature parameter value
;      HYDRO=2 -- not used set to null and not shown in widget
;		(temperature follows from density profile, ideal gas law, hydrostatic pressure balance)
;      HYDRO=3 scaling factor for model, divided by 1.5d6, and uses parameters in DENSPROF
;              temp=T0*(8e5/1.5e6)*(aa+1)/(aa+bb*r^alpha + (1-bb)*r^-beta)
;		(normalized to 1.5e6 so same T0 default can be used as other HYDRO choices)
;      HYDRO=4 scaling factor for model, divided by 1.5d6, and uses parameters in DENSPROF
;              temp_he= T0*(1e6/1.5e6)*(ta*r^tb + tc*r^td)^(-1)
;		(normalized to 1.5e6 so same T0 default can be used as other HYDRO choices)
;			(for HYDRO=4 CTO set to 'NULL'and for_hydrodefaults will 
;			   be called twice, the second time with HYDRO=3) 
;        DEFAULT CTO 1.5D6
;        DEFAULT OTO 1.D6
;		(except for HYDRO=3 and 4 -- DEFAULT OTO 1.5D6)
;      HYDRO=5 -- defines slope of radial falloff; designed for far field falloff
;		(temperature follows from density profile, ideal gas law, hydrostatic pressure balance)
;        DEFAULT CTO =-4 OTO=-2
;
;  BOOKKEEPING
;
;  R* -- input keywords set in *prams.pro, only set if readprams is set
;
;
; OUTPUT KEYWORDS
;
;	VODENSPROF, VCDENSPROF
;	  Full vectors for for_hydrocalc to use
;

if hydro gt 5 then begin
 print,'hydro must be 5 or less. Resetting to default hydro=3 (Vasquez profiles)'
 hydro = 3
endif

oduse='1.'
cduse='1.'
if hydro eq 5 then cduse='6.'
cdensprof=n_elements(cdensprof) eq 0?(n_elements(cdensprof_rd) eq 0?cduse:cdensprof_rd):$
   (strupcase(cdensprof) eq 'NULL'?'1.':cdensprof)
odensprof=n_elements(odensprof) eq 0?(n_elements(odensprof_rd) eq 0?oduse:odensprof_rd):$
   (strupcase(odensprof) eq 'NULL'?'1.':odensprof)
if hydro eq 0 then begin
 cdensprof='NULL'
 odensprof='NULL'
endif
if hydro eq 4 then begin
 cdensprof='NULL'
endif

case 1 of 
	hydro eq 5: begin
	 otset='-2.' 
	 ctset='-4.'
	 end
        hydro eq 3 or hydro eq 4: begin
         otset='1.5d6'
	 ctset='1.5d6'
	 end
        else: begin
	 otset='1.d6'
	 ctset='1.5d6'
	end
endcase

ct0=n_elements(ct0) eq 0?(n_elements(ct0_rd) eq 0?ctset:ct0_rd):$
   (strupcase(ct0) eq 'NULL'?ctset:ct0)
ot0=n_elements(ot0) eq 0?(n_elements(ot0_rd) eq 0?otset:ot0_rd):$
   (strupcase(ot0) eq 'NULL'?otset:ot0)
if hydro eq 2 then begin
 ct0='NULL'
 ot0='NULL'
endif
if hydro eq 4 then begin
 ct0='NULL'
endif


;
; make output vectors
;

Case 1 of
  hydro eq 0 : begin
    vcdensprof = 'NULL'
    vodensprof = 'NULL'
  end
  hydro eq 1 : begin
    if n_elements(cdensprof) eq 1 then vcdensprof = string(1000000000.*cdensprof) $
      else begin
        print,'exponential case requires scalar densprof; assuming default base density 1d9'
        vcdensprof = '1d9'
      endelse
    if n_elements(odensprof) eq 1 then vodensprof = string(100000000.*odensprof) $
      else begin
        print,'exponential case requires scalar densprof; assuming default base density 1d8'
        vodensprof = '1d8'
      endelse
  end
  hydro eq 2 : begin
     vcdensprof=string([cdensprof*360000000.,15.3,cdensprof*99000000.,7.34,cdensprof*36500000.,4.31])
; radial power law from WSM streamer in Gibson et al., 1999
     vodensprof=string([odensprof*173690000.,13.72,odensprof*1995000.,4.09,odensprof*131600.,2.])
; radial power law from WSM coronal hole in Guhathakurta et al., 1999
  end
  hydro eq 3 : begin
     vcdensprof=string([cdensprof*0.0032565,3.6728,4.8947,7.6123,5.9868,0.1,0.33,0.55,6.6])
     vodensprof=string([odensprof*0.0012921,4.8039,0.29696,-7.1743,12.321,0.,0.47,0.7,6.6])
; density radial power laws from Sittler-Guhathakurta 1999 (as described in Vasquez 2003)
; temperature Vasquez 2003 for streamer and coronal hole
  end
  hydro eq 4 : begin
     vodensprof=string([odensprof,3890.,10.5,8.69,2.57,0.35,1.1,1.9,-6.6])
; temperature, density from Cranmer 1999 coronal hole 
     vcdensprof='NULL'
  end
  hydro eq 5 : begin
     vodensprof=string(10000000.*odensprof)
     vcdensprof=string(10000000.*cdensprof)
  end
endcase

end
