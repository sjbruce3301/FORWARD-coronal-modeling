;************************************************************************
;+
function for_los_prams,losuse,losmin,losint,nlos,$
		axisym,nostretch,bang,thetao,dodisk,incres,occult,upoccult
;
;Name: FOR_LOS_PRAMS
;
;Purpose: To create structure containing information concerning the line of sight to be integrated over
;
;;Keyword Inputs:
;
;      LOSUSE-  integrating over LOS angle TAU (default) or LOS distance XLOS
;			and now also LOS distance along not-necessarily parallel
;			line of sight TLOS (centered on Thomson Sphere)
;
;      NOSTRETCH -- if set and LOSUSE=TLOS, 
;               will not stretch TLOS step with elongtion angle 
;               but rather stick to same LOSMIN and LOSINT for all 
;               lines of sight
;			(see for_losint.pro)
;
;      LOSMIN -  minimum line of sight angle 
;			LOSUSE = TAU default -pi/2 for infinite LOS
;                       LOSUSE = XLOS default -1.
;				note tau=0 / xlos = 0 is plane of the sky
;			LOSUSE = TLOS default -1
;				but scaled by elongation in for_intlos
;				note tau=0 / tlos = 0 is Thomson Sphere
;      LOSINT -  resolution in along the line of sight, default 0.01 (radians)
;			LOSINT = TAU default 0.01 (RADIANS)
;                       LOSINT = XLOS default 0.01 (RSUN)
;                       LOSINT = TLOS default 0.01 (RSUN)
;      NLOS - number of points to integrate along line of sight
;			default generally determined for symmetry`
;			if AXISYM set, cut in half
;
;      AXISYM - default = 'NULL'= 0; if axisymm set, NLOS is halved and LOS integration doubled
;      DODISK - default = 0; if dodisk set, calculate on disk  REQUIRES XLOS
;      INCRES - what to do above disk - INCRES=1 means double resolution	
;	   INCRES=0 means throw away half the points (the ones that would
;		have been behind the disk)
;      BANG -- Bang solar B angle (INPUT IN DEGREES), default zero
;      OCCULT, UPOCCULT - lower and upper limits for LOS 
;	WHITEDISK - color to plot occulter
;
;;Output: structure with info in it
; 	OutStruct structure contains fields
;		LOSUSE - as inputted (or default) UNLESS DODISK NE 0 --> XLOS
;
;		LOSMIN,LOSINT,NLOS - as inputted (or default) plus
;                        adjustment to allow integer nlos and if 
;			necessary to account for AXISYM
;
;		DODISK - passed through as inputted
;		AXISYMMULT - default = 1; if AXISYM keyword set, set to 2 to double LOS integration 
;		BANG -- bang as inputted (or default) and OUTPUT CONVERTED TO RADIANS
;			note nonzero bang requires call to procedure FOR_THROT in FOR_DOROTS
;		THETAO -- thetao as inputted (or default) and OUTPUT CONVERTED TO RADIANS
;			note thetao ne 90 requires call to procedure FOR_THROT in FOR_DOROTS
;
;Common Blocks: None
;
;
; Called by FOR_GETSTRUCTS
;
;Written: S. Gibson Jan. 22, 2010
; Version 2.0 July 2014
;  March 2022-- commented out overwrite for null settings los 
;    should not happen
;  April 2022- added nostretch
;  Aug 2023 -- dealt with special case USER PB giving dodisk and occult both 0

mdtor=!dpi/180d0

;
; special case dodisk and occult both 'NULL' can happen
; for USER and white light. 
if strupcase(string(occult)) eq 'NULL' and strupcase(string(dodisk)) eq 'NULL' then begin
 occult=1.
 dodisk=0.
endif

if strupcase(string(occult)) eq 'NULL' then occult=0.
if strupcase(string(upoccult)) eq 'NULL' then upoccult=1000.
if strupcase(string(dodisk)) eq 'NULL' then dodisk=0.d0
dodisk=double(dodisk)

;
; below should not happen for losuse
;  and if it happens for the others they shouldn't be changed 
;if losuse eq 'NULL' then losuse = 'TAU'
;if string(losmin) eq 'NULL' then losmin=-!dpi/2.d0
;if string(losint) eq 'NULL' then losint=0.01
;if string(nlos) eq 'NULL' then nlos=315

;
; set up outarray
;

outarray={LosUse:losuse,LosMin:losmin,LosInt:losint,NLos:nlos,NoStretch:nostretch,$
	AxiSymMult:1.d0,Bang:double(bang*mdtor),Thetao:double(thetao*mdtor),DoDisk:dodisk,Incres:incres,Occult:occult,UpOccult:upoccult}

if strupcase(string(axisym)) eq 'NULL' then axisym = 0
if axisym ne 0. then begin
	outarray.NLos = outarray.NLos/2
	outarray.AxiSymMult = 2.d0
endif

;print,'dodisk in for_los_prams',dodisk
;print,'occult in for_los_prams',occult
return,outarray

end
