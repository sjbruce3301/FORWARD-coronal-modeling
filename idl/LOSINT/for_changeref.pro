;************************************************************************

	pro for_changeref,thpos,StokesQ,StokesU,Qprime,Uprime,type=type

;
;  Name:  FOR_CHANGEREF
;
;  rotates Stokes vector from one reference to another
;
; Called by FOR_INTCOMPLOSE, FOR_PLOT, FOR_PLOTSTOKESLINES
;
; Written by Sarah Gibson 2014
; Version 2.0 July 2014

; RPOS, THPOS - plane of sky coordinates
;

; STOKESQ,U -  in original frame

; calculates Qprime, Uprime in rotated frame


; Keyword TYPE
;	0 - from N-S to radial
;	1 - from E-W to radial
;	2 - from radial to N-S
;	3 - from radial to E-W
;	4 - from E-W to N-S
;	5 - from 45 degrees to radial
;	6 - from radial to 45 degrees

	default,type,0
;
;  see Casini&Judge 1999
; 

        beta=-1.*thpos


        if type ne 4 then begin
	 case type of
		0: gamma=-1.*beta
		1: gamma=beta+!dpi/2.d0
		2: gamma=beta
		3: gamma=-1.*(beta+!dpi/2.d0)
		5: gamma=(beta+!dpi/4.d0)
		6: gamma=-1.*(beta+!dpi/4.d0)
         endcase

	 Qprime =  StokesQ*cos(2.d0*gamma) +  StokesU*sin(2.d0*gamma)

	 Uprime = -1.*StokesQ*sin(2.d0*gamma) +  StokesU*cos(2.d0*gamma)
        endif else begin
         gamma=beta+!dpi/2.d0
	 Qp1 =  StokesQ*cos(2.d0*gamma) +  StokesU*sin(2.d0*gamma)
	 Up1 = -1.*StokesQ*sin(2.d0*gamma) +  StokesU*cos(2.d0*gamma)
         gamma=beta
	 Qprime =  Qp1*cos(2.d0*gamma) +  Up1*sin(2.d0*gamma)
	 Uprime = -1.*Qp1*sin(2.d0*gamma) +  Up1*cos(2.d0*gamma)
        endelse

	end
