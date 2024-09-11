;************************************************************************
;+
pro liteslowprams,outarray,date=date,working_dir=working_dir,$
		      frontheight=frontheight0,legsang=legsang0,$
		      ao=ao0,alnotrbub=alnotrbub0,velimpose=velimpose0,$
		      sig1=sig10,ll_out=out0,densscale=densscale0, $
                      aa=aa0,bb=bb0,cc=cc0,dd=dd0,ee=ee0,ff=ff0,$
		      saveprams=saveprams,readprams=readprams0
;
;Name: liteslowprams
;
;Purpose: To create structure containing information concerning the
;         Lites-Low CME model; To be called by driver routine and resulting
;         structure will be named ModPramsStruct (with ModPramsStruct.name='liteslow')
;
;;Keyword Inputs:  
;		   
;         MORPHOLOGY/POSITION
;
;		FRONTHEIGHT, LEGSANG-- these are the position of the front
;			of the bubble and the angle subtended by its legs
;			at the bubble's broadest point, 
;			legsang INPUT in DEGREES
;			these inputs will determine 
;			OUTPUTS xo and rbub (see below)	
;			DEFAULT FRONTHEIGHT=1.35, LEGSANG=25.
;
;         PHYSICAL PROPERTIES
;
;
;		AO - this is the constant of proportionality between
;	   		the stream function S and the pressure Pi
;			DEFAULT AO=1.75  OUTPUT multiplied by -1/Muse
;			where Muse = (2./3.)*ao*alnot*alnot
;			This is because the roots alnot chosen in 
;			liteslow vs giblow end up with B vs. -B 
;			-- same handedness, but opposite sign.
;			Muse allows us to input ao with a value that ends up
;			equivalent to the maximum Br (bubble coords)
;			
;
;	     	ALNOTRBUB - the eigenvalue parameter of the twisted flux
;	    		rope model -- OUTPUT alnot (rbub calculated below)
;			DEFAULT (unlikely to change) ALNOTRBUB=4.4935
;			NOTE THIS HAS BEEN REMOVED FROM THE WIDGET PARAMETERS
;			AND SAVEPRAMS
;
; 		SIG1 - the rotation about the spheromak core (rotation
;           		about x-axis)  INPUT in DEGREES
;			DEFAULT SIG1=90  OUTPUT in RADIANS
;
;		(LL_)OUT -  a parameter which scales the outer field solution
;			since there is a discontinuity around bubble for field
;			(and total pressure) this can be used to scale external
;			field as desired  DEFAULT out= 0.1 OUTPUT unchanged
;
;		DENSSCALE - a parameter that scales the outer density
;			similar concept to OUT
;
;    		AA,BB,CC,DD,EE,FF - background density, pressure parameters, 
;			for hydrostatic equilibrium OUTPUT as above but
;			aa, cc, ee  multiplied by 1d8 
;			see below for DEFAULT
;
;  VELIMPOSE - impose a velocity of constant magnitude VELIMPOSE directed along the field
;               overwrites any velocity field already loaded in if nonzero
;                       DEFAULT 0.d0
;
;
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
;;ModPramsStruct Outputs:  
;
;		NAME -- liteslow-- so that procedure can be called in intensint
;		LABEL -- Lites and Low -- for plot label
;
;		XO - this is the shift coordinates between
;	   		the bubble system and the sun's coordinate system.
; 	   		A choice of xo =1 should put the bubble at
;	   		the solar surface at the equator.  
;			Determined from frontheight, legsang, 
;			as evaluated at 
;
;		RBUB - this is the radius of the bubble.  Determined from 
;			values of frontheight, xo  
;	
;		ALNOT -- this is the input alnotrbub/rbub
;
;		SIG1 -- as above but converted to RADIANS
;
;		AA, CC, EE  -- as above but multiplied by 1d8
;
;		AO, OUT, PIO, BB, DD, FF, - as inputted
;		
;
;
;;Output: outarray - structure containing keyword output model parameters 
;
;Common Blocks: None
;
;Written: Giblow-  J. Dove, Jan 25, 2010
; revised S. E. Gibson, Feb 1, 2010
; revised to be liteslow -- JD Apr 2010
; revised to allow over-writing individual parameters after reading a
; saved parameter file (using savepram) -- JD May 24 2010
; change to procedure and adjusted readprams/saveprams I/O Jan 2013 SEG
;	June 2019 used slash for PC compatibility
;- 


slash=path_sep()

if n_elements(frontheight0) ne 0 then frontheight=frontheight0
if n_elements(legsang0) ne 0 then legsang=legsang0
if n_elements(ao0) ne 0 then ao=ao0
if n_elements(alnotrbub0) ne 0 then alnotrbub=alnotrbub0
if n_elements(sig10) ne 0 then sig1=sig10
if n_elements(out0) ne 0 then out=out0
if n_elements(velimpose0) ne 0 then velimpose=velimpose0
if n_elements(densscale0) ne 0 then densscale=densscale0
if n_elements(aa0) ne 0 then aa=aa0
if n_elements(bb0) ne 0 then bb=bb0
if n_elements(cc0) ne 0 then cc=cc0
if n_elements(dd0) ne 0 then dd=dd0
if n_elements(ee0) ne 0 then ee=ee0
if n_elements(ff0) ne 0 then ff=ff0

if n_elements(saveprams) ne 0 then saveprams=saveprams
if n_elements(readprams0) ne 0 then readprams=readprams0


if not keyword_set(readprams) then begin


   default,frontheight,1.35d0
   default,legsang,25.d0
   default,ao,1.75d0
   default,sig1,90.d0
   default,out,0.1d0
   default,densscale,1.d0
   default,aa,1.27d0
   default,bb,22.8d0
   default,cc,5.97d0
   default,dd,16.d0
   default,ee,1.57d0
   default,ff,3.87d0
;   default,aa,8.d0
;   default,bb,24.d0
;   default,cc,4.9d0
;   default,dd,6.d0
;   default,ee,0.07d0
;   default,ff,3.d0
;   default,aa,3.6d0
;   default,bb,15.3d0
;   default,cc,0.99d0
;   default,dd,7.34d0
;   default,ee,0.365d0
;   default,ff,4.31d0
   
   default,alnotrbub,4.4935

   default,velimpose,0.d0
   
   
endif else begin
   ; read parameter file
   case 1 of 
      datatype(readprams) eq 'STR': restgen,readarray,file=readprams
     'STC': readarray=readprams
      else: message, 'must provide a named readprams file or a structure'
   endcase
   
   if tag_exist(readarray,'velimpose') then default,velimpose,readarray.velimpose else default,velimpose,0.d0
   default,frontheight,readarray.FrontHeight
   default,legsang,readarray.LegsAng
   default,ao,readarray.Ao
   default,sig1,readarray.Sig1
   if tag_exist(readarray,'ll_out') then default,out,readarray.LL_Out else $
       default,out,readarray.Out
   default,densscale,readarray.Densscale
   default,alnotrbub,readarray.AlnotRbub
   default,aa,readarray.AA
   default,bb,readarray.BB
   default,cc,readarray.CC
   default,dd,readarray.DD
   default,ee,readarray.EE
   default,ff,readarray.FF
endelse

; save parameters to a file (stored in file named saveprams if it is a string)
if keyword_set(saveprams) then begin
     savefilename=saveprams
     if n_elements(working_dir) eq 1 and datatype(saveprams) eq 'STR' then if working_dir ne '' then savefilename=working_dir+slash+saveprams

   pramarray={Name:'liteslow',FrontHeight:frontheight,LegsAng:legsang,Ao:ao,$
              Sig1:sig1,LL_Out:out,Densscale:Densscale,AA:aa,BB:bb,CC:cc,DD:dd,EE:ee,FF:ff,velimpose:velimpose}
     case 1 of
         datatype(saveprams) eq 'STR': savegen,pramarray,file=savefilename,/replace
         else: saveprams=pramarray
     endcase
endif


   mdtor=!dpi/180d0
   xo=(frontheight)/(1.+tan(legsang/2.*mdtor))
   rbub=frontheight - xo 

   sig1=sig1*mdtor

   alnot=alnotrbub/rbub

   Muse = (2./3.)*alnot*alnot
   ao = -ao/Muse


   outarray={Name:'liteslow',Label:'Lites and Low',Xo:xo,Rbub:rbub,Ao:ao,Alnot:alnot, $
          Sig1:sig1,Out:out,densscale:densscale,AA:aa*1d8,BB:bb,CC:cc*1d8,DD:dd,EE:ee*1d8,FF:ff,MagMod:1.,velimpose:velimpose}


end

;**********************************************************************
