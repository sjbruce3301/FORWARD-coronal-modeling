;************************************************************************
;+
pro pointvalprams,outarray,date=date,working_dir=working_dir,$
		      ptbrval=ptbrval0,ptbthval=ptbthval0,ptbphval=ptbphval0,$
		      ptdensval=ptdensval0,pttempval=pttempval0,$
                      velimpose=velimpose0,saveprams=saveprams,readprams=readprams0

;
;Name: pointvalprams
;
;Purpose: Arbitrary definition of physical state at a point
;
;;Keyword Inputs:  
;		   
;         PHYSICAL STATE PARAMERERS
;	 	 PTBRVAL
;        	 PTBTHVAL
;        	 PTBPHVAL
;          	 PTDENSVAL
;         	 PTTEMPVAL
;
; 	VELIMPOSE 
;		Allows imposition of field-directed velocity
;		Default 0
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
;         PHYSICAL STATE PARAMERERS
;	 	 BRVAL
;        	 BTHVAL
;        	 BPHVAL
;          	 DENSVAL
;         	 TEMPVAL
;		 VELIMPOSE
;
;
;		NAME -- pointval-- so that procedure can be called in intensint
;		LABEL -- Point Evaluation -- for plot label
;
;Output: outarray - structure containing keyword output model parameters 
;
;Common Blocks: None
;
; written S. E. Gibson, Jan 13, 2010
;	Added Velimpose, Sep 18, 2016
;	Jun 2019 - used slash for PC compatibility
;	Dec 2023 -- fixed bug where parameters array passed through Bth instead of Br
;- 

slash=path_sep()

if n_elements(ptbrval0) ne 0 then ptbrval=ptbrval0
if n_elements(ptbthval0) ne 0 then ptbthval=ptbthval0
if n_elements(ptbphval0) ne 0 then ptbphval=ptbphval0
if n_elements(ptdensval0) ne 0 then ptdensval=ptdensval0
if n_elements(pttempval0) ne 0 then pttempval=pttempval0
if n_elements(velimpose0) ne 0 then velimpose=velimpose0

if n_elements(saveprams) ne 0 then saveprams=saveprams
if n_elements(readprams0) ne 0 then readprams=readprams0

if not keyword_set(readprams) then begin

; 
; set up defaults if necessary
;

   default,ptbrval,10.
   default,ptbthval,10.
   default,ptbphval,10.
   default,ptdensval,1.d8
   default,pttempval,1.d6
   default,velimpose,0

endif else begin

; read parameter file (a structure file)

   case 1 of 
      datatype(readprams) eq 'STR': restgen,readarray,file=readprams
     'STC': readarray=readprams
      else: message, 'must provide a named readprams file or a structure'
   endcase

   default,ptbrval,readarray.ptbrval
   default,ptbthval,readarray.ptbthval
   default,ptbphval,readarray.ptbphval
   default,ptdensval,readarray.ptdensval
   default,pttempval,readarray.pttempval
   default,velimpose,readarray.velimpose

endelse


; save parameters to a file (stored in file named saveprams if it is a string)

pramarray={Name:'pointval',PtBrVal:ptbrval,PtBthVal:ptbthval,PtBphVal:ptbphval,PtDensVal:ptdensval,PtTempVal:pttempval,VelImpose:velimpose}

if keyword_set(saveprams) then begin
     savefilename=saveprams
     if n_elements(working_dir) eq 1 and datatype(saveprams) eq 'STR' then if working_dir ne '' then savefilename=working_dir+slash+saveprams 

     case 1 of
         datatype(saveprams) eq 'STR': savegen,pramarray,file=savefilename,/replace
         else: saveprams=pramarray
     endcase
endif

; now calculate output structure values (the ones the model uses)

outarray={Name:'pointval',Label:'Point Evaluation',BrVal:ptbrval,BthVal:ptbthval,BphVal:ptbphval,DensVal:ptdensval,TempVal:pttempval,VelImpose:velimpose,MagMod:1}

end
;**********************************************************************
