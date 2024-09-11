;************************************************************************
     pro for_intmem,memory,nunwrap,nx,nbite,nmem,verbose=verbose,datadump=datadump

;+
; Name: FOR_INTMEM
;
; Purpose:  	figure out memory chunks
;
; INPUTS
;	memory (whether to chunk)
;	nunwrap (total array length)
;	nx (original array x dimension if there is one - default chunk if memory=1)
;
; KEYWORD INPUT
;	datadump (turns off chunking to allow output file dump)
; OUTPUTS
;	nbite
;	nmem
;
; Called by: FOR_INTENSINT
;
; History: Written by Sarah Gibson 2015
;-

; If no memory set, use nunwrap as size of the one bite.
; if datadump set, need to turn off memory loop so as to save
; the whole cube in datadump

        if memory eq 0 or datadump eq 1 then begin
           nbite=1
           nmem=nunwrap
        endif else begin

; Default setting is to use Ny bites of length Nx (one for each row), but
; can use memory setting to override:

           if memory eq 1 then nmem=nx else nmem= memory

;  memory = -1 is actually how to have it loop every pixel

           nmem=abs(nmem)

           nmem = long(nmem)

; Number of line of sight integrals per bite: Using ceil will
; force a leftover (smaller amount) if nunwrap/nmem is not an integer.

           nbite=long(ceil(nunwrap/double(nmem)))
           if verbose ne 0 then $
                 print,'for_intensint: number of bites = ',nbite

        endelse

end
