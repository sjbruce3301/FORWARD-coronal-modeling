;************************************************************************
     pro for_intcompsetup,ObsPramsStruct,LosPramsStruct,nunwrap,working_dir,comprun,lun,verbose=verbose,nowidgmess=nowidgmess,alin=alin

;+
; Name: FOR_INTCOMPSETUP
;
; Purpose:  sets up for polarimetry (comp-type) integration
;
; INPUTS
;	ObsPramsStruct,LosPramsStruct,nunwrap,working_dir
;
; KEYWORD INPUTS
;		verbose
;
;
; OUTPUTS 	comprun,lun
;
; Calls: FOR_WRITE_COMPINPUT
;
; Called by: FOR_INTENSINT
;
; History: Written by Sarah Gibson 2015
;
;   Aug-13-2016: removed setup of IONEQ because this is done in for_specdefaults, SEG
;	also-- there should no longer be a file called IONEQ in rundir
;
;	NOTE: when this code runs it will either be in working_dir directory or
;		if working_dir not set then it will be in the directory forward is called from
;		and I/O will occur there. 
;		This will be a problem if the user does not have read/write permission for that directory
;  Jun-1-2019: used slash and file_copy for PC compatibility
;	Sept 2021 - passed through nowidgmess
;	Apr 2022 -- removed old_dir pass through and cd to working_dir
;		because now it is done in for_intensint
;  Mar-May 2024: added "alin" keyword which 
;       is _not_ passed through all of FORWARD
;       but can be changed default below, and allows
;       swap out of ATOM  file as communicated by
;       Alin Paraschiv in December 2022 
;     ALSO note -- abundance in Alin's file is not up to date
;	with latest Chianti
;	C_ will be sun_coronal, P_ will be 2021 Asplund
;-

; for comp:

; set up output file to  write cube points and model output
; to be used by Judge's comp program (forcomp/cle)
;          copy atom and write out input files

 slash=path_sep()
 default,alin,-1
; alin=-1 photospheric abundance asplund 2021
; alin=1  sun coronal abundance
; alin=-2 original alin (deprecated) is Feldman
; alin=0 -- 2006 atom file abundance, who knows

          for_write_compinput,ObsPramsStruct.FCompPrams

          rundir=file_dirname(GET_ENVIRON('FORWARD'))+slash+file_basename(GET_ENVIRON('FORWARD'))+slash+'FORCOMP'+slash+'run'+slash
  	   if abs(alin) eq 1 then rundir_alin=rundir+'C_alin_' else rundir_alin=rundir
	   if alin eq -1 then rundir_alin=rundir+'P_alin_'
;	   rundir_alin=rundir+'alin_'

; 
; note at the moment the alin updates are only for Fe13, si10, si9
;
          Case 1 of
                strupcase(ObsPramsStruct.Instrument) eq 'COMP' or strupcase(ObsPramsStruct.Instrument) eq 'OTHERCOMP' or strupcase(ObsPramsStruct.Instrument) eq 'WAVECOMP': file_copy, rundir_alin+'atom_fe13', 'ATOM', /force, /overwrite
                strupcase(ObsPramsStruct.Instrument) eq 'SI10COMP' or strupcase(ObsPramsStruct.Instrument) eq 'SICOMP': file_copy, rundir_alin+'atom_si10', 'ATOM', /force, /overwrite
                strupcase(ObsPramsStruct.Instrument) eq 'FE11COMP': file_copy, rundir+'atom_fe11', 'ATOM', /force, /overwrite
                strupcase(ObsPramsStruct.Instrument) eq 'SI9COMP': file_copy, rundir_alin+'atom_si9', 'ATOM', /force, /overwrite
                strupcase(ObsPramsStruct.Instrument) eq 'CORMAG': file_copy, rundir+'atom_fe14', 'ATOM', /force, /overwrite
          endcase

          intemp = long(LosPramsStruct.NLos)
          inunwrap=long(nunwrap)
          iverbose = long(0)
          if verbose eq 1 then iverbose = long(1)
;
; check CLE fortran executable access
;
          if ObsPramsStruct.FCompPrams.noftran ne 1 then begin
           comprun=file_dirname(GET_ENVIRON('FORWARD'))+slash+file_basename(GET_ENVIRON('FORWARD'))+slash+'FORCOMP'+slash+'forcomp'
            compruntest = working_dir+slash+'forcomp'
            compruntest2='.'+slash+'forcomp'

           if file_test(compruntest) then comprun = compruntest else begin
                if file_test(compruntest+'.exe') then comprun = compruntest+'.exe' else $
                 if file_test(compruntest2+'.exe') then comprun = compruntest2+'.exe'
           endelse

           if file_exist(comprun) eq 0 then begin
;             if keyword_set(nowidgmess) then message,/info,' no fortran executable -- using IDL. FACTOR OF ~50 SLOWER!!! To add Fortran version, see instructions at http://www.hao.ucar.edu/FORWARD/FOR_SSW/docs/INSTRUCTIONS/fortran.html.' else d=dialog(/warning,'no fortran executable -- using IDL. FACTOR OF ~50 SLOWER!!! To add Fortran version, see instructions at http://www.hao.ucar.edu/FORWARD/FOR_SSW/docs/INSTRUCTIONS/fortran.html.')
             if keyword_set(nowidgmess) then message,/info,'no fortran executable -- using IDL. FACTOR OF ~50 SLOWER!!! To add Fortran version, see instructions at http://www.hao.ucar.edu/FORWARD/FOR_SSW/docs/INSTRUCTIONS/fortran.html. ' else d=dialog(/warning,'no fortran executable. To add Fortran, see instructions at http://www.hao.ucar.edu/FORWARD/FOR_SSW/docs/INSTRUCTIONS/fortran.html.')
	     stop
             ObsPramsStruct.FCompPrams.noftran=1
           endif
          endif

;
; if fortran set (and executable found )
;
          if ObsPramsStruct.FCompPrams.noftran ne 1 then begin
           openw,lun,'modelcube4comp.dat',/f77_unformatted,/get_lun
           writeu,lun,intemp,inunwrap,iverbose
;	   print,intemp,inunwrap,iverbose
          endif

end
