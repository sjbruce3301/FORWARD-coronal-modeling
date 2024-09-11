;************************************************************************
     pro for_intcompclose,ObsPramsStruct,LosPramsStruct,comprun,lun,thuse,stokesred,stokeslred,wavelengths,velocities,centwave,linewidth,lchisq,parallel,notdisk,nunwrapall,CoordSize,NDim,intens,StokesStruct,verbose=verbose

;+
; Name: FOR_INTCOMPCLOSE
;
; Purpose:  completes for polarimetry (comp-type) integration (NOFTRAN)
;
; INPUTS ObsPramsStruct,LosPramsStruct,comprun,lun,thuse,parallel,notdisk,nunwrapall,CoordSize,NDim
;    (if NOFTRAN=1): stokesred,stokeslred,wavelengths,velocities,centwave,linewidth,lchisq
;
; KEYWORD INPUT: verbose
;
; OUTPUTS: intens, StokesStruct
;
; Called by: FOR_INTENSINT
;
; Calls: FOR_READCOMPRESULT,FORCOMP.F (comprun spawn command),FOR_CHANGEREF
;
; History: Written by Sarah Gibson 2015
;	Nov 2016 - changed fltarr to dblarr
;	Jan 2018 - fixed bug where Qprime and Uprime
;	 had large negative numbers instead of -8888 for bad data
;       Jun 2019 - used file_delete for PC compatibility
;	Sep 2021 - removed delete of IONEQ because that is set up in for_specdefaults,
;	 even when there is no integration (e.g., reading a save file) -- so put the delete
;	 at the end of for_drive and for_plotfits
;	 expanded conditional test for STOKESQOI etc
;	Apr 2022 -- removed pass through of working_dir and old_dir
;		and change directory -- now done in for_intensint
;	Jul 2022 -- allowed for notdisk=-2,-3 which will have full data
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;  March 2023 -- put in check for 0 in denominator for atan
;  Dec 2023 -- changed sign of Doppler and Stokes V to keep spectroscopic convention 
;  Jun 2024 - added comment about StokesStruct
;-
      if ObsPramsStruct.FCompPrams.noftran ne 1 then begin

;          close model cube data file that forcomp will read, call
;          fortran code, and output results into file:

           free_lun,lun

           if parallel eq 0 then spawn,comprun

;          read in output results of forcomp (which gives the stokes parameters)

           uselinenum=0
           if strupcase(ObsPramsStruct.Instrument) eq 'OTHERCOMP' then uselinenum=1
	   vintchoice= ObsPramsStruct.FCompPrams.vintchoice
           for_readcompresult,stokesred,stokeslred,wavelengths,velocities,centwave,verbose=verbose,uselinenum=uselinenum,linewidth=linewidth,lchisq=lchisq,vintchoice=vintchoice

;          delete forcomp data files
;
; if you want these for diagnostic purposes, comment these lines
;
           removefiles=['F77OUTPUT', 'JOBLOG', 'OUT', 'DUMI', 'DUMS', 'ATMOS', 'ATOM', 'INPUT', 'TIME', 'modelcube4comp.dat']
           for files_i=0, n_elements(removefiles)-1 do file_delete, removefiles[files_i], /quiet

      endif
; end test for NOFTRAN=1

;
; Now figure out INTENS
;

      Iuse=stokesred[0,*]
      Izero = where(Iuse eq 0.)
      Ibad=where(Iuse eq -8888.)

;
; convert Stokes Q, U to radial reference frame
; (starts out with N-S as ref)
; call for_changeref
; type=0 is N-S to radial
;

;Qprime=stokesred[1,*]
;Uprime=stokesred[2,*]
      for_changeref,thuse,stokesred[1,*],stokesred[2,*],Qprime,Uprime,type=0


;
      case 1 of
              (strupcase(ObsPramsStruct.Linename) eq 'STOKESI'): intens = stokesred[0,*]
;              (strupcase(ObsPramsStruct.Linename) eq 'STOKESQ'): intens = stokesred[1,*]
;              (strupcase(ObsPramsStruct.Linename) eq 'STOKESU'): intens = stokesred[2,*]
              (strupcase(ObsPramsStruct.Linename) eq 'STOKESQ'): intens = Qprime
              (strupcase(ObsPramsStruct.Linename) eq 'STOKESU'): intens = Uprime
              strpos(strupcase(ObsPramsStruct.Linename),'QOI') ge 0: intens = Qprime/stokesred[0,*]
              strpos(strupcase(ObsPramsStruct.Linename),'UOI') ge 0: intens = Uprime/stokesred[0,*]
;
; note, no lnoger multiply by minus one because CLE direction is not
; consistent with frame of reference
; instead keep spectroscopic convention of positive is away, but plot in red blue color table
; where red will be away (42) as opposed to (41) used by Bx, Vx where negative is away and flips color
;
;              (strupcase(ObsPramsStruct.Linename) eq 'STOKESV'): intens = -1.*stokesred[3,*]
;              (strupcase(ObsPramsStruct.Linename) eq 'STOKESW'): intens = -1.*stokesred[4,*]
;              (strupcase(ObsPramsStruct.Linename) eq 'DOPPLERVLOS'): intens = -1.*velocities[*]
              (strupcase(ObsPramsStruct.Linename) eq 'STOKESV'): intens = stokesred[3,*]
              (strupcase(ObsPramsStruct.Linename) eq 'STOKESW'): intens = stokesred[4,*]
              (strupcase(ObsPramsStruct.Linename) eq 'DOPPLERVLOS'): intens = velocities[*]
; note for now the information in LINEWIDTH is ignored, and it is recalculated here from I cent and I integrated
;
              (strupcase(ObsPramsStruct.Linename) eq 'LINEWIDTH'): intens = 3.d5*stokesred[0,*]/stokesred[5,*]/double(centwave[*])
;
; may test this next line
; if it works, also edit similar line within FOR_DRIVE
;
;              (strupcase(ObsPramsStruct.Linename) eq 'LINEWIDTH'): intens = 3.d5*linewidth[*]/double(centwave[*])
;
; note this is equivalent width but data is e-folding (1/e) width
;
;              strpos(strupcase(ObsPramsStruct.Linename),'VOI') ge 0: intens = -1.*stokesred[3,*]/stokesred[0,*]
              strpos(strupcase(ObsPramsStruct.Linename),'VOI') ge 0: intens = stokesred[3,*]/stokesred[0,*]
              (strpos(strupcase(ObsPramsStruct.Linename), 'LOI') ge 0 or strupcase(ObsPramsStruct.Linename) eq 'STOKESL' or strpos(strupcase(ObsPramsStruct.LineName),'AZ') ge 0): begin
                 q = Qprime
                 u = Uprime
                 p = sqrt(q*q+u*u)
                 intens = p
                 if strpos(strupcase(ObsPramsStruct.Linename), 'LOI') ge 0 then begin
                   if min(Izero) ge 0. then Iuse[Izero] = 1.
                   intens = p/Iuse
                 endif
                 if strupcase(ObsPramsStruct.LineName) eq 'STOKESAZ' then begin
                  alpha = 0.5*atan(u,q)
		  test=where(q eq 0. and u eq 0.) 
                  if min(test) ne -1 then alpha[test] = sqrt(-1)
                  mdtor=!dpi/180d0
                  intens = alpha/mdtor
                  intens=intens mod 180.d0
                  test=where(intens lt 0.)
                  if min(test) ne -1 then intens[test]=intens[test]+180.d0
                 endif
              end
      endcase

      if min(Izero) ge 0. then intens[Izero] = 0.
;
; watch out because changing Q, U frame of reference messed up bad
; data values of -8888
;

if min(Ibad) ne -1 then begin
  Qprime[Ibad]=-8888.
  Uprime[Ibad]=-8888.
  intens[Ibad]=-8888.
endif

;
; Now figure out StokesStruct
;
      stokesI = dblarr(nunwrapall)
      stokesQ = dblarr(nunwrapall)
      stokesU = dblarr(nunwrapall)
      stokesV = dblarr(nunwrapall)
      stokesW = dblarr(nunwrapall)
      stokescentI=dblarr(nunwrapall)
      Icentwave=dblarr(nunwrapall)
      Lchisqwave=dblarr(nunwrapall)

      if min(notdisk) ge 0 then stokesI[notdisk]=stokesred[0,*] else stokesI = stokesred[0,*]
;      if min(notdisk) ge 0 then stokesQ[notdisk]=stokesred[1,*] else stokesQ = stokesred[1,*]
;      if min(notdisk) ge 0 then stokesU[notdisk]=stokesred[2,*] else stokesU = stokesred[2,*]
      if min(notdisk) ge 0 then stokesQ[notdisk]=Qprime else StokesQ=Qprime
      if min(notdisk) ge 0 then stokesU[notdisk]=Uprime else StokesU=Uprime
;      if min(notdisk) ge 0 then stokesV[notdisk]=-1.*stokesred[3,*] else stokesV = -1.*stokesred[3,*]
;      if min(notdisk) ge 0 then stokesW[notdisk]=-1.*stokesred[4,*] else stokesW = -1.*stokesred[4,*]
      if min(notdisk) ge 0 then stokesV[notdisk]=stokesred[3,*] else stokesV = stokesred[3,*]
      if min(notdisk) ge 0 then stokesW[notdisk]=stokesred[4,*] else stokesW = stokesred[4,*]
      if min(notdisk) ge 0 then stokescentI[notdisk]=stokesred[5,*] else stokescentI = stokesred[5,*]
      if min(notdisk) ge 0 then Icentwave[notdisk]=centwave else Icentwave=centwave
      if min(notdisk) ge 0 then Lchisqwave[notdisk]=lchisq else Lchisqwave=lchisq

      stokesI=LosPramsStruct.AxiSymMult*reform(stokesI,CoordSize[1:NDim],/overwrite)
      stokesQ=LosPramsStruct.AxiSymMult*reform(stokesQ,CoordSize[1:NDim],/overwrite)
      stokesU=LosPramsStruct.AxiSymMult*reform(stokesU,CoordSize[1:NDim],/overwrite)
      stokesV=LosPramsStruct.AxiSymMult*reform(stokesV,CoordSize[1:NDim],/overwrite)
      stokesW=LosPramsStruct.AxiSymMult*reform(stokesW,CoordSize[1:NDim],/overwrite)
      stokescentI=LosPramsStruct.AxiSymMult*reform(stokescentI,CoordSize[1:NDim],/overwrite)
      Icentwave=LosPramsStruct.AxiSymMult*reform(Icentwave,CoordSize[1:NDim],/overwrite)
      Lchisqwave=LosPramsStruct.AxiSymMult*reform(Lchisqwave,CoordSize[1:NDim],/overwrite)


; make structure!
      StokesStruct = {I:stokesI,Q:stokesQ,U:stokesU,V:stokesV,W:stokesW,centI:stokescentI,centwave:Icentwave,chisqwave:Lchisqwave}

 ;       **NOTE THAT somewhat different quantities (but equivalent) are stored for data, see FOR_READCOMPFITS

      if max(stokeslred) ne 0. then begin
            CoordSizeLout=size(stokeslred)
            stokeslreduse = dblarr(CoordSizeLout[1],nunwrapall,CoordSizeLout[3])
            for istokes =0,CoordSizeLout[1]-1 do $
             if min(notdisk) ge 0 then stokeslreduse[istokes,notdisk,*]=stokeslred[istokes,*,*] else stokeslreduse=stokeslred
            stokeslreduse=LosPramsStruct.AxiSymMult*reform(stokeslreduse,[CoordSizeLout[1],CoordSize[1:NDim],CoordSizeLout[3]],/overwrite)
            velocitiesuse = dblarr(nunwrapall)
;            if min(notdisk) ge 0 then velocitiesuse[notdisk]=-1.*velocities else velocitiesuse=-1.*velocities
            if min(notdisk) ge 0 then velocitiesuse[notdisk]=velocities else velocitiesuse=velocities
            velocitiesuse=LosPramsStruct.AxiSymMult*reform(velocitiesuse,CoordSize[1:NDim],/overwrite)
            StokesStruct = {I:stokesI,Q:stokesQ,U:stokesU,V:stokesV,W:stokesW,centI:stokescentI,stokeslred:stokeslreduse,wavelengths:wavelengths,velocities:velocitiesuse,centwave:Icentwave,chisqwave:Lchisqwave}
      endif
end
