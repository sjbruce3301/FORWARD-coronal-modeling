;************************************************************************
;+
function for_obs_name,magmod,modelname,line,instr,pos,frequency_MHz,dogyro,fcor,ulimb,rotaz,wavelength_Ang,wavelength2_Ang,numion,labelonly,obsloslimit,date,NoisePrams,pop2tregime,$
	SpecPrams,FCompPrams,xrt=xrt,eit=eit,wl=wl,cds=cds,iris=iris,myspect=myspect,iondens=iondens,eis=eis,radio=radio,faraday=faraday,lya=lya,neviii770=neviii770,neviii780=neviii780,ovi1032=ovi1032,ovi1037=ovi1037,mgix706=mgix706,aia=aia,euvia=euvia,euvib=euvib,trace=trace,$
        swap=swap,kcor=kcor,cormag=cormag,comp=comp,fe11comp=fe11comp,si9comp=si9comp,othercomp=othercomp,si10comp=si10comp,greencomp=greencomp,$
        swss=swss,losem=losem,colden=colden,benergy=benergy,ben_dens_int=ben_dens_int,b_int=b_int,b_dens_int=b_dens_int,b_pos_int=b_pos_int,b_pos_dens_int=b_pos_dens_int,pop2losem=pop2losem,pop2colden=pop2colden,working_dir=working_dir
;
;Name: FOR_OBS_NAME
;
;Purpose: To create a structure containing information concerning the observations to be modeled
;
;; Inputs:
;	Magmod, modelname (whether model is magnetic, its name)
;  	LINE --  Line Name for observable, or, alternatively, physical parameter (e.g. dens, temp)
;	Instr - Instrument string 
;	POS - for observables, means no LOS integration - done in plane of sky slice
;	frequency_Mhz,dogyro,fcor,rotaz,wavelength_ang,numion,NoisePrams
;	SpecPrams,FCompPrams
;	LABELONLY:  ObsStruct will only have a plot label in it
;      	OBSLOSLIMIT (double-RSUN)
;               if set, and integrating in constant TAU along LOS
;                 then LOS integration will not go past this radial
;                 height for LOS intersecting the photosphere.
;       DATE -- string containing observation date and time
;	POP2TREGIME -- regime of population 2 if it exists 
;	 -- set in FOR_OBSDEFAULTS
;
;  Keyword Inputs
;	working_dir -- also instruments
;
;;Output: structure with info in it unless LABELONLY set, when only a label string is returned
; 	OutStruct structure contains fields
;		INSTRUMENT, string
;		LINENAME, string description
;		LINENUM (number with one to one correspondence to LINENAME - or set to -99 for physical parameters, or -999 for WL or EM), 
;		LABEL text label for plots
;		NAME text name for files
;
;Common Blocks: None
;
; Called by FOR_GETSRUCTS, FOR_DRIVE
;
; Calls FOR_OBS_CHOOSE
;
; HISTORY:
; Written: T. Kucera Jan. 3, 2010
; Modified: S. Gibson Feb 3, 2010
;           J. Dove Feb 25, 2010 (added BETA for possible line)
;           SEG Mar 1, 2010 (added COMP)
;           JBD Mar 11, 2010 (added linenames of 'W' and 'PoI' for comp)
;               fixed bug for case where line is defined but not a pos variable (added an
;               else : in the case logic for ("case strupcase(linename) of')
;           SEG Apr 13, 2010 (adapted to allow plane of sky calculation of observables)
;           JBD May 14 (added VoI as option for Stokes)
;           SEG May 13 (added cavity-model parameter outputs for plane of sky
;           TAK July 7, 2010 Added SXT, SWAP
;           TAK July 13, 2010, Added Instrument Keyword. 
;		TAK July 26, 2010 Added date Keyword
;		TAK Sep 3, 2010 Added AIA
;		TAK Nov 5, 2010 Added SWSS
;		TAK Nov 11, 2010 Added some extra messages in case unavailable lines 
;					requested for EIS or SWSS and added wavelength to map ID.
;		TAK Dec 4, 2010 Adjusted for some variations in the XRT line names.
;					fixed bug for cases where Instr0 is array of 1 element.
;		TAK Feb 14, 2011 Added new lines for EIS (Fe  X and XI)
;		SEG Made date default ' ' to be consistent with for_posmap and for_plot
;		TAK July 29, 2011 Fixed bug related to using line names 'Fe12_ratio' and 'Fe12_Don'
;		TAK Aug. 29, 2011 Redid EIS line naming to incorporate new lines.
;		TAK Sept. 22, 2011 added two more  possible EIS line names, fe12_195.1v2 and fe14_274.2v2
;		TAK Oct. 3, 2011 added "line" for the LOS Emission Measure
;		TAK Oct. 27, 2011 added FF for filling factor
;		SEG added SI10 option for COMP
;		TAK Nov. 4, 2011 Added CDS 
;		SEG added NINST - instability power law n (e.g. torus instability)
;		TAK Apr. 3, 2012, added message if no EIS lines match.
;		SEG May 5, 2012 - changed PoI and P to LoI and L
;		LR Jan 12, 2013 -- added WAVECOMP
;		SEG Jan 2013 - moved defaults to for_defaults.pro
;		SEG Jan 2013 - fixed bug where NINST and NInst same name -> NNInst
;		SEG Jan 2013 - fixed bug where LOSEM was being treated like POS variable
;		SEG Jan 2013 - made Line='NONE' and Instr0='NONE' 
;		  default for no choice of line and/or instrument
;		SEG Jan 2013 - assigned Pos=1 for plane-of-sky variables
;		SEG Feb 2013 - added GREENCOMP
;		SEG Mar 2013 - simplified and broke out for_obs_setup
;		SEG July 2013 - changed Pos=-1 for plane-of-sky variables
;		SEG Oct 2014 - changed GREENCOMP to CORMAG
; 
; Version 2.0 July 2014
;
;               13-Jan-2013 Adding pop2colden and pop2losem. TAK
;		30-Jan-2016 adding FF for filling factor
;		06-Feb-2016 put in check on POP2ON, POP2TREGIME
;		06-Feb-2017 saved line[0] rather than line 
;		July 2018 added benergy etc pass through
;		Sept 2021 - added UOI and QOI to list of renamings
;		April 2022 -- added POS=2 for Thomson Sphere
;		Oct 2022 -- and POS=-2 for Physical Diagnostics at TS
;		Dec 2022 -- added NEVIII
;		Sep 2023 -- added iondens
;               Feb 2024 -- added MGIX
;       Jun 2024 -- added BPOS column variables
;	July 2024 -- added WLRAT hooks
;
	if strupcase(line) eq 'I' then line='STOKESI'
	if strupcase(line) eq 'Q' then line='STOKESQ'
	if strupcase(line) eq 'U' then line='STOKESU'
	if strupcase(line) eq 'V' then line='STOKESV'
	if strupcase(line) eq 'VOI' then line='STOKESVOI'
	if strupcase(line) eq 'UOI' then line='STOKESUOI'
	if strupcase(line) eq 'QOI' then line='STOKESQOI'
	if strupcase(line) eq 'L' then line='STOKESL'
	if strupcase(line) eq 'LOI' then line='STOKESLOI'
	if strupcase(line) eq 'W' then line='STOKESW'
	if strupcase(line) eq 'AZ' then line='STOKESAZ'
      if pop2tregime ne 0 then pop2on=1 else pop2on=0
      for_obs_choose,magmod,modelname,line,instr,label,number,type,$
	xrt=xrt,eit=eit,wl=wl,cds=cds,iris=iris,myspect=myspect,iondens=iondens,eis=eis,radio=radio,faraday=faraday,neviii770=neviii770,neviii780=neviii780,ovi1032=ovi1032,ovi1037=ovi1037,mgix706=mgix706,lya=lya,aia=aia,euvia=euvia,euvib=euvib,trace=trace,$
        swap=swap,kcor=kcor,cormag=cormag,comp=comp,fe11comp=fe11comp,si9comp=si9comp,othercomp=othercomp,si10comp=si10comp,greencomp=greencomp,$
        swss=swss,losem=losem,colden=colden,pop2losem=pop2losem,pop2colden=pop2colden,pop2on=pop2on,working_dir=working_dir

      if strupcase(line) eq 'RM' then label='Faraday Rotation Measure'
      if strupcase(line) eq 'FR' then label='Faraday Rotation Angle'

      if is_number(pos) then begin
	if pos eq 0 and number eq -99 then pos = -1
      endif else begin
       if number eq -99 then pos=-1
      endelse

      outarray={Instrument:strupcase(instr),LineName:strupcase(Line[0]),LineNum:number,Label:label,IClass:type,Date:date,Pos:pos,ObsLosLimit:obsloslimit,Frequency_MHz:frequency_MHz,DoGyro:DoGyro,FCor:FCor,ULimb:Ulimb,RotAz:RotAz,WaveLength_Ang:wavelength_Ang,Wavelength2_Ang:wavelength2_ang,NumIon:numion,Pop2TRegime:Pop2Tregime,SpecPrams:SpecPrams,NoisePrams:NoisePrams,FCompPrams:FCompPrams} 

;      print,'for_obs_name pos',pos
;      print,'for_obs_name number',number
;      print,'for_obs_name instrument',instr
      if labelonly eq 1 then return,outarray.Label else return,outarray

end

