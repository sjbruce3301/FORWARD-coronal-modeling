pro for_getstructs,GridPramsStruct,ObsPramsStruct,LosPramsStruct,ModPramsStruct,$
        gridtype,ngrid,ngy,rheight,xxmin,xxmax,yymin,yymax,$
	losoffset,limb,cmer,phio,losmin,losint,nlos,axisym,nostretch,$
	azequi,distobs,$
	dodisk,losuse,occult,upoccult,ruser,thuser,phuser,coorduser,bang,thetao,line,incres,instrument,$
	pos,frequency_MHz,dogyro,fcor,ulimb,rotaz,wavelength_Ang,wavelength2_Ang,numion,labelonly,obsloslimit,date,working_dir,$
        pop2tregime,SpecPrams,NoisePrams,FCompPrams,$
	xrt=xrt,eit=eit,wl=wl,cds=cds,iris=iris,myspect=myspect,iondens=iondens,eis=eis,neviii770=neviii770,neviii780=neviii780,ovi1032=ovi1032,ovi1037=ovi1037,mgix706=mgix706,lya=lya,radio=radio,faraday=faraday,aia=aia,euvia=euvia,euvib=euvib,trace=trace,$
	swap=swap,kcor=kcor,cormag=cormag,comp=comp,fe11comp=fe11comp,si9comp=si9comp,othercomp=othercomp,si10comp=si10comp,$
        greencomp=greencomp,swss=swss,losem=losem,colden=colden,ben_dens_int=ben_dens_int,b_int=b_int,b_dens_int=b_dens_int,b_pos_int=b_pos_int,b_pos_dens_int=b_pos_dens_int,benergy=benergy,pop2losem=pop2losem,pop2colden=pop2colden

;+
; Name:  FOR_GETSTRUCTS
;
; Program to create structures containing forward calculation keywords:
;
; OUTPUTS: GridPramsStruct,ObsPramsStruct,LosPramsStruct
; INPUTS: ModPramsStruct
;
;  see DEFAULTS/for*defaults.pro for keyword description
;   also OBSERVABLES/for_obs_choose.pro
;
;
; Called by FOR_DRIVE
; Calls FOR_OBS_NAME, FOR_GET_GRID, FOR_LOS_PRAMS
;
;  Written Terry Kucera, Sarah Gibson, 2010-2014
;  Version 2.0 July 2014
;   Modifications:
;               13-Jan-2015  added pop2colden and pop2losem, TAK
;		10-Feb-2016  added bang to for_get_grid call SEG
;		March 2016 - added coorduser pass through
;		June 2018 - added benergy etc pass through
;		Sept 2021 - testing azequi pass through
;		Dec 2021 -- passed through distobs
;		Mar 2022 -- changed xoff --> losoff
;			passed losuse through to for_get_grid
;		May 2022 -- passed through nostretch
;               July 2023
;                    added IONDENS
;       Jun 2024 -- added BPOS column variables
;	Jul 2024 -- passed through ULIMB
;	Aug 2024 -- added WLRAT hooks
;-

; Build the structures
;

  ObsPramsStruct = for_obs_name(ModPramsStruct.MagMod,ModPramsStruct.Name,line,instrument,pos,frequency_MHz,dogyro,fcor,ulimb,rotaz,wavelength_Ang,wavelength2_Ang,numion,labelonly,obsloslimit,date,NoisePrams,pop2tregime,SpecPrams,FCompPrams,$
	xrt=xrt,eit=eit,wl=wl,cds=cds,iris=iris,myspect=myspect,iondens=iondens,eis=eis,neviii770=neviii770,neviii780=neviii780,ovi1032=ovi1032,ovi1037=ovi1037,mgix706=mgix706,lya=lya,radio=radio,faraday=faraday,aia=aia,euvia=euvia,euvib=euvib,trace=trace,$
	swap=swap,kcor=kcor,cormag=cormag,comp=comp,fe11comp=fe11comp,si9comp=si9comp,othercomp=othercomp,si10comp=si10comp,$
	greencomp=greencomp,swss=swss,losem=losem,colden=colden,ben_dens_int=ben_dens_int,b_int=b_int,b_dens_int=b_dens_int,benergy=benergy,working_dir=working_dir)


  GridPramsStruct = for_get_grid(gridtype,ngrid,ngy,rheight,$
	limb,cmer,phio,bang,$
        ruser,thuser,phuser,coorduser,$
	xxmin,xxmax,yymin,yymax,losoffset,azequi,distobs,losuse)

  LosPramsStruct = for_los_prams(losuse,losmin,$
	losint,nlos,axisym,nostretch,$
	bang,thetao,dodisk,incres,occult,upoccult)

end
