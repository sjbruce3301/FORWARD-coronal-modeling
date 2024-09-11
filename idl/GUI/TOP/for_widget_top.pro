PRO for_widget_top

;+
;  Name:  FOR_WIDGET_TOP
;
;  This program creates the top widget bar
;
;  Called by FOR_WIDGET, FOR_WIDGET_CALENDAR, FOR_WIDGET_MODEL_EVENT, FOR_WIDGET_PLOT_EVENT
;
;  External call FOR_WIDGET_BUILD
;
; Written by Blake Forland, Sarah Gibson 2013-2014
; Version 2.0 July 2014
;   June 2019 - used slash for PC compatibility
;   May 2020 -- added KCor by date hooks
;  August 2020 -- added hooks for CROISSANT and TOMO  (ADAPTCUBE added previously)
;  November 2020 -- moved CROISSANT and TOMO to main FORWARD
;  January 2021 -- added hooks for STRIA
;  March 2022 -- added hooks for TURBHY
; June 2023 -- added SYNCOM hooks
; Jan 2024 -- added UCOMP lines
; Feb 2024 -- changed title to COMP/UCOMP
;

  common forward,flag,variables,settings,plotops,losops,gridops,widgets,obsops,modops,strings,outops

  mainbar = widget_base(title='FORWARD',row=2,tlb_kill_request_events=1)
  topbar = widget_base(mainbar, /row,frame =1)

  widgets.top = mainbar

;
; get list of model names
;

 slash=path_sep()

 modeldir=concat_dir(GET_ENVIRON('FORWARD'),'MODELS')
 modelfile = file_dirname(modeldir)+slash+file_basename(modeldir)+slash+'ModelList.txt'

;
; uncomment if add new models
; (actually, this will happen first in for_modeldefaults
; so better to uncomment there
; 
; spawn,'ls -1 '+modeldir +'> '+modelfile

 nummodels=file_lines(modelfile)
 modelnames=strarr(nummodels-3)
;
; don't want the txt files or POINTVAL
;
 modname=''
 icount=0
 openr,unit,modelfile,/get_lun
 for im = 0,nummodels-1 do begin
  readf,unit,modname
  if strpos(modname,'txt') le 0 and strpos(strupcase(modname),'POINTVAL') lt 0 then begin
        modelnames(icount)=modname
        icount=icount+1
  endif
 endfor
 free_lun,unit

;
; check for ADAPTCUBE in working directory
;       
        
 if n_elements(settings.working_dir) eq 1 then begin
            checkadapt=concat_dir(settings.working_dir,'ADAPTCUBE')
 endif else begin
  	    checkadapt='ADAPTCUBE'
 endelse

 if file_test(checkadapt) ne 0 then begin
  modelnames=[modelnames,'ADAPTCUBE']
  icount=icount+1
 endif

;
; check for STRIA or SYNCOM or TURBHY in working directory
;       
        
 if n_elements(settings.working_dir) eq 1 then begin
            checkstria=concat_dir(settings.working_dir,'STRIA')
            checksyncom=concat_dir(settings.working_dir,'SYNCOM')
            checkturbhy=concat_dir(settings.working_dir,'TURBHY')
 endif else begin
  	    checkstria='STRIA'
  	    checksyncom='SYNCOM'
  	    checkturbhy='TURBHY'
 endelse

 if file_test(checkstria) ne 0 then begin
  modelnames=[modelnames,'STRIA']
  icount=icount+1
 endif
 if file_test(checksyncom) ne 0 then begin
  modelnames=[modelnames,'SYNCOM']
  icount=icount+1
 endif
 if file_test(checkturbhy) ne 0 then begin
  modelnames=[modelnames,'TURBHY']
  icount=icount+1
 endif

 dummy={MENU, flags: 0, name: '', proc: ''}

 topbarstring = "['1\Data',"
 topbarstring=topbarstring+$
                 "'1\AIA',"+$
	         "'1\ByDate',"+$
                 "'0\94','0\131','0\171','0\193','0\211','0\304','2\335',"+$
                 "'2\ByFile',"
 topbarstring=topbarstring+$
                 "'1\EUVIA',"+$
	         "'1\ByDate',"+$
                 "'0\171','0\195','0\284', '2\304',"+$
                 "'2\ByFile',"
 topbarstring=topbarstring+$
                 "'1\EUVIB',"+$
	         "'1\ByDate',"+$
                 "'0\171','0\195','0\284', '2\304',"+$
                 "'2\ByFile',"
 topbarstring=topbarstring+$
                 "'1\EIT',"+$
	         "'1\ByDate',"+$
                 "'0\171','0\195','0\284', '2\304',"+$
                 "'2\ByFile',"
 topbarstring=topbarstring+$
                 "'1\XRT',"+$
;	         "'0\ByDate',"+$
	         "'2\ByFile',"
 topbarstring=topbarstring+$
                 "'1\SWAP',"+$
	         "'0\ByDate',"+$
	         "'2\ByFile',"
 topbarstring=topbarstring+$
                 "'1\KCOR',"+$
	         "'0\ByDate',"+$
	         "'2\ByFile',"
 topbarstring=topbarstring+$
                 "'1\CORMAG',"+$
;	         "'1\ByDate',"+$
                 "'1\ByFile',"+$
                 "'0\StokesI','0\StokesQ','0\StokesQoI','0\StokesU','0\StokesUoI','0\StokesL','0\StokesLoI','0\StokesAz', '0\I_KCOR','2\L_KCOR','2\',"
;                 "'1\StokesI','0\5303','2\5308','1\StokesQ','0\5303','2\5308','1\StokesQoI','0\5303','2\5308','1\StokesU','0\5303','2\5308','1\StokesUoI','0\5303','2\5308','1\StokesL','0\5303','2\5308','1\StokesLoI','0\5303','2\5308','1\DopplerVlos','0\5303','2\5308','1\LineWidth','0\5303','2\5308','1\StokesAz','0\5303','2\5308','2\','\2',"
 topbarstring=topbarstring+$
                 "'1\COMP/UCOMP',"+$
	         "'1\ByDate',"+$
                 "'1\StokesI','0\10747','2\10798','1\StokesQ','0\10747','2\10798','1\StokesQoI','0\10747','2\10798','1\StokesU','0\10747','2\10798','1\StokesUoI','0\10747','2\10798','1\StokesL','0\10747','2\10798','1\StokesLoI','0\10747','2\10798','1\DopplerVlos','0\10747','2\10798','1\LineWidth','0\10747','2\10798','1\StokesAz','0\10747','2\10798','2\',"+$
                 "'1\ByFile',"+$
;; uncomment these when ready to give ucomp option with other wavelengths-- but be careful because the instrument=ucomp causes problems
;                 "'0\StokesI','0\StokesQ','0\StokesQoI','0\StokesU','0\StokesUoI','0\StokesL','0\StokesLoI','0\DopplerVlos','0\LineWidth','2\StokesAz','2\',"
; topbarstring=topbarstring+$
;                 "'1\UCOMP',"+$
;	         "'1\ByDate',"+$
;                 "'1\StokesI',"+$
;        	"'0\10747','0\10798','0\5303','0\6374','0\7062','2\7894',"+$
;		"'1\StokesQ',"+$
;        	"'0\10747','0\10798','0\5303','0\6374','0\7062','2\7894',"+$
;        	"'1\StokesQoI',"+$
;        	"'0\10747','0\10798','0\5303','0\6374','0\7062','2\7894',"+$
;        	"'1\StokesU',"+$
;        	"'0\10747','0\10798','0\5303','0\6374','0\7062','2\7894',"+$
;        	"'1\StokesUoI',"+$
;        	"'0\10747','0\10798','0\5303','0\6374','0\7062','2\7894',"+$
;        	"'1\StokesL',"+$
;        	"'0\10747','0\10798','0\5303','0\6374','0\7062','2\7894',"+$
;        	"'1\StokesLoI',"+$
;        	"'0\10747','0\10798','0\5303','0\6374','0\7062','2\7894',"+$
;        	"'1\DopplerVlos',"+$
;        	"'0\10747','0\10798','0\5303','0\6374','0\7062','2\7894',"+$
;        	"'1\LineWidth',"+$
;        	"'0\10747','0\10798','0\5303','0\6374','0\7062','2\7894',"+$
;        	"'1\StokesAz',"+$
;        	"'0\10747','0\10798','0\5303','0\6374','0\7062','2\7894',"+$
;        	"'2\',"+$
;                 "'1\ByFile',"+$
                 "'0\StokesI','0\StokesQ','0\StokesQoI','0\StokesU','0\StokesUoI','0\StokesL','0\StokesLoI','0\DopplerVlos','0\LineWidth','2\StokesAz','2\','2\',"
 topbarstring = topbarstring+"'1\Models',"
 for im = 0,icount-2 do topbarstring=topbarstring+$
                 "'0\"+modelnames(im)+"',"
 topbarstring=topbarstring+$
                 "'2\"+modelnames(icount-1)+"',"

;
; get Observables and Physical Parameters list
;

 obsstringtop=1
 for_widget_build,obsops,titlestring=obsstringtop

 topbarstringend = "'0\Settings','0\Calendar','2\FORWARD']"
 
 topbarstring = topbarstring + obsstringtop + topbarstringend

 bottombar = widget_base(mainbar, /row,frame =1)

 bottombarstring = "['0\Gather Windows',"+$
                    "'0\Output',"+$
                    "'0\Print Command',"+$
                    "'0\Reset',"+$
                    "'0\Quit',"+$
;                    "'2\Recalculate']"                ;"'0\Help',"+$
                    "'0\Recalculate','2\SAVE']"                ;"'0\Help',"+$


 x = execute("menu1 = cw_pdmenu(topbar,"+topbarstring+",/RETURN_FULL_NAME)")

 x = execute("menu3 = cw_pdmenu(bottombar,"+bottombarstring+",/RETURN_FULL_NAME)")

  tophelp=widget_base(bottombar,/column)
  help=widget_button(tophelp,value='TOP HELP',uvalue='Help')

 widget_control, mainbar, /realize

;
; The next line defines the program that controls what each button and
; field in the main menu does.
;  this is subroutine "for_widget_event"
;       doesnt call it here, BUT every time you change something
;       in the top bar, it runs
;

 xmanager, 'for_widget', mainbar, /no_block


end
