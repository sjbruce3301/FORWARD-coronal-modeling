;************************************************************************
;+
pro for_obs_choose,magmod,modelname,line,instr0,label,number,type,$
	xrt=xrt,eit=eit,wl=wl,cds=cds,myspect=myspect,iondens=iondens,iris=iris,eis=eis,neviii770=neviii770,neviii780=neviii780,ovi1032=ovi1032,ovi1037=ovi1037,mgix706=mgix706,lya=lya,radio=radio,faraday=faraday,aia=aia,euvia=euvia,euvib=euvib,trace=trace,$
        swap=swap,kcor=kcor,cormag=cormag,comp=comp,fe11comp=fe11comp,si9comp=si9comp,othercomp=othercomp,si10comp=si10comp,greencomp=greencomp,swss=swss,losem=losem,colden=colden,benergy=benergy,ben_dens_int=ben_dens_int,b_int=b_int,b_dens_int=b_dens_int,b_pos_int=b_pos_int,b_pos_dens_int=b_pos_dens_int,pop2losem=pop2losem,pop2colden=pop2colden,pop2on=pop2on,working_dir=working_dir

;
;Name: FOR_OBS_CHOOSE
;
;Purpose: Given line and/or instr, determine label and number
;
; Called by FOR_WIDGET, FOR_DRIVE, FOR_OBS_NAME
;
; Calls FOR_OBS_SETUP
; 
; Written by Sarah Gibson, Blake Forland 2013
; 
; Version 2.0 July 2014
;
; Modifications:
;  Added POP2LOSEM and POP2COLDEN Jan 2015
;  Added check for Pop2On Jan 2016
;  Kluge KCOR - WL keyword mixup fix May 2018
;  June 2018 added benergy pass through
;  October 2020 added WL variables XPOLF, XPOLB, XPOLG
;  September 2021 fixed cases where LinePos were not replaced with LinePos[0]
;	turning line (and label) into 1-element arrays
; January 2022 -- added TPOLF, TPOLB, TPOLG angular distance from TS functionality
; March 2022 -- changed to TPOLU, TPOLS
; December 2022 -- added neviii
; August 2023 -- added iondens
; Feb 2024 -- added mgix
; Jun 2024 -- added BPOS column variables
; Jul 2024 -- added PR
; Aug 2024 -- added WLRAT hooks
;

for_obs_setup,magmod,modelname,$
        all_inst=all_inst,all_lines=all_lines,all_names=all_names,all_nums=all_nums,all_labels=all_labels,all_types=all_types,all_defaults=all_defaults,phys_params=phys_params,phys_labels=phys_labels,model_params=model_params,model_labels=model_labels,pop2on=pop2on,working_dir=working_dir

Case 1 of 
	datatype(line) eq 'UND': 
	valid_num(line): LineNum=line
	datatype(line) eq 'STR' and strupcase(line) eq 'NONE': 
	datatype(line) eq 'STR' and strupcase(line) ne 'NONE': LineName=line
	else: message,'line must be a string or number'
endcase

if n_elements(LineName) eq 1 then begin
	if strupcase(LineName) eq 'PB' or strupcase(LineName) eq 'TB' or strupcase(LineName) eq 'XPOLF' $
	or strupcase(LineName) eq 'PR' $
	or strupcase(LineName) eq 'WLRAT' $
	or strupcase(LineName) eq 'TPOLU' or strupcase(LineName) eq 'TPOLG' or strupcase(LineName) eq 'TPOLS' $
	or strupcase(LineName) eq 'XPOLB' or strupcase(LineName) eq 'XPOLG' or strupcase(LineName) eq 'P' then wl=1
endif

	;means we don't have to use keywords for instrument
if n_elements(Instr0) eq 1 and datatype(Instr0) eq 'STR' then begin
    if strupcase(Instr0) eq 'NONE' then Instr0='Physical Diagnostics'
    if strupcase(Instr0) eq 'PLANE OF SKY SLICE' then Instr0='Physical Diagnostics'
    if strupcase(Instr0) eq 'OBSERVABLES' then Instr0='Physical Diagnostics'
    if strupcase(Instr0) eq 'MODEL DIAGNOSTICS' then Instr0='Physical Diagnostics'
;
; note that Physical Diagnostics will be overwritten if LineName not assigned
; and no keyword assigned, and Instr0 default to WL
;
    if (size(Instr0))[0] eq 1 then Instr0=Instr0[0]

     w=where(strupcase(Instr0) eq strupcase(all_inst),c)
     if c eq 1 then res=execute(Instr0+'=1') 
     if c ne 1 and strupcase(Instr0) ne 'PHYSICAL DIAGNOSTICS' then begin
	print,Instr0+' is not an allowed Instrument, switching to WL/PB.'
	LineName = 'PB'
	wl=1
	Instr0='WL'
     endif
endif


NNInst=0
;
; note kcor and wl both get set -- kluge fix here
;
countwl=0
if keyword_set(kcor) then if kcor eq 1 then countwl=countwl+1
if keyword_set(wl) then if wl eq 1 then countwl=countwl+1
if countwl eq 2 then wl=0
for i = 1,n_elements(all_inst)-1 do begin
  try=execute('test=keyword_set('+all_inst[i]+')')
  NNInst=NNINst+test
  if test eq 1 then instr0=all_inst[i]
;  print,nninst,instr0,keyword_set(all_inst[i]),all_inst[i]
endfor

if NNInst eq 0 and n_elements(LineName) eq 0 then begin
	LineName = 'PB'
	wl=1
	instr0='WL'
	NNInst=1
        message,/info,'No Instrument Specified. Assuming: PB'
endif

Case 1 of
   NNInst eq 1: begin

            case 1 of 
               exist(LineName): begin
;                  LineName=str_replace(LineName,'-','_')
;                  LineName=str_replace(LineName,'.','_')
                  try=execute('test=all_names.'+instr0+'_names')
                  try=execute('test2=all_lines.'+instr0+'_lines')
	          c=0
		  LinePos='NULL'
                  for jj = 0,n_elements(test)-1 do begin
	           LinePosTest=string(where(strpos(strupcase(LineName),strupcase(test[jj])) ge 0,cc))
	           if cc gt 0 then begin
		    if LinePos eq 'NULL' then begin
	              c=fix(cc)
                      LinePos=string(jj)
		    endif else begin
		     if strupcase(test[double(LinePos)]) eq strupcase(LineName) then cc=0 else begin
	               c=fix(cc)
                       LinePos=string(jj)
		     endelse
		    endelse
	           endif
	          endfor
		  if c le 0 then LinePos=string(where(strupcase(LineName) eq strupcase(test2),c))
		  if c gt 0 then begin
		    if strupcase(LineName) ne strupcase(test[LinePos[0]]) then line=linename else $
		       try=execute('line=all_names.'+instr0+'_names['+LinePos[0]+']')
		    try=execute('number=all_nums.'+instr0+'_nums['+LinePos[0]+']')
		    try=execute('label=all_labels.'+instr0+'_labels')
		    label=label+line
                    try=execute('type=all_types.'+instr0+'_type')
		  endif else begin
	 	    namelist=''
  		    space=', '
	            try=execute('for i = 0,n_elements(test)-1 do namelist = namelist + string(test[i]) + space')
	            try=execute('for i = 0,n_elements(test2)-1 do namelist = namelist + string(test2[i]) + space')
	            message,'Line names allowed for '+instr0+' are '+namelist
		  endelse
               end
               exist(LineNum): begin
                  try=execute('test=all_nums.'+instr0+'_nums')
                  LinePos=string(where(strupcase(LineNum) eq test,c))
		  if c gt 0 then begin
		    try=execute('line=all_names.'+instr0+'_names['+LinePos[0]+']')
		    try=execute('number=all_nums.'+instr0+'_nums['+LinePos[0]+']')
		    try=execute('label=all_labels.'+instr0+'_labels')
		    label=label+line
                    try=execute('type=all_types.'+instr0+'_type')
		  endif else begin
	 	    numlist=''
  		    space=', '
	            try=execute('for i = 0,n_elements(test)-1 do numlist = numlist + string(test[i]) + space')
	            message,'Line numbers allowed for '+instr0+' are '+numlist
		  endelse
               end
               else: begin
  		  try=execute('test=all_defaults.'+instr0+'_default')
		  defaultnum=string(test) 
		  try=execute('line=all_names.'+instr0+'_names['+defaultnum+']')
		  try=execute('number=all_nums.'+instr0+'_nums['+defaultnum+']')
		  try=execute('label=all_labels.'+instr0+'_labels')
                  try=execute('type=all_types.'+instr0+'_type')
		  label=label+line
                  message,/info,'Assuming instr='+instr0+' line= '+line
               end
            endcase

   endcase                      ;end case that Instrument set (NNInst = 1)

   NNInst eq 0: begin

                  LinePos=where(strupcase(LineName) eq strupcase(phys_params[*]),c)
		  if c gt 0 then begin
		    line=phys_params[LinePos[0]]
		    label=phys_labels[LinePos[0]-1]
		  endif

                  LinePos=where(strupcase(LineName) eq strupcase(model_params[*]),d)
		  if d gt 0 then begin
		    line=model_params[LinePos[0]]
		    label=model_labels[LinePos[0]-1]
		  endif

		  number=-99
		  type='NONE'
		  instr0='Physical Diagnostics'

		  if c le 0 and d le 0 then begin
	 	    	modlist=''
	 	    	physlist=''
			space=', '
	            	try=execute('for i = 1,n_elements(model_params)-1 do modlist = modlist +  model_params[i] + space')
	            	try=execute('for i = 1,n_elements(phys_params)-1 do physlist = physlist + phys_params[i] + space')
			message,'Physical diagnostics allowed are limited to '+physlist + ' model diagnostics to '+modlist+ 'for model CAVMORPH'
		  endif

   endcase                          ;end no Instrument set		

   NNInst gt 1: message,/info,'Please set only one Instrument'

endcase

if line eq 'Rotation Measure' then line='RM'
if line eq 'Rotation (angle)' then line='FR'
end

