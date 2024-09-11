;+
; Project: FORWARD
;
; Name: FOR_SAVECUBE
;
; Purpose: Save reformatted numerical datacube in IDL save file
;
; Inputs:
;	r - spherical radial coordinate in solar radii from low to high
;	th - spherical theta coordinate ranging from 0 to pi from low to high
;	ph - spherical phi  coordinate ranging from -pi to pi from low to high
;       dens - corresponding density values in cm^-3
;       pres - corresponding pressure values in dyn/cm2
;       temp - corresponding density values in kelvin
;	Br - Radial component of magnetic field
;	Bth - Theta component of magnetic field
;	Bph - Phi component of magnetic field
;	axisym - set to 1 if data cube is axisymetric
;	hydro - Hydrodynamic assumption used. See make_my_cube.pro
;	simname - name of simulation file, used to title cube structure and 
;            for default output file
; Keyword Inputs
;	Vr - Radial component of velocity 
;	Vth - Theta component of velocity
;	Vph - Phi component of  velocity
; 	  NOTE - if these are all the same then they are actuall representing
;		VEL which is the magnitude of velocity along the field
;	FillingFactor - filling factor for main plasma population
;	Pop2FillingFactor - filling factor for second plasma population
;	Pop2Dens - density values for second plasma population in cm^-3
;       Pop2Temp - temperature values for second plasma population in kelvin
;       modelname - name of model if different from simname.
;	brphot1,brphot2,bthphot1,bthphot2,bphphot1,bphphot2 - field line 
;		footpoint magnetic vectory, only for /topology option
;		in make_my_cube
;	open,fllen -- also representing fieldlines for /topology option
;	magmod - whether the cube has magnetic field (usually does)
;	global - full sun sim
;	date - used e.g. by MAS
;	mheat  - used by mas
;	iondens - used by UV codes values in cm^-3
;	  note pres/temp electron for now
;	ionname - e.g., 'LyA', 'OVI1032'
;	OutFile - name to use for output file. Defaults supplied by program
;	OutDir - Output directory. Default is to use same directory as file 
;		specified in simname. Default is working directory. 
;		This keyword will override directory in UseOutFile
;
;Keyword Outputs
;	cubefile - name of output file
;	cubeStr - data cube
;
; Called by MAKE_MY_CUBE
;
; History
;       Created March 2011 L. Rachmeler
;       15-May-2012 Added UseOutFile OutDir, Cube keywords, documentation.  
;                               Added hydro parameter to cube
;                                                               T.A. Kucera
;       9-July-2012 Added modelname keyword and separated out filename and title keywords.
;                       If these are different then the title is the name of the model and 
;                       the filename is the name of the outfult file. Change OutFile to 
;                       CubeFile, UseOutFile to OutFile and Cube to CubeStr. 
;       10-Jan-2014 Added velocity
;       Jan 2014 added global keyword
;
; Version 2.0 July 2014
;		27-Jan-2016 added fillingfactor, pop2fillingfactor, pop2dens, pop2temp. TAK
;	Aug-2020 -- added iondens and ionname pass through
;		    also added magmod pass through
;-


pro for_savecube,r, th, ph, dens, pres, temp, br, bth, bph, axisym, hydro, simname,$
			vr=vr,vth=vth,vph=vph,$
			fillingfactor=fillingfactor,pop2fillingfactor=pop2fillingfactor,$
			pop2dens=pop2dens,pop2temp=pop2temp,$
			cubefile=cubefile,outfile=outfile,outdir=outdir,cubestr=cubestr,$
			modelname=modelname,global=global,date=date,mheat=mheat,$
			brphot1=brphot1,brphot2=brphot2,$
			bthphot1=bthphot1,bthphot2=bthphot2,$
			bphphot1=bphphot1,bphphot2=bphphot2,$
			iondens=iondens,ionname=ionname,magmod=magmod,$
			open=open,fllen=fllen

default,date,''
default,mheat,''
default,global,0

break_file, simname, disk_log, dir, filnam, ext, fversion, node,/last_dot
if n_elements(modelname) eq 1 then title=modelname else title=filnam

if exist(vth) and exist(vph) then begin
;if max(vr) gt 1d-5 and max(vth) gt 1d-5 and max(vph) gt 1d-5 then begin
 if exist(brphot1) then begin
  cube={$
     title   : title, $
     r       : r,$ ;r in solar radii from low to high!
     th      : th,$ ;theta 0-pi from low to high!
     ph      : ph,$ ;phi -pi to pi from low to high!
     dens    : dens,$ ; in cm-3
     pres    : pres,$ ; in dyn/cm2
     temp    : temp,$ ; in kelvin
     br      : br,$
     bth     : bth,$
     bph     : bph,$
     vr      : vr,$
     vth     : vth,$
     vph     : vph,$
     global  : global,$
     axisym  : axisym,$  ;set to 1 if your cube is axisymmetric!
     hydro	 : hydro,$  ;hydro setting used 
     brphot1 : brphot1,$
     brphot2 : brphot2,$
     bthphot1 : bthphot1,$
     bthphot2 : bthphot2,$
     bphphot1 : bphphot1,$
     bphphot2 : bphphot2,$
     open : open,$
     fllen: fllen,$
     topology: 1,$
     filename: filnam}
 endif else begin
  cube={$
     title   : title, $
     r       : r,$ ;r in solar radii from low to high!
     th      : th,$ ;theta 0-pi from low to high!
     ph      : ph,$ ;phi -pi to pi from low to high!
     dens    : dens,$ ; in cm-3
     pres    : pres,$ ; in dyn/cm2
     temp    : temp,$ ; in kelvin
     br      : br,$
     bth     : bth,$
     bph     : bph,$
     vr      : vr,$
     vth     : vth,$
     vph     : vph,$
     global  : global,$
     axisym  : axisym,$  ;set to 1 if your cube is axisymmetric!
     hydro	 : hydro,$  ;hydro setting used 
     filename: filnam}
 endelse
endif else begin
 if exist(brphot1) then begin
  cube={$
     title   : title, $
     r       : r,$ ;r in solar radii from low to high!
     th      : th,$ ;theta 0-pi from low to high!
     ph      : ph,$ ;phi -pi to pi from low to high!
     dens    : dens,$ ; in cm-3
     pres    : pres,$ ; in dyn/cm2
     temp    : temp,$ ; in kelvin
     br      : br,$
     bth     : bth,$
     bph     : bph,$
     vel      : vr,$
     global  : global,$
     axisym  : axisym,$  ;set to 1 if your cube is axisymmetric!
     hydro	 : hydro,$  ;hydro setting used 
     date	: date,$
     mheat	: mheat,$
     brphot1 : brphot1,$
     brphot2 : brphot2,$
     bthphot1 : bthphot1,$
     bthphot2 : bthphot2,$
     bphphot1 : bphphot1,$
     bphphot2 : bphphot2,$
     open : open,$
     fllen: fllen,$
     topology: 1,$
     filename: filnam}
 endif else begin
  cube={$
     title   : title, $
     r       : r,$ ;r in solar radii from low to high!
     th      : th,$ ;theta 0-pi from low to high!
     ph      : ph,$ ;phi -pi to pi from low to high!
     dens    : dens,$ ; in cm-3
     pres    : pres,$ ; in dyn/cm2
     temp    : temp,$ ; in kelvin
     br      : br,$
     bth     : bth,$
     bph     : bph,$
     vel      : vr,$
     global  : global,$
     axisym  : axisym,$  ;set to 1 if your cube is axisymmetric!
     hydro	 : hydro,$  ;hydro setting used 
     date	: date,$
     mheat	: mheat,$
     filename: filnam}
 endelse
endelse
if keyword_set(Pop2Fillingfactor) then $
            cube=add_tag(cube,Pop2FillingFactor,'POP2FILLINGFACTOR',index='TEMP')
if keyword_set(Pop2Temp) then cube=add_tag(cube,Pop2Temp,'POP2TEMP',index='TEMP')
if keyword_set(Pop2Dens) then cube=add_tag(cube,Pop2Dens,'POP2DENS',index='TEMP')
if keyword_set(fillingfactor) then $
            cube=add_tag(cube,fillingfactor,'FILLINGFACTOR',index='TEMP')

if keyword_set(IonDens) then begin
  cube=add_tag(cube,iondens,'IONDENS')
  if keyword_set(IonName) then begin
    cube=add_tag(cube,ionname,'IONNAME')
  endif else begin
    cube=add_tag(cube,'','IONNAME')
  endelse
endif
if n_elements(magmod) eq 1 then cube=add_tag(cube,magmod,'MAGMOD')

										;file nameing
										;if UseOutFile set use that instead of simname
if keyword_set(OutFile) then begin 
	break_file, OutFile, disk_log, dir, filnam, ext, fversion, node,/last_dot
	dir=disk_log+dir
endif  
						;add .dat extension
if ext ne '.dat' then ext=ext+'.dat'	

if keyword_set(OutDir) then dir=OutDir else dir=disk_log+dir

						;make sure directory exists and is writeable
if dir ne '' then $
	if not dir_exist(dir)  then begin
		file_mkdir,dir
		print,'Creating directory '+dir
	endif
;if not write_access(dir) then message,'No write access for directory '+dir


if keyword_set(OutFile) then cubefile=concat_dir(dir,filnam+ext) $
	else case hydro of 
		1: cubefile=concat_dir(dir,'for_hydro_exp_'+filnam+ext)
		2: cubefile=concat_dir(dir,'for_hydro_power_'+filnam+ext)
		3: cubefile=concat_dir(dir,'for_hydro_power_alt_'+filnam+ext)
		else: cubefile=concat_dir(dir,'for_'+filnam+ext)
	endcase
	
cube.filename=cubefile

save, cube, FILENAME=cubefile
print, 'cube saved as ', cubefile


end
