pro for_outputdefaults,$
	saveprams=saveprams,savemap=savemap,mapname=mapname,$
	extratitle=extratitle,$
        noerase=noerase,moreplots=moreplots,$
        gif=gif,tiff=tiff,jpeg=jpeg,$
        psplot=psplot,eps=eps,$
	OutputInputs=OutputInputs

;
; this program sets all defaults for outputs
;
; Optional output keyword OutputInputs gathers all keyword inputs
;       useful for widget setup
;
; Keywords defined below
;  
; Called by FOR_DRIVE, FOR_WIDGET
;
; Written 2013 Sarah Gibson
; Version 2.0 July 2014


;  INPUT keywords that control how the code is called and run
;
;      keyword SAVEPRAMS (string), if set, saves (input) parameters to file
;		default '' unset
;

default,saveprams,''


;
;
;      keyword SAVEMAP (1 or 0), if set, saves all the output structures described below
;               to a save file called  MAPNAME.sav. Internal to FOR_DRIVE.
;

default,savemap,0

;      keyword MAPNAME (string) defaults to name of model+observable+output type.
;               default set here as then this is overwritten
;               once these elements are established within FOR_DRIVE.
;               ***note -- mapname is used for any output, whether it is
;               a IDL save file, plot, or fits. So is
;               used in codes FOR_PLOT, FOR_PLOTFITS, FOR_SAVE_PLOT, and PLOT_COMP_QUICKINV.
;               Since FOR_PLOT and FOR_SAVE_PLOT can be called external to FOR_DRIVE (e.g. from
;               FOR_PLOTFITS and PLOT_COMP_QUICKINV) a default mapname is set also in these codes***
;
;               ***SAVE FILE VIA FOR_DRIVE,/SAVEMAP,MAPNAME='myfile' ***
;               ***RESTORE FILE VIA FOR_DRIVE,READMAP='myfile.sav'
;               ***NOTE READMAP IS SET AND DOCUMENTED IN FOR_SETTINGSDEFAULTS***

	default,mapname,'defaults'

;
;      keyword  MOREPLOTS (1 or 0) - useful when multiple plots are being made using NOERASE
;               basically keeps postscript file device open while set.
;		in widget it also allows multiple x windows open at once
;               Internal to FOR_DRIVE
;

default,moreplots,0

;
;       keyword EXTRATITLE (string) -- extra information for title
;               used in figures and also added to output mapname
;               (e.g., name of parameter being varied)
;               used in various names, i.e. in FOR_DRIVE and FOR_POS_MAP
;               Default '' means unset
;

default, extratitle, ''


;
;   inputs to FOR_PLOT
;
;	NOTE - these are all essentially repeated in FOR_PLOTFITS and 
;		PLOT_COMP_QUICKINV

;
;       keyword NOERASE (0/1) - don't erase
;

default,noerase,0



;
;       keyword PSPLOT (0/1) make postscript file 
; 	keyword	EPS (0/1) make eps 
;		Default X plots, no output
;
 
default,psplot,0
if psplot eq 1 then default,eps,1 else default,eps,0

;
;   inputs to FOR_SAVE_PLOT
;	
;	keywords GIF, TIFF, JPEG (1/0)
;	Defaults 0
;

default,gif,0
default,tiff,0
default,jpeg,0

OutputInputs={SavePrams:saveprams,SavePramsVal:'string',SaveMap:savemap,SaveMapVal:'tog',MapName:mapname,MapNameVal:'string',$
MorePlots:moreplots,MorePlotsVal:'tog',$
	ExtraTitle:extratitle,ExtraTitleVal:'string',NoErase:noerase,NoEraseVal:'tog',$	
 	PsPlot:psplot,PsPlotVal:'tog',$
	EPs:eps,EPsVal:'nodisplay',$
;        Gif:gif,GifVal:'tog',$
;Jpeg:jpeg,JpegVal:'tog',$
Tiff:tiff,TiffVal:'tog'$
}
end
