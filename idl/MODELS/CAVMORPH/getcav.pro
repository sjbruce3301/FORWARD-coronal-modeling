;+**********************************************************************
pro getcav,r,th,ph,cavfn,cavheight,cavwidth,cavbot,fraclength,gamma,tunnel=tunnel,tunslope=tunslope
;
;Modifcations:
;	25-Apr-2011 Added TUNNEL option SEG
;   28-Apr-2011 Revised TUNNEL implimentation SEG
;	6-May-2011 Fixed Bug, SEG, TAK
;	9-May-2011 Revised for Tunnel with slope. TAK
;	10-May-2011 Tunnel related bug fixes. SEG & TAK
;	14-July-2011 fixed bug ziti --> ziti*ziti
;	21-March-21 fixed bug tunnel to fit to cavheight
;		not cavwidth (since vs phi_mg) SEG
;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)
;-
common pramsstream,thcs,phcs,alpha,m,awidth,bwidth,wmult
common cavprop,semmin,semmaj,ziti


;default,tunnel,0
; set in cavmorphprams


;****
; define semimajor and semiminor axes at center (transformed) longitude 
; of ellipse (phpiv=0)

semmaj0=cavheight/2.d0
semmin0=cavwidth/2.d0

wa=awidth+bwidth*(cavbot+semmaj0)
if semmin0 gt wa then print,'wa=',wa,'semmin=',semmin0,'cavity wider than streamer gaussian width'

;****
; if not in tunnel mode
; let semmaj and semmin vary with phpiv
; but keep aspect ratio constant

; redefine wa to be consistent with streamer width

wa=awidth+bwidth
wb=1.d0/(wa*wmult)
wb=wb/fraclength

if tunnel eq 0 then begin
 semmaj=semmaj0*exp(-(ph*wb)^2)
 semmin=semmin0*exp(-(ph*wb)^2)
endif
		
if tunnel eq 1 then begin
; default,tunslope,0. - this should be in cavmorphparms
; get rid of 1/e vs half-length conversion, only
; meaningful for Gaussian
 wbb = wb/sqrt(2.d0)
 tunfn=ph*tunslope/semmaj0 + 1.
 test=where((ph lt -1./wbb/2.) or (ph gt 1./wbb/2.),c)
 if c gt 0 then begin
 	tunfn[test]=0.
 endif
 semmaj=semmaj0*tunfn
 semmin=semmin0*tunfn
endif

;****
; shift coordinate system to be centered on ellipse, which
; in turn is positioned with bottom at position cavbot (cavbot=1 is photosphere)
; bear in mind th,ph are thpiv, phpiv, and are zero along th=thcs
;****

; create cartesian coordinates
; bear in mind phi dependence will be taken care of by 
; variation of semmaj and semmin with phpiv

xy=r*sin(th)
z=r*cos(th)

zp=z-1.d0

; now rotate by gamma
; note rotate clockwise instead of counterclockwise

rp=sqrt(zp*zp+xy*xy)
thp=atan(xy,zp)

xyp=rp*sin(thp-gamma)
zpp=rp*cos(thp-gamma)
zpp=zpp-(cavbot-1.d0)-semmaj

;****
; define cavity to be 1 within ellipse

cavfn=(xyp/semmin)^2 + (zpp/semmaj)^2

test=where(cavfn lt 1.)

;
; ziti will create a hollow tube rather than solid croissant
; the width of the tube wall is ziti, which varies from
; 0 to 1 and is fraction of ellipse radius
;

if ziti ne 0 then test = where(cavfn lt 1. and cavfn ge ziti*ziti)

if min(test) ne -1 then cavfn[test]=0.
test=where(cavfn gt 1.)
if min(test) ne -1 then cavfn[test]=1.

if min(test) ne -1 then semmin[test]=0.
if min(test) ne -1 then semmaj[test]=0.

end
