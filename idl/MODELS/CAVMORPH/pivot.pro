;**********************************************************************
pro pivot,r,th,ph,rpiv,thpiv,phpiv,thcs,phcs,alpha,m

;       Oct 2022 -- changed array ( to [ (with help from Vema Panditi)

; pivot streamer for nonradiality
;
;inputs: r, th, ph, thcs, alpha,m (pramsstream)
;outputs: rpiv,thpiv,phpiv (gridpiv3d)
;	

; NOTE that thcs, phcs are in standard
; spherical coords, and represent central
; point of streamer/cavity

; now pivot around r=r' = 1, th=th'=thcs
; to get r',th'
; ACTUALLY this isnt really a pivot, it is
; a warping such that the sphere r=r'=1 does
; not change.  But it is a pivot of the central
; axis.  

thpiv= th - thcs - (alpha - asin(sin(alpha)/r))
rpiv=1. - cos(alpha) + sqrt( r^2 - sin(alpha)^2 )

; now figure out heliomagnetic coordinates

phpiv=ph-phcs

; now calculate ph center
; and fix boundary problems
; make sure phpiv lies between 0 and 2pi
; so that we can center it properly below 

test=where(abs(phpiv) gt 2.d0*!dpi)
if min(test) ne -1 then phpiv[test]=phpiv[test]-2.d0*!dpi
test=where(phpiv lt 0.d0)
if min(test) ne -1 then phpiv[test]=2.d0*!dpi + phpiv[test]

; but now center streamer on ph-phcs=0

test1=where(phpiv gt !dpi)
test2=where(phpiv lt -!dpi)
if min(test1) ne -1 then phpiv[test1]=-(2.d0*!dpi-phpiv[test1])
if min(test2) ne -1 then phpiv[test2]=2.d0*!dpi+phpiv[test2]

; now allow thpiv to vary with phpiv
; and vice verse for neutral lines not parallel to equator

thpiv= (thpiv + m*phpiv)/sqrt(1.d0+m*m)
phpiv= -thpiv*m + phpiv*sqrt(1.d0+m*m)
end


;******************************************************************************************
