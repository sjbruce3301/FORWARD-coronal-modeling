pro for_create_ct

; create a custom color table to add to the standard
; right now the example below creates color table 41 
; which is red-blue, but with white at position 131 so 
; that it is in the center of our color table after
; we add the contour colors
;
; NOT CALLED IN FORWARD 
; USE TO SET UP/UPDATE COLOR TABLE
;
; Written by Laurel Rachmeler 2011-2012
;
; JUNE 2014 ADDED 42 WHERE BLUE IS NEGATIVE! SG
;
; Version 2.0 July 2014
;--

r = reverse([2*indgen(125)+6,bytarr(131)+255])
g = [0,0,0,2*indgen(128),254-2*indgen(125)]
b = reverse([bytarr(125)+255,254-2*indgen(131)])
b[0:2]=0

tvlct,r,g,b
modifyct,41,'blue-red',r,g,b,file='for_colors.tbl'

b = reverse([2*indgen(125)+6,bytarr(131)+255])
g = [0,0,0,2*indgen(128),254-2*indgen(125)]
r = reverse([bytarr(125)+255,254-2*indgen(131)])
r[0:2]=0

tvlct,r,g,b
modifyct,42,'red-blue',r,g,b,file='for_colors.tbl'

end
