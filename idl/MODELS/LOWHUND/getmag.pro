;*******************************************************************
pro getmag,lambda,adip,zo,x_o,r_o,bxex

; external B field evaluated at x = x_o, z = zo=sqrt(r_o^2-x_o^2)

; bxex= lambda^2*ro/4. - dipterm

dipterm=2.*zo*(x_o+r_o)
dipterm=dipterm/(zo*zo + (x_o+r_o)^2)
dipterm=dipterm/(zo*zo + (x_o+r_o)^2)
;
; note since adip is forced to be zero right now
; this will also result in it becoming zero
dipterm=dipterm*adip

lambda=sqrt((4./r_o)*(bxex+dipterm))

end
