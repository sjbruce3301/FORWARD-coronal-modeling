;*******************************************************************
pro getbzbx,z,x,r,zeta,lambda,adip,a1,bzin,bzout,bxin,bxout,zeta_o,r_o

dasheardrout = -(r_o^2*lambda^2/4.d0)/r
dasheardrin = -r*lambda^2/4.d0

dzetadx = zeta/(x+r_o) + 2.d0*zeta^2
dzetadz = zeta^2*2.*z/(x+r_o) 

;
; note for now adip forced to zero
;
dadxout=dasheardrout*x/r + adip*dzetadx
dadxin=dasheardrin*x/r + adip*dzetadx + 4.d0*!dpi*a1*(1.d0/zeta + 1.d0/zeta_o)*dzetadx

dadzout=dasheardrout*z/r + adip*dzetadz
dadzin=dasheardrin*z/r + adip*dzetadz + 4.d0*!dpi*a1*(1.d0/zeta + 1.d0/zeta_o)*dzetadz

bzin=dadxin
bzout=dadxout
bxin=-dadzin
bxout=-dadzout

;print,minmax(4.d0*!dpi*a1*(1.d0/zeta + 1.d0/zeta_o)*dzetadz)
;print,minmax(dasheardrin*z/r)
;print,minmax(adip*dzetadz)
;print,min(bzin),max(bzout),min(bxin),max(bxout)
end
