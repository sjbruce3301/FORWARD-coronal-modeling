pro tomo_sizearr,a,n0,n1,n2,n3,n4,n5,n6,n7,size=s,ndim=ndim
s=size(a)
ndim=s[0]
for i=1,ndim do void=execute('n'+int2str(i-1,1)+'=s['+int2str(i,1)+']')
end
