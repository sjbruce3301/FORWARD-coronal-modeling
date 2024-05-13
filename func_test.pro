function func_test, starting, sum
  sum=0
  if (starting eq 0) then begin ;comment begin if
    print,"Don't use zero!"
  endif else begin
    for i=0,starting do begin
      sum = sum + i
    endfor
  endelse
return,sum
end
