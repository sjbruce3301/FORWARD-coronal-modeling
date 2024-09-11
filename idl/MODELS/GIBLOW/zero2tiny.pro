pro zero2tiny, arr
 tiny=1d-4
 if n_elements(arr) eq 1 then begin
  if arr ge 0 and arr le tiny then arr = tiny
  if arr lt 0 and arr ge -tiny then arr = -tiny
 endif else begin
  test1=where(arr ge 0 and arr le tiny, nt1)
  test2=where(arr lt 0 and arr ge -tiny, nt2)
  if nt1 ne 0 then arr[test1]=tiny
  if nt2 ne 0 then arr[test2]=-tiny
 endelse
end
