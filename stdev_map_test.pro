pro stdev_map_test

  temp_min = 0.2
  temp_max = 8
  temp_points = 10
  
  temp_range = temp_min + (temp_max - temp_min) * findgen(temp_points) / (temp_points - 1)
  dev_array = fltarr(n_elements(temp_range), 2)
  
  for i = 0, (n_elements(temp_range) - 1) do begin
    ; Call cram_sim function
    ;print,lamdaobs_range[i]
    t = cram_sim_spectrum(3500, 4600, 20, 1.5, temp_range[i])
    ;print,t

    ; Unpack results
    dev = t[0]
    ;    intor = result[1]
    ;    polp = result[2]
    ;print,intot
    ; Store results in spectrum array
    ;print,i
    dev_array[i, 0] = temp_range[i]
    dev_array[i,1] = dev
  endfor

  print,dev_array
end