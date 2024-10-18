function for_linear_mapping, v_in, new_data_min, new_data_max
  ; function for linear mapping between two ranges
  ; inputs:
  ; v_in: the input vector you want to map, range [min(vin),max(vin)]
  ; range: the range of the resulting vector
  ; output:
  ; vout: the resulting vector in range rout
  ; usage:
  ; >> v1 = linspace(-2,9,100);
  ; >> v2 = linmap(v1,[-5,5]);

  ;add smth to get min and max of data entered
  
  ;a and b are extrema of original data, c and d are new extrema
  
  a = MIN(v_in); %min
  b = MAX(v_in); %max
  c = new_data_min
  d = new_data_max
  v_out = ((c+d) + (d-c)*((2*v_in - (a+b))/(b-a)))/2;
  
  print,a
  print,b
  print,MIN(v_out)
  print,MAX(v_out)
  
  return, v_out
end