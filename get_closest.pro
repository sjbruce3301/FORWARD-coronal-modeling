; Define the get_closest function
function get_closest, array, value
  ; Find the index of the element in 'array' that is closest to 'value'
  ; 'array' is a 1D array
  ; 'value' is the target value to find the closest element to

  ; Calculate the absolute differences between each element in the array and the value
  diffs = abs(array - value)

  ; Find the index of the minimum difference
  closest_index = min(diffs, index)

  ; Return the index of the closest element
  return, index
end
