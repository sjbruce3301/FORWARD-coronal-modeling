; Define the get_closest function - find index of element in an array that is
  ; closest to given value
function get_closest, array, value

  ;array is provided array, value is the target value to get closest to

  ; Calculate the absolute differences between each element in the array and the value
  diffs = abs(array - value)

  ; Find the index of the minimum difference
  closest_index = min(diffs, index)

  ; Return the index of the closest element
  return, index
end
