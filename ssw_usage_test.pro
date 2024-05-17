; Ensure SolarSoft is initialized
@SSW/idl/setup/setup.ssw

; Define the path to the AIA FITS file
fits_file = 'path/to/your/aia_image.fits'

; Read the AIA image data
aia_read_image, fits_file, image, header

; Display the image using IDL's TV function
window, 0
tv, image

; Add some enhancements and annotations
loadct, 3  ; Load a color table
tv, bytscl(image, min=0, max=500)  ; Scale image intensity

; Display the header information
print, header

;ssw or SSW? use idl89?
