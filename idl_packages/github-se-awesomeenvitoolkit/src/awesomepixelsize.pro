;+
; :Description:
;    Function that returns the pixel size of an ENVIRaster in meters. This
;    function returns a 2 element array for X and Y pixel sizes.
;
; :Params:
;    raster: in, required, type=ENVIRaster
;
; :Example:
;
;    raster = e.openraster('C:\Program Files\Harris\ENVI54\data\qb_boulder_msi')
;    pixSize =  raster_pixel_size(raster)
;
; :Author: Zachary Norman
;-
function awesomePixelSize, raster
  compile_opt idl2
  on_error, 2

  ;make sure ENVI has started
  e = envi(/current)
  if (e eq !NULL) then begin
    message, 'ENVI has not started yet, requried!'
  endif

  ;make sure we passed in a spatial reference
  if (raster eq !NULL) then begin
    message, '"raster" argument not set, required!'
  endif

  ;get raster center latitude for rasters, leaving code here for reference
  raster._Component->GetProperty, CENTER_LATITUDE = refLat

  ;get the raster pixel size
  pixelSize = raster.SpatialRef._Component.Pixel_Size

  ;get the units for the spatial reference
  units = raster.SpatialRef._Component.Units

  ;convert the pixel size to meters
  pixelSizeMeters = IDLcfProjUnitsConvertValue(pixelSize,  units, IDLcfProjUnitsTranslate('Meters'),  REFERENCE_LATITUDE = refLat)

  ;return values
  return, pixelSizeMeters
end