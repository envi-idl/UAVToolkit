; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.

;+
; :Description:
;    Function that creates a virtual raster from the input image groups. The
;    raster has a dummy spatial reference applied so that the image can be
;    processed by image-registration routines.
;    
;    Note that each file must have the same dimensions.
;
; :Returns:
;   A virtual raster where each band represents one of the images in `groupFiles`.
; 
; :Params:
;    groupFiles: in, required, type=string[*]
;      Specify the files that represent an image group.
;
;
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
function bandAlignment_group_to_virtualRaster, groupFiles
  compile_opt idl2, hidden
  
  e = envi(/current)
  
  ;open as rasters
  rasters = list()
  foreach file, groupFiles do rasters.Add, e.openRaster(file)
  
  ;check for spatialref and make dummy one if not present
  if rasters[0].SPATIALREF eq !NULL then begin
    spatialref = ENVIStandardRasterSpatialRef($
      coord_sys_code = 4326, $
      /GEOGCS,$
      pixel_size = [0.00001d, 0.00001d], $
      tie_point_pixel = [0, 0], $
      tie_point_map = [0, 0])
  endif else begin
    spatialref = rasters[0].SPATIALREF
  endelse

  ;make a metaspectral raster
  return, ENVIMetaspectralRaster(rasters.toArray(), SPATIALREF = spatialref)
end