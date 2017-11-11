; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.

;+
; :Private:
;   not currently used.
;
; :Description:
;    Converts image groups to metaspectral (virtual) rasters. Needed for the updated
;    routines that perform band-band registration. This makes the separate files
;    on disk behave in the same manner as a single raster on disk. It simplifies the
;    processing significantly.
;
; :Returns:
; 
;   An ENVIRaster object of the group that was passed in.
;  
; :Params:
;    image_groups: in, requried, type=hash/orderedhash
;      Key-value pair of folder and group name with a value of all
;      the fully-qualified filepaths for the images that belong to the
;      image group.
;
; :Keywords:
;    BANDORDER: in, optional, default=indgen(nbands)
;      The optional order for the bands in the virtual raster group. If specified
;      then the bands will be re-ordered based on the value of this array. For
;      example, MicaSense RedEdge used BANDORDER = [0, 1, 2, 4, 3]
;    GET_SPATIALREF: in, optional, type=boolean
;      If specified, a spatial reference will be built from the metadata associated
;      with the scenes, otherwise no spatialref will be used.
;    SPATIALREF: in, optional, type=ENVIStandardrasterSpatialref
;      If you know the spatial reference for your iamge group this can be passed
;      in and will be applied to the metaspectral rasters that are generated.
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
function image_groups_to_virtual_rasters, image_groups,$
  BANDORDER = bandorder,$
  GET_SPATIALREF = get_spatialref,$
  SPATIALREF = spatialref
  compile_opt idl2

  ;check for ENVI
  e = envi(/current)
  if (e eq !NULL) then begin
    e = envi(/headless)
  endif

  ;create an orderedhash
  rasterized_groups = orderedhash()

  ;get all the image groups that we are going to work with
  keys = image_groups.keys()

  nImages = n_elements(image_groups[keys[0]])

  ;check if we don't pass in the band order
  if ~keyword_set(bandorder) then begin
    bandorder = indgen(nImages)
  endif

  ;loop over each image group
  foreach key, keys do begin
    ;check if we need to get our spatialref
    images = image_groups[key]

    if keyword_set(get_spatialref) AND ~keyword_set(spatialref) then begin
      oinfo = image_info(images[0], /NO_PRINT)
      spatialref = oinfo.Get('SPATIALREF')
    endif

    ;preallocate an array to hold our rasters
    rasters = objarr(nImages)

    ;open each image as a raster
    for i=0,nImages-1 do begin
      case (1) of
        keyword_set(get_spatialref):rasters[i] = e.openraster(images[i], SPATIALREF_OVERRIDE = spatialref)
        keyword_set(spatialref):rasters[i] = e.openraster(images[i], SPATIALREF_OVERRIDE = spatialref)
        else:rasters[i] = e.openraster(images[i])
      endcase
    endfor

    ;build our metaspectral raster
    case (1) of
      keyword_set(get_spatialref):metaraster = ENVIMetaspectralRaster(rasters[bandorder], SPATIALREF = spatialref)
      keyword_set(spatialref):metaraster = ENVIMetaspectralRaster(rasters[bandorder], SPATIALREF = spatialref)
      else:metaraster = ENVIMetaspectralRaster(rasters[bandorder], SPATIALREF = rasters[0].SPATIALREF)
    endcase

    ;save the stacked raster
    rasterized_groups[key] = metaraster
  endforeach

  ;return our ordered hash
  return, rasterized_groups
end