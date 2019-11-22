;h+
; Copyright (c) 2019 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT. See LICENSE.txt for additional details and information.
;h-

;+
; :Private:  
;
;-

;+
; TODO: save grid definition for regridding iamges so that it can be used elsewhere
;
; :Description:
;    Procedure that uses built-in ENVI tasks to perform the image-image
;    registration for band alignment. Instead of doing things in memory, this 
;    routine creates files on disk based on the `INPUT_REGISTRATION_TASK` that
;    is specified by the user.
;    
;    Once finished, this task then tiles over the stacked layers and turns off
;    pixels that do not have valid data values for each band. This helps remove
;    potential artifacts around the edges of images that might appear.
;    
;    Fot the stacking this task uses virtual rasters behind the scene which
;    will help make this process more efficient.
;
;
; :Keywords:
;    INPUT_RASTER: in, required, type=ENVIRaster
;      Specify the raster that you want to apply the `INPUT_TIEPOINTS` to.
;    INPUT_TIEPOINTS: in, required, type=BandAlignmentTiePoints
;      This should be set to a pointer array that contains the file coordinates
;      for the tiepoints that will be applied to the images.
;    IMAGE_REGISTRATION_TASK: in, requried, type=ENVITask, taskname=ImageToImageRegistration
;      Provide the task that you want to be used for image-image registration when
;      lining the bands up to one another.
;    OUTPUT_RASTER_URI: in, optional, type=string
;      Specifyt he fully-qualified output file that will be written to disk with the
;      bands registered to one another.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro BandAlignment_ApplyReferenceTiePointsWithENVI,$
  INPUT_RASTER = input_raster,$
  INPUT_BANDALIGNMENTTIEPOINTS = input_BandAlignmentTiePoints,$
  IMAGE_REGISTRATION_TASK = image_registration_task,$
  OUTPUT_RASTER_URI = output_raster_uri
  compile_opt idl2

  e = envi(/current)
  if (e eq !NULL) then begin
    e = envi(/headless)
  endif

  ;get info on our raster
  nBands = input_raster.NBANDS

  ;make sure that our tie points are valid
  if ~isa(input_BandAlignmentTiePoints, 'BandAlignmentTiePoints') then begin
    message, 'INPUT_BANDALIGNMENTTIEPOINTS is not a valid BandAlignmentTiePoints object, requried!'
  endif

  ;extract the tie points from our object
  use_tiepoints = input_BandAlignmentTiePoints.TIE_POINTS

  ;set our reference to the first invalid tiepoint set
  idxRef = where(~ptr_valid(use_tiepoints), countRef)
  if (countRef eq 0) then begin
    message, 'No reference band found within the INPUT_TIEPOINTS and all pointers are valid. Were they generated correctly?'
  endif
  reference_band = idxRef[0]

  ;create a lost of rasters to clean up
  clean = list()

  ;get base band
  baseRaster = ENVISubsetRaster(input_raster, BANDS = reference_band)
  clean.add, baseRaster

  ;preallocate an array to hold our rasters
  outputRasters = objarr(input_raster.NBANDS)

  ;save our base raster
  outputRasters[reference_band] = baseRaster

  ;iterate over each tiepoint task
  for i=0,nBands-1 do begin
    ;skip our base band
    if (i eq reference_band) then continue
    
    ;subset our raster
    bandRaster = ENVISubsetRaster(input_raster, BANDS = i)

    ;make a tiepointset
    tiePoints = ENVITiePointSet(TIEPOINTS = *use_tiepoints[i], $
      INPUT_RASTER1 = baseRaster, INPUT_RASTER2 = bandRaster)

    ;duplicate our registration task
    regTask = ENVITask('ImageToImageRegistration')
    regTask.INPUT_TIEPOINTS = tiePoints
    regTask.WARPING = image_registration_task.WARPING
    regTask.POLYNOMIAL_DEGREE = image_registration_task.POLYNOMIAL_DEGREE
    regTask.RESAMPLING = image_registration_task.RESAMPLING
    regTask.BACKGROUND = image_registration_task. BACKGROUND
    regTask.FULL_EXTENT = image_registration_task.FULL_EXTENT
    regTask.OUTPUT_PIXEL_SIZE = image_registration_task.OUTPUT_PIXEL_SIZE

    ;run the task
    regTask.execute

    ;save output raster and add to data collection
    outputRasters[i] = regTask.OUTPUT_RASTER
    clean.add, regTask.OUTPUT_RASTER

    ;close our band raster if not our reference band
    bandRaster.close
  endfor

  ;find the grid that covers the intersection of all of our data
  foreach raster, outputRasters, i do begin
    ;skip reference
    if (i eq reference_band) then continue

    ; Create a grid definition for the first raster
    Raster1CoordSys = ENVICoordSys(COORD_SYS_STR = baseRaster.SPATIALREF.COORD_SYS_STR)
    Raster1Grid = ENVIGridDefinition(Raster1CoordSys, $
      PIXEL_SIZE=baseRaster.SPATIALREF.PIXEL_SIZE, $
      NROWS=baseRaster.NROWS, $
      NCOLUMNS=baseRaster.NCOLUMNS, $
      TIE_POINT_MAP=baseRaster.SPATIALREF.TIE_POINT_MAP, $
      TIE_POINT_PIXEL=baseRaster.SPATIALREF.TIE_POINT_PIXEL)

    ; Create a grid definition for the second raster
    Raster2CoordSys = ENVICoordSys(COORD_SYS_STR = raster.SPATIALREF.COORD_SYS_STR)
    Raster2Grid = ENVIGridDefinition(Raster2CoordSys, $
      PIXEL_SIZE=raster.SPATIALREF.PIXEL_SIZE, $
      NROWS=raster.NROWS, $
      NCOLUMNS=raster.NCOLUMNS, $
      TIE_POINT_MAP=raster.SPATIALREF.TIE_POINT_MAP, $
      TIE_POINT_PIXEL=raster.SPATIALREF.TIE_POINT_PIXEL)

    ; Get the intersection of the two rasters
    grid_definition = ENVIGridDefinition(Raster1CoordSys, $
      EXTENT = Raster1Grid.Intersection(Raster2Grid), $
      PIXEL_SIZE=baseRaster.SPATIALREF.PIXEL_SIZE < raster.SPATIALREF.PIXEL_SIZE)

    ;regrid our reference
    baseRaster = ENVISpatialgridRaster(baseRaster, GRID_DEFINITION = grid_definition)
    clean.add, baseRaster
  endforeach

  ;regrid all of our rasters
  foreach raster, outputRasters, i do $
    outputRasters[i] = ENVISpatialgridRaster(raster, GRID_DEFINITION = grid_definition)
  clean.add, outputRasters, /EXTRACT

  ;stack together
  stackedRaster = ENVIMetaspectralRaster(outputRasters, $
    SPATIALREF = outputRasters[reference_band].SPATIALREF)
  clean.add, stackedRaster

  ;check if we want to write to disk
  if keyword_set(output_raster_uri) then begin
    ;init raster after updating metadata to have data ignore value
    meta = stackedRaster.METADATA
    meta['data ignore value'] = 0
    output_raster = ENVIRaster(INHERITS_FROM = stackedRaster, URI = output_raster_uri, METADAT = meta)
    
    ;create tile iterator
    createAwesomeTileIterator,$
      INPUT_RASTER = stackedRaster,$
      OUTPUT_SUB_RECTS = sub_rects
      
    ;tile over our raster
    foreach sub, sub_rects do begin
      ;read raster data
      dat = stackedRaster.getData(SUB_RECT = sub, PIXEL_STATE = ps, INTERLEAVE = 'BSQ')
      
      ;collapse pixel state
      ps = total(ps, 3, /INTEGER)
      
      ;find pixels to turn off
      idxOff = where(ps, countOff)
      if (countOff gt 0) then begin
        for z=0, nBands-1 do begin
          bandDat = dat[*,*,z]
          bandDat[idxOff] = 0
          dat[*,*,z] = temporary(bandDat)
        endfor
      endif
      
      ;write our data
      output_raster.setData, dat, SUB_RECT = sub
    endforeach
    
    ;finalize raster
    output_raster.save
    clean.add, output_raster
  endif

  ;clean up
  foreach r, clean do if isa(r, 'ENVIRaster') then r.close
end
