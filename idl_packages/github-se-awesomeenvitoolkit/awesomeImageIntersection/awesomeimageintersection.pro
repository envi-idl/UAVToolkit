pro awesomeImageIntersection, $
  DATA_IGNORE_VALUE = data_ignore_value,$
  GENERATE_PIXEL_STATE_MASK = generate_pixel_state_mask,$
  INPUT_RASTER1 = input_raster1,$
  INPUT_RASTER2 = input_raster2,$
  OUTPUT_RASTER1_URI = output_raster1_uri,$
  OUTPUT_RASTER2_URI = output_raster2_uri,$
  OUTPUT_MASK_RASTER_URI = output_mask_raster_uri
  compile_opt idl2
  
  ;hard coded parameters
  pixels_per_tile = 4096*long(4096)
  debug = 1             ;flag for debug, stops on errors to make debugging easy
  progress_print = 1    ;print progress messages to screen
  progress_time = 1     ;estimate time for progress to finish

  ;initial error catching
  if ~keyword_set(debug) then begin
    ;return to parent scope on error
    on_error, 2
    
    ;handle error
    catch, err
    if (err ne 0) then begin
      catch, /CANCEL
      ;do any opptional cleanup here that you might need (should be ok without)
      message, /REISSUE_LAST
    endif
  endif
  
  ;get current session of ENVI
  e = awesomeGetENVI()
  
  ;validate our input
  if (input_raster1 eq !NULL) then begin
    message, 'INPUT_RASTER1 not specified, required!'
  endif
  if (input_raster2 eq !NULL) then begin
    message, 'INPUT_RASTER2 not specified, required!'
  endif
  if (input_raster1.SPATIALREF eq !NULL) then begin
    message, 'INPUT_RASTER1 does not have spatial reference, required!'
  endif
  if (input_raster2.SPATIALREF eq !NULL) then begin
    message, 'INPUT_RASTER2 does not have spatial reference, required!'
  endif
  
  ;check output keywords
  if ~keyword_set(output_raster1_uri) then begin
    output_raster1_uri = e.getTemporaryFilename()
  endif
  if ~keyword_set(output_raster2_uri) then begin
    output_raster2_uri = e.getTemporaryFilename()
  endif
  if ~keyword_set(output_mask_raster_uri) then begin
    output_mask_raster_uri = e.getTemporaryFilename()
  endif
  
  ;get metadata
  meta1 = input_raster1.METADATA
  meta2 = input_raster2.METADATA
  
  ;check if we need to generate a pixel state mask
  ps_mask = keyword_set(generate_pixel_state_mask) 
  
  ;generate pixel state mask by default if we have no data ignore value on 
  ;either of our input rasters so that a user can mask data as expected
  if ~meta1.hasTag('data ignore value') then ps_mask = 1
  if ~meta2.hasTag('data ignore value') then ps_mask = 1
  
  ;check if we set a data ignore value specified by the user
  if (data_ignore_value ne !NULL) then begin
    meta1['data ignore value'] = data_ignore_value
    meta2['data ignore value'] = data_ignore_value
  endif
  
  ;get the raster intersection
  virtualRasterIntersection,$
    INPUT_RASTER1 = input_raster1,$
    INPUT_RASTER2 = input_raster2,$
    OUTPUT_RASTER1 = output_regrid1,$
    OUTPUT_RASTER2 = output_regrid2,$
    OUTPUT_GRID_DEFININTION = grid_def
  
  ;initialize our output rasters
  output_raster1 = ENVIRaster(INHERITS_FROM = output_regrid1, INTERLEAVE = 'BSQ', METADATA = meta1, URI = output_raster1_uri)
  output_raster2 = ENVIRaster(INHERITS_FROM = output_regrid2, INTERLEAVE = 'BSQ', METADATA = meta2, URI = output_raster2_uri)
  
  ;initialize pixel state mask if necessary
  if ps_mask then begin
    tempSubset = ENVISubsetRaster(output_regrid1)
    output_mask_raster = ENVIRaster(DATA_TYPE = 'byte', INTERLEAVE = 'BSQ', INHERITS_FROM = tempSubset, URI = output_mask_raster_uri)
    tempSubset.close
  endif
  
  ;get our tiles
  createAwesomeOptimizedTileIterator,$
    INPUT_RASTER = output_regrid1,$
    PIXELS_PER_TILE = pixels_per_tile,$
    OUTPUT_SUB_RECTS = sub_rects
 
  ;get the number of tiles that we need to process
  nTiles = n_elements(sub_rects)

  ;initialize our progress message
  prog = awesomeENVIProgress('Awesome Raster Intersection', /PRINT)
  
  ;set first update
  prog.SetProgress, 'Processing', 0, PRINT = progress_print, TIME = progress_time

  ;second catch block once we have initialized our error message
  if ~keyword_set(debug) then begin
    ;cancel original error catch
    catch, /CANCEL

    ;new block to clean up progress and anything else
    catch, err
    if (err ne 0) then begin
      catch, /CANCEL

      ;clean up progress and anything else before issue same error message
      prog.finish
      message, /REISSUE_LAST
    endif
  endif

  ;process our data
  foreach sub, sub_rects, idx do begin
    ;extract data from each raster    
    dat1 = output_regrid1.getData(INTERLEAVE = 'BSQ', PIXEL_STATE = ps1, SUB_RECT = sub)
    dat2 = output_regrid2.getData(INTERLEAVE = 'BSQ', PIXEL_STATE = ps2, SUB_RECT = sub)
    
    ;flatten and combine our pixel state masks
    if (output_regrid1.NBANDS gt 1) then begin
      ps1 = total(ps1, 3, /INTEGER)
    endif
    if (output_regrid2.NBANDS gt 1) then begin
      ps2 = total(ps2, 3, /INTEGER)
    endif
    
    ;combine pixel state: values greater than zero represent pixels to ignore
    ps = temporary(ps1) + temporary(ps2)
    
    ;get the tile dimensions
    tileDims = [sub[2] - sub[0] + 1, sub[3] - sub[1] + 1]
    
    ;check if we have to turn off pixels
    idxOff = where(ps, countOff)
    if (countOff gt 0) then begin
      ;get x and y indices of our locations to turn off
      xIdx = idxOff mod tileDims[0]
      yIdx = idxOff/tileDims[0]
      
      ;check if we need to apply our pixel state if we have data ignore values
      if meta1.hasTag('data ignore value') then begin
        for i=0, output_regrid1.NBANDS -1 do begin
          dat1[xIdx,yIdx,replicate(i, countOff)] = meta1['data ignore value']
        endfor
      endif
      if meta2.hasTag('data ignore value') then begin
        for i=0, output_regrid2.NBANDS -1 do begin
          dat2[xIdx,yIdx,replicate(i, countOff)] = meta2['data ignore value']
        endfor
      endif
    endif

    ;save our data
    output_raster1.setData, dat1, SUB_RECT = sub
    output_raster2.setData, dat2, SUB_RECT = sub
    if ps_mask then output_mask_raster.setData, ps eq 0, SUB_RECT = sub
    
    ;set progress
    prog.SetProgress, 'Processing', 100*(float(idx + 1)/nTiles), PRINT = progress_print, TIME = progress_time
  endforeach
  
  ;save our output rasters
  output_raster1.save
  output_raster2.save
  if ps_mask then output_mask_raster.save
  
  ;finish progress
  prog.finish
end

e = envi(/HEADLESS)

raster1 = e.openRaster('C:\some\056395450010_01_assembly_sub_bil.dat')
raster2 = e.openRaster('C:\some\057198000010_01_assembly_sub_bil.dat')

output_raster1_uri = !NULL
output_raster2_uri = !NULL
tic
awesomeImageIntersection, $
  INPUT_RASTER1 = raster1,$
  INPUT_RASTER2 = raster2,$
  OUTPUT_RASTER1_URI = output_raster1_uri,$
  OUTPUT_RASTER2_URI = output_raster2_uri,$
  OUTPUT_MASK_RASTER_URI = output_mask_raster_uri
toc

tic
task = ENVITask('ImageIntersection')
task.INPUT_RASTER1 = raster1
task.INPUT_RASTER2 = raster2
task.execute
toc
print

raster1 = e.openRaster('C:\some\056395450010_01_assembly_sub_bip.dat')
raster2 = e.openRaster('C:\some\057198000010_01_assembly_sub_bip.dat')

output_raster1_uri = !NULL
output_raster2_uri = !NULL
tic
awesomeImageIntersection, $
  INPUT_RASTER1 = raster1,$
  INPUT_RASTER2 = raster2,$
  OUTPUT_RASTER1_URI = output_raster1_uri,$
  OUTPUT_RASTER2_URI = output_raster2_uri,$
  OUTPUT_MASK_RASTER_URI = output_mask_raster_uri
toc
tic
task = ENVITask('ImageIntersection')
task.INPUT_RASTER1 = raster1
task.INPUT_RASTER2 = raster2
task.execute
toc
print

raster1 = e.openRaster('C:\some\056395450010_01_assembly_sub.dat')
raster2 = e.openRaster('C:\some\057198000010_01_assembly_sub.dat')

output_raster1_uri = !NULL
output_raster2_uri = !NULL
output_mask_raster_uri = !NULL
tic
awesomeImageIntersection, $
  INPUT_RASTER1 = raster1,$
  INPUT_RASTER2 = raster2,$
  OUTPUT_RASTER1_URI = output_raster1_uri,$
  OUTPUT_RASTER2_URI = output_raster2_uri,$
  OUTPUT_MASK_RASTER_URI = output_mask_raster_uri
toc

tic
task = ENVITask('ImageIntersection')
task.INPUT_RASTER1 = raster1
task.INPUT_RASTER2 = raster2
task.execute
toc
end