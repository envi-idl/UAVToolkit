pro awesomeSAMimageDifference, $
  INPUT_RASTER1 = input_raster1,$
  INPUT_RASTER2 = input_raster2,$
  OUTPUT_RASTER_URI = output_raster_uri,$
  OUTPUT_RASTER_UNITS = output_raster_units
  compile_opt idl2

  ;hard coded parameters
  pixels_per_tile = 4096*long(4096)
  debug = 1                 ;flag for debug, stops on errors to make debugging easy
  progress_print = 1        ;print progress messages to screen
  progress_time = 1         ;estimate time for progress to finish
  data_ignore_value = -1.0  ;sets the output data ignroe value, no angles should ever be this

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
  if (input_raster1.NCOLUMNS ne input_raster2.NCOLUMNS) OR (input_raster1.NROWS ne input_raster2.NROWS) then begin
    message, 'The input rasters do not have the same dimensions, required!'
  endif
  if (input_raster1.NBANDS ne input_raster2.NBANDS) then begin
    message, 'The input rasters do not have the number of bands, required!'
  endif
  if (input_raster1.NBANDS eq 1) then begin
    message, 'The input rasters do noth have more than one band, required!'
  endif
  
  ;check our units
  if (output_raster_units eq !NULL) then begin
    output_raster_units = 'Degrees'
  endif
  compare_units = strlowcase(strtrim(output_raster_units,2))

  ;check output keywords
  if ~keyword_set(output_raster_uri) then begin
    output_raster_uri = e.getTemporaryFilename()
  endif

  ;make a new metadata item
  meta = ENVIRasterMetadata()
  meta['band names'] = ['Awesome SAM Image Difference (' + output_raster_units + ')']
  meta['data ignore value'] = data_ignore_value ;defined at top of procedure

  ;initialize our output rasters
  output_raster = ENVIRaster(INHERITS_FROM = input_raster1, INTERLEAVE = 'BSQ', DATA_TYPE = 'float', NBANDS = 1, METADATA = meta, URI = output_raster_uri)
  
  ;specify the number of pixels we want per tile
  min_pix_per_tile = input_raster1.NBANDS*input_raster1.NCOLUMNS*2
  
  ;get our tiles
  createAwesomeOptimizedTileIterator,$
    INPUT_RASTER = input_raster1,$
    PIXELS_PER_TILE = (pixels_per_tile lt min_pix_per_tile)? min_pix_per_tile : pixels_per_tile,$
    OUTPUT_SUB_RECTS = sub_rects

  ;get the number of tiles that we need to process
  nTiles = n_elements(sub_rects)

  ;initialize our progress message
  prog = awesomeENVIProgress('Awesome SAM Image Difference', /PRINT)

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
    dat1 = float(input_raster1.getData(INTERLEAVE = 'BSQ', PIXEL_STATE = ps1, SUB_RECT = sub))
    dat2 = float(input_raster2.getData(INTERLEAVE = 'BSQ', PIXEL_STATE = ps2, SUB_RECT = sub))

    ;check for bad pixel values
    ps1 += (~finite(dat1))
    ps2 += (~finite(dat2))

    ;flatten, combine, and add any pixels of all zero to the mask
    ps = total(ps1, 3, /INTEGER) + total(ps2, 3, /INTEGER) + (total(dat1, 3) eq 0) + (total(dat2,3) eq 0)
    
    ;check what our units are
    case (compare_units) of
      'degrees': diff = acos(total(dat1*dat2, 3)/(sqrt(total(dat1^2,3))*sqrt(total(dat2^2,3))))*(1/!DTOR)
      'radians': diff = acos(total(dat1*dat2, 3)/(sqrt(total(dat1^2,3))*sqrt(total(dat2^2,3))))
    endcase

    ;check for pixels to turn off
    idxOff = where(ps OR ~finite(diff), countOff)

    ;apply bad pixels to result
    if (countOff gt 0) then begin
      diff[idxOff] = data_ignore_value
    endif

    ;save our data
    output_raster.setData, diff, SUB_RECT = sub

    ;set progress
    prog.SetProgress, 'Processing', 100*(float(idx + 1)/nTiles), PRINT = progress_print, TIME = progress_time
  endforeach

  ;save our output rasters
  output_raster.save

  ;finish progress
  prog.finish
end

e = envi(/HEADLESS)

raster1 = e.openRaster('\\fridge\envi_data\change_detection\christchurch\000000200885_01_P001_MUL-ortho\02OCT18223105-M1BS-ortho.img')
raster2 = e.openRaster('\\fridge\envi_data\change_detection\christchurch\000000200885_01_P002_MUL-ortho\05JAN21223334-M1BS-ortho.img')

;get the raster intersection
virtualRasterIntersection,$
  INPUT_RASTER1 = raster1,$
  INPUT_RASTER2 = raster2,$
  OUTPUT_RASTER1 = output_regrid1,$
  OUTPUT_RASTER2 = output_regrid2
tic

awesomeSAMimageDifference, $
  INPUT_RASTER1 = output_regrid1,$
  INPUT_RASTER2 = output_regrid2,$
  OUTPUT_RASTER_UNITS = 'Radians'
toc
stop
tic
Task = ENVITask('SAMImageDifference')
Task.INPUT_RASTER1 = output_regrid1
Task.INPUT_RASTER2 = output_regrid2
Task.Execute
toc
end