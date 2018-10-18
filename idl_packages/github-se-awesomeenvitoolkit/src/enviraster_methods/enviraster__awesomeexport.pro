;+
; :Description:
;    Procedure method that, when used, will provide feedback to the
;    user on the progress for exporting a raster to disk. As is, you
;    have no indicator for how long it will take to export the raster
;    to disk and is a challenge when dealing with larger rasters.
;
; :Params:
;    output_raster_uri: in, required, type=string
;      Specify the fully-qualified filepath to a location
;      on disk where you want the source raster to be saved to.
;
; :Keywords:
;    DATA_IGNORE_VALUE: in, optional, type=number
;      Optionally specify a data ignore value for the output raster. 
;      If the input raster is a virtual raster then this should be
;      speicified so that the information will persist.
;    DEBUG: in, optional, type=boolean
;      If set, errors will be stopped on.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro ENVIRaster::AwesomeExport, output_raster_uri, DATA_IGNORE_VALUE = data_ignore_value, DEBUG = debug
  compile_opt idl2, hidden
  if ~keyword_set(debug) then on_error, 2
  
  ;hard coded parameters
  nPixMax = 2048*2048l   ;max number of pixels to export
  
  ;validate inputs
  inputValidator, hash('output_raster_uri', ['required', 'string'], 'data_ignore_value', ['number'])
  
  ;make sure our output file does not exist already
  if file_test(output_raster_uri) then begin
    message, 'output_raster_uri specified, but file exists already!', LEVEL = -1
  endif
  
  ;check if we need metadata for our output raster
  if keyword_set(data_ignore_value) then begin
    meta = ENVIRasterMetadata()
    meta['data ignore value'] = data_ignore_value
  endif
  
  ;initialize our output raster
  outRaster = ENVIRaster(INHERITS_FROM = self, METADATA = meta, URI = output_raster_uri)
  
  ;get our tiles for iterating
  createAwesomeOptimizedTileIterator,$
    PIXELS_PER_TILE = nPixMax,$
    INPUT_RASTER = self,$
    OUTPUT_SUB_RECTS = sub_rects
    
  ;get the number of tiles
  nTiles = n_elements(sub_rects)
  
  ;get how often to print
  nPrint = nTiles lt 100 ? 1 : ceil(float(nTiles)/100)
  
  ;catch errors
  catch, err
  if (err ne 0) then begin
    catch, /CANCEL
    prog.finish, /PRINT, /ERROR
    message, /REISSUE_LAST
  endif
  
  ;initialize our progress
  prog = awesomeENVIProgress('Awesome Export Raster', /PRINT)
  prog.setProgress, 'Processing...', 0, /PRINT
  
  ;loop over each tile
  foreach sub, sub_rects, i do begin
    ;get data from our raster
    dat = self.GetData(SUB_RECT = sub, PIXEL_STATE = ps)
    
    ;check if we have a data ignore value to preserve
    if keyword_set(data_ignore_value) then begin
      idxOff = where(ps, countOff)
      if (countOff gt 0) then dat[idxOff] = 0
    endif
    
    ;save data to disk
    outRaster.SetData, dat, SUB_RECT = sub
    
    ;check for abort
    prog.abortRequested
    
    ;update progress
    if ~(i mod nPrint) then prog.setProgress, 'Processing...', 100.0*(i+1.0)/nTiles, /PRINT, /TIME
  endforeach

  ;save the raster
  outRaster.save
  
  ;close progress
  prog.finish, /PRINT
end