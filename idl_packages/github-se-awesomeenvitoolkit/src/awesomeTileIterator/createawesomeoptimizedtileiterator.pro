;+
; :Description:
;    Tile iterator that generates tiles that are more optimized
;    for different image interleaves. Essentially it just ensures
;    that the sub_rects generated contain a full "column" of data.
;    
;    Ensuring that our tiles occupy full columns of information helps
;    reduce the number of jumps that happen when reading and writing
;    data. This can drastically improve read/write speeds (depending on
;    hardware and raster) up to about 15x at extremes. Speed improvements
;    are not guaranteed for all algorithms and do depend on what chunks
;    of your raster data you are extracting.
;    
;    The "column" of data is based on the interelave. For BIL and BSQ
;    the columns represent whole rows of pixels (and all bands) while 
;    the columns for BIP represent all the pixels for a given sample 
;    and line.
;    
;    The metric for generating tiles in this routine is the number of
;    pixels that you want per tile which takes into account the number
;    of bands in your raster.
;    
;    The sub-rects are generated with the intent of reading/writing all
;    bands of data at once. Performance will likely be affected for 
;    BIL and BIP interleaves if this is not followed.
;    
;    Note that this tiler does not support tile buffers like the
;    ```idl createAwesomeTileIterator``` procedure does.
;
;
;
; :Keywords:
;    INPUT_RASTER: in, required, type=ENVIRaster
;      Specify the raster that you want to generate tiles for.
;    PIXELS_PER_TILE: in, optional, default = 1024*1024
;      Optionally specify how many pixels you want a tile to approximately
;      have. Tiles are rounded up for some interleaves to ensure faster
;      processing times.
;    OUTPUT_SUB_RECTS: out, requried, type=list
;      List that contains the sub rects of data to read/write to a raster.
;    OUTPUT_TILE_SUB_RECTS: out, required, type=list, private
;      List that specifies the [xmin, ymin, xmax, ymax] of valid pixel
;      coordinates for the data. Not meant for general use.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro createAwesomeOptimizedTileIterator,$
  INPUT_RASTER = input_raster,$
  PIXELS_PER_TILE = pixels_per_tile,$
  OUTPUT_SUB_RECTS = output_sub_rects,$
  OUTPUT_TILE_SUB_RECTS = output_tile_sub_rects
  compile_opt idl2
  
  ;get current ENVI session
  e = awesomegetenvi()
  
  ;validate input
  if (input_raster eq !NULL) then begin
    message, 'INPUT_RASTER not specified, required!'
  endif
  
  ;check how many pixels per tile we want
  if (pixels_per_tile eq !NULL) then begin
    pixels_per_tile = 1024*ulong(1024)
  endif
  
  ;calculate the number of columns of pixel data that we want to extract
  nCols = float(pixels_per_tile)/input_raster.NBANDS
  
  ;get the number of rows
  nRows = nCols / input_raster.NCOLUMNS

  ;round up and cap if we have a float
  nCols  = ceil(nCols) < input_raster.NCOLUMNS 
  
  ;check if we have bsq or bil data - need at least one full row of data
  ;which means we need as many columns as our raster has - doesn't matter for bip
  if (input_raster.INTERLEAVE eq 'bsq') OR (input_raster.INTERLEAVE eq 'bil') then begin
    nCols >= input_raster.NCOLUMNS
  endif
  
  ;roun up nrows
  nRows = ceil(nRows) < input_raster.NROWS
  
  ;get our sub-rects
  createAwesomeTileIterator,$
    INPUT_RASTER = input_raster,$
    TILE_SIZE = [nCols, nRows],$
    OUTPUT_SUB_RECTS = output_sub_rects,$
    OUTPUT_TILE_SUB_RECTS = output_tile_sub_rects
end