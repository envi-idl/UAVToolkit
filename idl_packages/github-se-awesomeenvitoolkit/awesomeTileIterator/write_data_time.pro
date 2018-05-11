pro write_data_time
  compile_opt idl2

  ;get current envi
  e = envi(/HEADLESS)

  ;dims of our data
  nBands = 8
  nColumns = 9000
  nRows = 5000

;  ;###########################################
;  ; process with square tiles
;
;  ;set up an output raster
;  output_raster = ENVIRaster( $
;    INTERLEAVE = 'BSQ',$
;    NBANDS = nBands, $
;    NCOLUMNS = nColumns, $
;    NROWS = nRows, $
;    DATA_TYPE = 'uint')
;
;  ;create tile iterator
;  createAwesomeTileIterator,$
;    INPUT_RASTER = output_raster,$
;    TILE_SIZE = [1024, 1024],$
;    OUTPUT_SUB_RECTS = sub_rects
;
;  ;process
;  tic
;  foreach sub, sub_rects, idx do begin
;    ; get the tile size
;    tileSize = [sub[2] - sub[0] + 1, sub[3] - sub[1] + 1]
;
;    ;make some data
;    dat = uintarr(tileSize[0], tileSize[1], output_raster.NBANDS, /NOZERO)
;
;    ;write the data
;    output_raster.setData, dat, SUB_RECT = sub
;  endforeach
;  print, 'Time with square tiles and all bands : ' + strtrim(toc(),2)
;  output_raster.save
;
;
;
;  ;###########################################
;  ; process with optimized tiles for BSQ interleave and all bands at once
;
;  ;set up an output raster
;  output_raster = ENVIRaster( $
;    INTERLEAVE = 'BSQ',$
;    NBANDS = nBands, $
;    NCOLUMNS = nColumns, $
;    NROWS = nRows, $
;    DATA_TYPE = 'uint')
;
;  ;create tile iterator
;  createAwesomeOptimizedTileIterator,$
;    INPUT_RASTER = output_raster,$
;    PIXELS_PER_TILE = long(1024)*1024,$
;    OUTPUT_SUB_RECTS = sub_rects
;
;  ;process
;  tic
;  foreach sub, sub_rects, idx do begin
;    ; get the tile size
;    tileSize = [sub[2] - sub[0] + 1, sub[3] - sub[1] + 1]
;
;    ;make some data
;    dat = uintarr(tileSize[0], tileSize[1], output_raster.NBANDS, /NOZERO)
;
;    ;write the data
;    output_raster.setData, dat, SUB_RECT = sub
;  endforeach
;
;  print, 'Time with optimized tiles and all bands: ' + strtrim(toc(),2)
;  output_raster.save
;
;
  ;###########################################
  ; process with optimized tiles for BSQ interleave by each band

  ;set up an output raster
  output_raster = ENVIRaster( $
    INTERLEAVE = 'BSQ',$
    NBANDS = nBands, $
    NCOLUMNS = nColumns, $
    NROWS = nRows, $
    DATA_TYPE = 'uint')

  ;create tile iterator
  createAwesomeOptimizedTileIterator,$
    INPUT_RASTER = output_raster,$
    PIXELS_PER_TILE = long(1024)*1024,$
    OUTPUT_SUB_RECTS = sub_rects

  ;process
  tic
  for i=0, 7 do begin
    foreach sub, sub_rects, idx do begin
      ; get the tile size
      tileSize = [sub[2] - sub[0] + 1, sub[3] - sub[1] + 1]

      ;make some data
      dat = uintarr(tileSize[0], tileSize[1], /NOZERO)

      ;write the data
      output_raster.setData, dat, SUB_RECT = sub, BANDS = i
    endforeach
  endfor

  print, 'Time with BSQ optimized tiles : ' + strtrim(toc(),2)
  output_raster.save
  stop
  ;###########################################
  ; process with optimized tiles for BIL interleave

  ;set up an output raster
  output_raster = ENVIRaster( $
    INTERLEAVE = 'BIL',$
    NBANDS = nBands, $
    NCOLUMNS = nColumns, $
    NROWS = nRows, $
    DATA_TYPE = 'uint')

  ;create tile iterator
  createAwesomeOptimizedTileIterator,$
    INPUT_RASTER = output_raster,$
    PIXELS_PER_TILE = long(1024)*1024,$
    OUTPUT_SUB_RECTS = sub_rects

  ;process
  tic
  foreach sub, sub_rects, idx do begin
    ; get the tile size
    tileSize = [sub[2] - sub[0] + 1, sub[3] - sub[1] + 1]

    ;make some data
    dat = uintarr(tileSize[0], nBands, tileSize[1], /NOZERO)

    ;write the data
    output_raster.setData, dat, SUB_RECT = sub
  endforeach

  print, 'Time with BIL optimized tiles : ' + strtrim(toc(),2)
  output_raster.save
  stop

  ;###########################################
  ; process with optimized tiles for BIP interleave
  
  ;set up an output raster
  output_raster = ENVIRaster( $
    INTERLEAVE = 'BIP',$
    NBANDS = nBands, $
    NCOLUMNS = nColumns, $
    NROWS = nRows, $
    DATA_TYPE = 'uint')
  
  ;create tile iterator
  createAwesomeOptimizedTileIterator,$
    INPUT_RASTER = output_raster,$
    PIXELS_PER_TILE = long(1024)*1024,$
    OUTPUT_SUB_RECTS = sub_rects
  
  ;process
  tic
  foreach sub, sub_rects, idx do begin
    ; get the tile size
    tileSize = [sub[2] - sub[0] + 1, sub[3] - sub[1] + 1]
  
    ;make some data
    dat = uintarr(nBands, tileSize[0], tileSize[1], /NOZERO)
  
    ;write the data
    output_raster.setData, dat, SUB_RECT = sub
  endforeach
  toc

  print, 'Time with BIP tiles : ' + strtrim(toc(),2)
  output_raster.save
end