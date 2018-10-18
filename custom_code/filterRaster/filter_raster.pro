;h+
; (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT. See LICENSE.txt for additional details and information.
;h-

;+
; :Private:
;  Routine to apply sobel filter to raster to try and improve band-band registration
;
;
; :Keywords:
;    INPUT_RASTER
;    FILTER
;    OUTPUT_RASTER_URI
;
; :Author: znorman
;-
pro filter_raster,$
  INPUT_RASTER = input_raster,$
  FILTER = filter,$
  OUTPUT_RASTER_URI = output_raster_uri
  compile_opt idl2
  
  e = envi(/current)
  if (e eq !NULL) then begin
    e = envi(/headless)
  endif
  
  if (filter eq !NULL) then begin
    filter = 'roberts'
  endif

  ;make sure the output filename is specified
  if (output_raster_uri eq !NULL) then begin
    output_raster_uri = e.GettemporaryFilename()
  endif
  
  ;get info on our raster
  dims = [input_raster.NCOLUMNS, input_raster.NROWS] 
  nbands = input_raster.NBANDS

  ;initialize our tile iterator
  ;add a buffer of 3 to each tile since our kernels are mostly 3 x 3 (i think)
  createAwesomeTileIterator,$
    INPUT_RASTER = input_raster,$
    TILE_BUFFER = 3, $
    TILE_SIZE = [20480, 20480],$
    NCOLUMNS = ncolumns,$
    NROWS = nrows,$
    OUTPUT_SUB_RECTS = output_sub_rects,$
    OUTPUT_RASTER_TILE_LOCATIONS = output_raster_tile_locations,$
    OUTPUT_TILE_SUB_RECTS = output_tile_sub_rects
  

  ;get original raster metadata
  orig_meta = input_raster.meta.dehydrate()
  
  ;set up our output raster
  meta = ENVIRasterMetadata()
  meta.AddItem, 'band names',  filter.CapWords() + ' Filter: ' + orig_meta['BAND NAMES']

  ;tile!
  foreach sub_rect, output_sub_rects, i do begin
    ;get the data
    tile_data = input_raster.GetData(SUB_RECT = sub_rect, INTERLEAVE = 'BSQ', PIXEL_STATE = ps)
        
    ;update any nans
    idxBad = where(~finite(tile_data), countBad, COMPLEMENT = idxGood, NCOMPLEMENT = countGood)
    if (countBad gt 0) then begin
      if (countGood eq 0) then begin
        message, 'No valid data to process....'
      endif
      
      stop
      
      ;get tile size
      tDims = size(tile_data, /DIMENSIONS)
      
;      ps[idxBad]++
;      tile_data[idxBad] = fix(interpolate(tile_data[idxGood], idxGood mod tDims[0], idxGood / tDims[0], $
;        idxBad mod tDims[0], idxBad / tDims[1]), TYPE=tile_data.TYPECODE) 
    endif
    
    ;do edge detection for every band
    ;ensure that the type of the data matches the original
    ;this should be true for the filter functions, but it is imporant just to make sure
    for j=0, nbands-1 do begin
      case strlowcase(filter) of
        'sobel'      : edgeBand = sobel(tile_data[*,*,j])
        'prewitt'    : edgeBand = prewitt(tile_data[*,*,j])
        'roberts'    : edgeBand = roberts(tile_data[*,*,j])
        'emboss'     : edgeBand = emboss(tile_data[*,*,j])
        'edge_dog'   : edgeBand = edge_dog(tile_data[*,*,j])
        'shift_diff' : edgeBand = shift_diff(tile_data[*,*,j])
        'laplacian'  : edgeBand = laplacian(tile_data[*,*,j])
      endcase
      
      if (j eq 0) then begin
        ;preallocate an array to hold our edge data
        tile_edge_data = make_array(sub_rect[2] - sub_rect[0] + 1, sub_rect[3] - sub_rect[1] + 1, nbands, TYPE = edgeBand.typecode, /NOZERO)
      endif
      
      tile_edge_data[0,0,j] = edgeBand
    endfor

    ;if first sub rect, get the typecode and initialize our raster
    if (i eq 0) then begin
      typecode = tile_edge_data.typecode
      output_raster = ENVIRaster($
        DATA_TYPE = typecode,$
        NBANDS = nbands,$
        NCOLUMNS = dims[0],$
        NROWS = dims[1],$
        SPATIALREF = input_raster.SPATIALREF,$
        METADATA = meta,$
        URI = output_raster_uri)
    endif
    
    ;check pixel state
    idxOff = where(ps, countOff) 
    if (countOff gt 0) then begin
      tile_data[idxOff] = -1
    endif

    ;get the output index locations
    idx_tile = output_tile_sub_rects[i] 
    
    ;set the data
    sub = tile_edge_data[idx_tile[0]:idx_tile[2],idx_tile[1]:idx_tile[3],0:nbands-1]
    output_raster.SetData, sub, SUB_RECT = output_raster_tile_locations[i]
  endforeach
  
  ;save our output raster
  output_raster.save
end

