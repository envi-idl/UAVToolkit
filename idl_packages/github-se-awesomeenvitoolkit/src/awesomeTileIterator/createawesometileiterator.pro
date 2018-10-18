;+
; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.
;
; :Description:
;    Procedure to create a much better tile iterator which includes a buffer to
;    remove tiling effects from some processing. If you specify that you want a
;    buffer then the OUTPUT_TILE_SUB_RECTS will contain a list of the regions of
;    each tile that represents the actualy data in the raster.
;
;
;
; :Keywords:
;    INPUT_RASTER: in, required, tpye=ENVIRaster
;      Specify the ENVIRaster that you want to create a nice tile iterator for
;    TILE_BUFFER: in, optional, type=long
;      Set this keyword to the buffer that you want to add to each tile to help
;      remove edge effects from processing. This is added to the sides of each tile
;      when added. See `OUTPUT_TILE_SUB_RECTS` for how to get the actual data out of the
;      tiles ignoring the buffer.
;    TILE_SIZE: in, optional, type=longarr, default=[1024,1024]
;      Specify the custom tile size that you want to use when determining the sub rects
;      to split up the INPUT_RASTER. If the raster is smaller than the requested tile
;      size then the sub rects will be snapped to the size of the raster.
;    OUTPUT_SUB_RECTS: out, required, type=list
;      This output  keyword contains the sub rects for the `INPUT_RASTER`.
;    OUTPUT_RASTER_TILE_LOCATIONS: out, required, type=list
;      This outward parameters contains the sub rects in the original raster that the
;      `OUTPUT_TILE_SUB_RECTS` will correspond to. This is added so you can easily map
;      the tiles back to the original raster. This is only needed if you specify the
;      `TILE_BUFFER` keyword.
;    OUTPUT_TILE_MAP: out, optional, type=arr
;      A 2D array that contains the index of each tile in the `OUTPUT_SUB_RECTS` list so that
;      a user can easily understand the spatial content between them all.
;    OUTPUT_TILE_SUB_RECTS: out, required, type=list
;      This keyword contains a list where each element represents the sub rect of each tile that
;      will exclude the buffer. This is used so that you can remove the buffer from each tile and,
;      in conjunction with the `OUTPUT_RASTER_TILE_LOCATIONS`, can be used to place the data where
;      it belongs in an output raster. This output keyword is only needed when `TILE_BUFFER` is set.
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro createAwesomeTileIterator,$
  DEBUG = debug,$
  INPUT_RASTER = input_raster,$
  TILE_BUFFER = tile_buffer, $
  TILE_SIZE = tile_Size,$
  NCOLUMNS = ncolumns,$
  NROWS = nrows,$
  OUTPUT_SUB_RECTS = output_sub_rects,$
  OUTPUT_RASTER_TILE_LOCATIONS = output_raster_tile_locations,$
  OUTPUT_TILE_MAP = output_tile_map,$
  OUTPUT_TILE_SUB_RECTS = output_tile_sub_rects
  compile_opt idl2, hidden
  if ~keyword_set(debug) then on_error, 2

  if keyword_set(input_raster) then begin
    ;get some information from the INPUT_RASTER
    ncolumns = input_raster.NCOLUMNS
    nrows = input_Raster.NROWS
  endif else begin
    if ~keyword_set(ncolumns) then begin
      message, 'NCOLUMNS was not specified when INPUT_RASTER was set, required!'
    endif
    if ~keyword_set(nrows) then begin
      message, 'NROWS was not specified when INPUT_RASTER was set, required!'
    endif
  endelse

  ;default tile buffer
  if ~keyword_set(tile_buffer) then begin
    tile_buffer = 0
  endif

  ;default tile size
  if ~keyword_set(tile_size) then begin
    tile_Size = [1024, 1024]
  endif

  ;determine how many tiles we need
  if (ncolumns le tile_size[0]) then begin
    nx = 1
  endif else begin
    nx = ceil(float(ncolumns)/tile_size[0])
  endelse

  if (nrows le tile_size[1]) then begin
    ny = 1
  endif else begin
    ny = ceil(float(nrows)/tile_size[1])
  endelse

  ;hold a reference to all the output tiles in a map
  output_tile_map = ulonarr(nx, ny)

  ;determine our sub rects without a buffer that correspond to where the real data
  ;in each tile will exist for our output raster
  output_raster_tile_locations = list()
  for j = 0, ny - 1 do begin
    for i = 0, nx - 1 do begin
      left = (i*tile_size[0]) > 0
      right = (((i+1)*tile_size[0]-1) < (ncolumns-1)) > 0
      top = (j*tile_size[1]) > 0
      bottom = (((j+1)*tile_size[1]-1) < (nrows-1)) > 0
      output_raster_tile_locations.add, [left, top, right, bottom]
    endfor
  endfor

  ;determine our sub rects with a buffer
  output_sub_rects = list()
  for j = 0, ny - 1 do begin
    for i = 0, nx - 1 do begin
      left = (i*tile_size[0]-tile_buffer) > 0
      right = (((i+1)*tile_size[0]-1+tile_buffer) < (ncolumns-1)) > 0
      top = (j*tile_size[1]-tile_buffer) > 0
      bottom = (((j+1)*tile_size[1]-1+tile_buffer) < (nrows-1)) > 0
      output_tile_map[i,j] = n_elements(output_sub_rects)
      output_sub_rects.add, [left, top, right, bottom]
    endfor
  endfor

  ; get the regions in our sub_rects that represent real data
  ; this only matters i buffer is set which means that each sub rect we calculated above
  ; actually represents data that is duplicated between each tile to help remove edge effects for
  ; some processing. the
  output_tile_sub_rects = list()
  foreach sub_rect, output_sub_rects, i do begin
    out_loc = output_raster_tile_locations[i]

    case sub_rect[0] of
      0:    minx = 0
      else: minx = tile_buffer
    endcase

    case sub_rect[1] of
      0:    miny = 0
      else: miny = tile_buffer
    endcase

    case sub_rect[2] of
      (ncolumns-1): maxx = sub_rect[2] - sub_rect[0]
      else:         maxx = sub_rect[2] - sub_rect[0] - tile_buffer
    endcase

    case 1 of
      (sub_rect[3] eq (nrows-1)): maxy = sub_rect[3] - sub_rect[1]
      else:      maxy = sub_rect[3] - sub_rect[1] - tile_buffer
    endcase

    ;correct for tile buffers where the buffer is less than the extra space for
    ;the tile. i.e. we have a 1015 by 1015 array with 1000 by 1000 tiles and
    ;a 16 pixel buffer
    if (tile_buffer gt 0) then begin
      diffx = sub_rect[2] - out_loc[2]
      if (diffx gt 0) and (diffx lt tile_buffer) then begin
        maxx -= diffx
      endif

      diffy = sub_rect[3] - out_loc[3]
      if (diffy gt 0) and (diffy lt tile_buffer) then begin
        maxy -= diffy
      endif
    endif

    output_tile_sub_rects.add, [minx, miny, maxx, maxy] > 0
  endforeach
end