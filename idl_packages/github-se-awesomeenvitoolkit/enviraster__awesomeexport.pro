pro ENVIRaster::AwesomeExport, outFile, DATA_IGNORE_VALUE = data_ignore_value
  compile_opt idl2
  
  ;basic error checking
  if (outFIle eq !NULL) then begin
    message, 'outFile not specified, requried argument!'
  endif
  
  if file_test(outFile) then begin
    message, 'outFile specified, but file exists already on disk!'
  endif
  
  ;check if we need metadata to set up our raster
  if keyword_set(data_ignore_value) then begin
    meta = ENVIRasterMetadata()
    meta['data ignore value'] = data_ignore_value
  endif
  
  ;initialize our output raster
  outRaster = ENVIRaster(INHERITS_FROM = self, METADATA = meta, URI = outFile)
  
  ;get our tiles for iterating
  CreateBetterTileIterator,$
    TILE_SIZE = [2048, 2048],$
    INPUT_RASTER = self,$
    OUTPUT_SUB_RECTS = sub_rects
  
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
;    print, i+1, n_elements(sub_rects)
  endforeach

  ;save the raster
  outRaster.save
end

raster = ENVIHydrate(json_parse('{"factory":"RoiMaskRaster","data_ignore_value":0,"input_raster":{"factory":"RoiMaskRaster","data_ignore_value":0,"input_raster":{"url":"C:\\Users\\znorman\\Desktop\\biggerSubset1.dat","factory":"URLRaster","auxiliary_url":["C:\\Users\\znorman\\Desktop\\biggerSubset1.hdr","C:\\Users\\znorman\\Desktop\\biggerSubset1.dat.enp"]},"input_rois":[{"url":"C:\\Temp\\envitempfileWedAug301142202017524170620.xml","factory":"URLRoi","dataset_index":0}]},"input_rois":[{"url":"C:\\Temp\\envitempfileWedAug3011422020171754421512.xml","factory":"URLRoi","dataset_index":0}],"inverse":1}'))
tic
raster.AwesomeExport, e.gettemporaryfilename(), DATA_IGNORE_VALUE = 0
toc
tic
raster.export,  e.gettemporaryfilename(), 'ENVI', DATA_IGNORE_VALUE = 0
toc
end