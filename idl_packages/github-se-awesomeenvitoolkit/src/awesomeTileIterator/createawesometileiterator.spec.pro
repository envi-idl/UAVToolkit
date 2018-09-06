;create a tester
l = luna(CONFIG_FILE='./../../idl.test.json', /DEBUG)

; start envi headlessly
e = envi(/HEADLESS)

; Open an input file
File = Filepath('qb_boulder_msi', Subdir=['data'], $
  Root_Dir=e.Root_Dir)
Raster = e.OpenRaster(File)

  ;create a suite
  s = l.suite('Test suite that')
  
    ; create a test
    it = s.test('makes sure we can tile over a raster and that our results match expected')
    
      ; add an expectation for our test
      (it.expects('createAwesomeTileIterator')).toRunProcedure,$
        INPUT_RASTER = raster,$
        OUTPUT_SUB_RECTS = sub_rects
      
      ;make sure our results are equal
      (it.expects(sub_rects)).toEqual, list([0, 0, 1023, 1023])

    ; create a test
    it = s.test('makes sure we can tile over a raster with a buffer and that our results match expected')
    
      ; add an expectation for our test
      (it.expects('createAwesomeTileIterator')).toRunProcedure,$
        TILE_SIZE = [512, 512],$
        TILE_BUFFER = 2,$
        INPUT_RASTER = raster,$
        OUTPUT_SUB_RECTS = sub_rects,$
        OUTPUT_RASTER_TILE_LOCATIONS = raster_tile_locations,$
        OUTPUT_TILE_SUB_RECTS = tile_sub_rects

      ; specify what our results should be
      expected_subs =  list([0,0,513,513], [510,0,1023,513], [0,510,513,1023], [510,510,1023,1023])
      expected_locs =  list([0,0,511,511], [511,0,1023,511], [0,511,511,1023], [511,511,1023,1023])
      expected_tile_subs =  list([0,0,511,511], [2,0,513,511], [0,2,511,513], [2,2,513,513])
      
      ;make sure our results are equal
      (it.expects(sub_rects)).toEqual, expected_subs
      (it.expects(raster_tile_locations)).toEqual, expected_locs
      (it.expects(expected_tile_subs)).toEqual, tile_sub_rects

    ; create a test
    it = s.test('makes sure we can tile over dimensions with a buffer and that our results match expected')
    
      ; add an expectation for our test
      (it.expects('createAwesomeTileIterator')).toRunProcedure,$
        TILE_SIZE = [512, 512],$
        TILE_BUFFER = 2,$
        NCOLUMNS = 1024,$
        NROWS = 1024,$
        OUTPUT_SUB_RECTS = sub_rects,$
        OUTPUT_RASTER_TILE_LOCATIONS = raster_tile_locations,$
        OUTPUT_TILE_SUB_RECTS = tile_sub_rects

      ; specify what our results should be
      expected_subs =  list([0,0,513,513], [510,0,1023,513], [0,510,513,1023], [510,510,1023,1023])
      expected_locs =  list([0,0,511,511], [511,0,1023,511], [0,511,511,1023], [511,511,1023,1023])
      expected_tile_subs =  list([0,0,511,511], [2,0,513,511], [0,2,511,513], [2,2,513,513])
      
      ;make sure our results are equal
      (it.expects(sub_rects)).toEqual, expected_subs
      (it.expects(raster_tile_locations)).toEqual, expected_locs
      (it.expects(expected_tile_subs)).toEqual, tile_sub_rects
    
l.generateTestSummary
end
