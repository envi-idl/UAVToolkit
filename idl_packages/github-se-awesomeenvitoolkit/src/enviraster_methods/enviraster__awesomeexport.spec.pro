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
    it = s.test('makes sure we can export a raster')
    
      ; add an expectation for our test
      (it.expects(raster)).toRunProcedureMethod, 'awesomeexport', e.GetTemporaryFilename(), DATA_IGNORE_VALUE = 0
    
l.generateTestSummary
end
