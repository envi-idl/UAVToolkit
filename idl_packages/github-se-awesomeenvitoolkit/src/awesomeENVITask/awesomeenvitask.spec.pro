; create our tester
l = luna(CONFIG_FILE='./../../idl.test.json')

; start envi headlessly
e = envi(/HEADLESS)

  ;create a suite
  s = l.suite('Test suite that')
  
    ; create a test
    it = s.test('makes sure our function works')
    
      ; add an expectation for our test with existing task
      (it.expects('awesomeENVITask')).toRunFunction, 'ISODataClassification'
    
      ; add an expectation for our test with task on the path
      (it.expects('awesomeENVITask')).toRunFunction, 'classify_raster'
    
l.generateTestSummary
end
