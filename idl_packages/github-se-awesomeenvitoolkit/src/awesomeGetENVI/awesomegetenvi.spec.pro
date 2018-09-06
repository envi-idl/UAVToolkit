; create our tester object
l = luna(CONFIG_FILE='./../../idl.test.json')

  ;create a suite
  s = l.suite('Test suite that')
  
    ; create a test
    it = s.test('fails to get the current ENVI session because it has not started')
    
      ; check if ENVI is open and close if it is
      e = envi(/CURRENT)
      if (e ne !NULL) then begin
        e.close
      endif
    
      ; add an expectation for our test
      (it.expects('awesomeGetENVI'))._not_.toRunFunction
  
    ; create a test
    it = s.test('gets the current ENVI session')
    
      ; start envi headlessly
      e = envi(/HEADLESS)
    
      ; add an expectation for our test
      (it.expects('awesomeGetENVI')).toRunFunction

    ; create a test
    it = s.test('fails when the UI is not open and the UI is requested')
    
      ; add an expectation for our test
      (it.expects('awesomeGetENVI'))._not_.toRunFunction, /UI
      
    ; create a test
    it = s.test('passes when the UI is open and the UI is requested')
    
      ; close envi and start with UI
      e.close
      e = envi()
    
      ; add an expectation for our test
      (it.expects('awesomeGetENVI')).toRunFunction, /UI
      
      ; clean up
      e.close 
    
l.generateTestSummary
end