; create our tester object
l = luna(CONFIG_FILE='./../../idl.test.json')

; start envi headlessly
e = envi(/HEADLESS)

  ;create a suite
  s = l.suite('Test suite that')
  
    ; create a test
    it = s.test('validates our definition and other routines')
    
      (it.expects('awesomeENVIProgress')).toRunProcedure, /CLEAR_PROGRESS
      (it.expects('awesomeENVIProgress__define')).toRunProcedure

  
    ; create a test
    it = s.test('validates our progress message as an object')
    
      ; create our progress object
      prog = awesomeENVIProgress('My Super Progress Message', /PRINT)
  
      ;creat an expectation to run against
      e = it.expects(prog)
  
      ; make sure it is a valid object
      e.toBeAValidObject
  
      ; send some progress messages
      for i=0, 3 do begin
        ; check for cancellation both ways
        e.toRunProcedureMethod, 'abortRequested'
        e.toRunFunctionMethod, 'abortRequested'
  
        ; send progress messages
        if (i eq 0) then begin
          prog.setProgress,  'Processing...', 100*((i+1)/3), APPEND = 'something', /NO_TAB, /PRINT, /TIME
        endif
        e.toRunProcedureMethod, 'setProgress', 'Processing...', 100*((i+1)/3)
  
        wait, 1
      endfor
  
      ; close progress message
      e.toRunProcedureMethod, 'finish', /PRINT
    
l.generateTestSummary
end
