;h+
; Copyright (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT. See LICENSE.txt for additional details and information.
;h-

;+
; :Private:  
;
;-

;+
;  Simple routine that sets task parameters from the input parameters. Just convenience
;  in case we need to set parameters in multiple places.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro bandalignment_set_rededge_task_parameters, task, parameters, group
  compile_opt idl2, hidden
  
  ;check if we want a dynamic search window
  if parameters.SEARCH_WINDOW_FROM_HEIGHT then begin
    ;extract image properties
    oImageInfo = obj_new('image_info', group[0], /PRINT_GPS)
    height = oImageInfo.Get('HEIGHT_ABOVE_GROUND')

    ;update the task
    task.SEARCH_WINDOW = 2*(((height>0)/3 < 100) + 5)
  endif else begin
    ;update the task
    task.SEARCH_WINDOW = parameters.SEARCH_WINDOW
  endelse
  
  ;set matching window
  task.MATCHING_WINDOW = (task.SEARCH_WINDOW/2) > 21
end



pro bandalignment_set_task_parameters, parameters
  compile_opt idl2, hidden
  
  ;set cross correlation parameters
  parameters.CORRELATION_TASK.MINIMUM_MATCHING_SCORE = parameters.CORRELATION_MATCHING_SCORE
  parameters.CORRELATION_TASK.REQUESTED_NUMBER_OF_TIEPOINTS = parameters.REQUESTED_NUMBER_OF_TIEPOINTS
  parameters.CORRELATION_TASK.SEARCH_WINDOW = parameters.CORRELATION_SEARCH_WINDOW
  parameters.CORRELATION_TASK.MATCHING_WINDOW = (parameters.CORRELATION_TASK.SEARCH_WINDOW/2) > 21

  ;set mutual information parameters
  parameters.MUTUAL_TASK.MINIMUM_MATCHING_SCORE = parameters.MUTUAL_MATCHING_SCORE
  parameters.MUTUAL_TASK.REQUESTED_NUMBER_OF_TIEPOINTS = parameters.REQUESTED_NUMBER_OF_TIEPOINTS
  parameters.MUTUAL_TASK.SEARCH_WINDOW = parameters.MUTUAL_SEARCH_WINDOW
  parameters.MUTUAL_TASK.MATCHING_WINDOW = (parameters.MUTUAL_TASK.SEARCH_WINDOW/2) > 21

end
