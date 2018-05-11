;h+
; (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT, see LICENSE.txt for more details.
;h-

;+
; :Private:  
;
;-

;+
; :Description:
;    Simple wrapper procedure that validates and saves our tie points to an
;    IDL save file for later use. The tie points that are saved represent
;    an array of pointers that, if valid pointers, contain the file coordinates
;    of two rasters for image registration. 
;    
;    To use the tie points later you will need to do the following:
;    
;        file = 'C:\someeDir\points.sav'
;        tiePoints = BandAlignmentTiePoints(SAVE_FILE = file)
;    
;
;
; :Keywords:
;    INPUT_BANDALIGNMENTTIEPOINTS: in, required, type=BandAlignmentTiePoints
;      Specify the BandAlignmentTiePoints object that contains the file coordinates for 
;      our ENVI tie points.
;    OUTPUT_TIEPOINTS_SAVEFILE_URI: in, optional, type=string
;      Specify the fully-qualified path to an IDL SAVE file that will contain the 
;      tie points. If not specified, then this keyword will be populated with 
;      a filename that was picked by ENVI.
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro BandAlignment_SaveReferenceTiepoints,$
  INPUT_BANDALIGNMENTTIEPOINTS = input_BandAlignmentTiePoints,$
  OUTPUT_TIEPOINTS_SAVEFILE_URI = output_tiepoints_savefile_uri
  compile_opt idl2, hidden

  ;get current ENVI
  e = envi(/CURRENT)

  ;make sure that our outputs are specified
  if ~keyword_set(output_tiepoints_savefile_uri) then begin
    output_tiepoints_savefile_uri = e.getTemporaryFilename('sav')
  endif

  ;make sure we have valid tiepoints
  if ~isa(input_BandAlignmentTiePoints, 'BandAlignmentTiePoints') then begin
    message, 'input_tiepoints are not a valid BandAlignmentTiePoints object, required!'
  endif
  
  ;duplicate variable name for consistent naming
  bandAlignmentTiePointsObject = input_BandAlignmentTiePoints

  ;save to disk
  save, bandAlignmentTiePointsObject, FILENAME = output_tiepoints_savefile_uri
end
