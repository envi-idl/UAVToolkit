;h+
; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.
;h-

;+
; :Description:
;    Procedure used with the BandAlignment tools that will generate tie points
;    for each band of the INPUT_RASTER.
;
;
;
; :Keywords:
;    INPUT_RASTER: in, required, type=ENVIRaster
;      Specify the raster that you want to register the bands to for.
;    TIEPOINT_GENERATION_TASK: in, requried, type=EVNITask
;      Provide the base task that you want to use for generating the tie
;      points for each band. This either uses cross correlation or mutual 
;      information to generate the points. To take advantage of this routine,
;      you should set this task to cross correlation and let the routine
;      switch to the more robust, mutual information algorithm if enough
;      tie points are not generated.
;    TIEPOINT_FILTERING_TASK: in, required, type=ENVITask
;      This parameter must be set to the task that you want for filtering
;      the generated tie points to eliminate false positives.
;    MINIMUM_FILTERED_TIEPOINTS: in, required, type=number
;      Specify the number of tiepoints that you want to be found after filtering. This
;      will switch the algorithm to the mutual information option and try again
;      if enough points are not found the first time through.
;    REFERENCE_BAND: in, optional, type=long, default=0
;      Provide the zero-based index that represents which band will be used as the
;      reference band for your image. It defaults to the first band if not
;      specified.
;    OUTPUT_BANDALIGNMENTTIEPOINTS: out, required, type=BandAlignmentTiePoints
;      This will contain the output tiepoints used for this set. This will not
;      exist on disk until you manually sav ethe tie points.
;    REGISTER_BANDS: in, optional, type=bytarr
;      Specify an array of 1/0 values that indicate whether or not a particular
;      band should be registered to the `REFERENCE_BAND` or not.
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro BandAlignment_GenerateReferenceTiePoints,$
  INPUT_RASTER = input_raster,$
  TIEPOINT_GENERATION_TASK = tiepoint_generation_task,$
  TIEPOINT_FILTERING_TASK = tiepoint_filtering_task,$
  MINIMUM_FILTERED_TIEPOINTS = minimum_filtered_tiepoints,$
  REFERENCE_BAND = reference_band,$
  OUTPUT_BANDALIGNMENTTIEPOINTS = output_BandAlignmentTiePoints,$
  REGISTER_BANDS = register_bands
  compile_opt idl2, hidden
;  on_error, 2
  
  ;catch so that we may clean up correctly
  catch, err
  if (err ne -1) then begin
    ;check if we need to clean up after our base raster
    if isa(base_raster, 'ENVIRASTER') then begin
      uri = [base_raster.URI, base_raster.AUXILIARY_URI]
      base_raster.close
      foreach file, uri do file_delete, uri, /QUIET, /ALLOW_NONEXISTENT
    endif
    ;check if we need to clean up after our band raster
    if isa(band_raster, 'ENVIRASTER') then begin
      uri = [band_raster.URI, band_raster.AUXILIARY_URI]
      band_raster.close
      foreach file, uri do file_delete, uri, /QUIET, /ALLOW_NONEXISTENT
    endif
    catch, /CANCEL
    message, /REISSUE_LAST
  endif
  
  ;start ENVI
  e = envi(/current) 
  if (e eq !NULL) then begin
    e = envi()
  endif
  
  ;get some information on our input raster
  nbands = input_raster.NBANDS
  
  ;check the reference band
  if (reference_band eq !NULL) then reference_band = 0
  
  ;check if we set a threshold for the minimum umber of tiepoints that we want to generate
  if (minimum_filtered_tiepoints eq !NULL) then begin
    minimum_filtered_tiepoints = .1*tiepoint_generation_task.REQUESTED_NUMBER_OF_TIEPOINTS
  endif
  
  ;check if we specify bands that we want to register or not
  ;if not, then register all bands by default
  if ~n_elements(register_bands) then begin
    register_bands = intarr(nbands) + 1
  endif else begin
    
    ;check for too many things passed in
    if (n_elements(register_bands) gt nbands) then begin
      message, 'Invalid keyword value REGISTER_BANDS. More bands passed in than number of bands in INPUT_RASTER.'
    endif
    
    ;check the numbers that are passed in
    if total(register_bands gt (nbands-1)) then begin
      message, 'Invalid band number for keyword REGISTER_BANDS. Requested band (s) for registration exceed number of bands present'
    endif
    
    if total(register_bands lt 0) then begin
      message, 'Invalid band number for keyword REGISTER_BANDS. Requested band (s) for registration are less than zero.'
    endif
    
    temp = intarr(nbands)
    temp[register_bands] = 1
    register_bands = temp
  endelse
  
  ;preallocate some arrays
  filtered_tiepoint_tasks = objarr(nbands)
  
  ;create a virtual raster for our base raster
  base_raster = ENVISubsetraster(input_raster, BANDS = reference_band)
  
  ;generate our tiepoints
  for i=0, nbands-1 do begin
    ;chceck if we need to register our band to the reference band
    if (register_bands[i] eq 0) then continue
    
    ;initialize a flag for error
    was_error = 0
    
    ;skip our reference band
    if (i eq reference_band) then begin
      continue
    endif
    
    ;print some info to the screen
    print, '  ' + 'Generating and filtering tie points between band ' + strtrim(i + 1,2) + ' and ' + strtrim(reference_band + 1,2) + '...'
    
    ;subset our raster and export to disk
    band_raster = ENVISubSetraster(input_raster, BANDS = i)
    
    ;clone task and generate reference tiepoints
    ;first, check if we failed with cross correlation
    if (1 eq 0) then begin
      ;goto statement for if we fail once and are using the cross correlation algorithm for
      ;finding tiepoints
      RUN_TASK:
      ;clone task and generate reference tiepoints
      band_tiepoint_task = ENVITask('GenerateTiePointsByMutualInformation')
      band_tiepoint_task.REQUESTED_NUMBER_OF_TIEPOINTS = tiepoint_generation_task.REQUESTED_NUMBER_OF_TIEPOINTS
    endif else begin
      band_tiepoint_task = ENVITask(tiepoint_generation_task.NAME)
      band_tiepoint_task.MINIMUM_MATCHING_SCORE = tiepoint_generation_task.MINIMUM_MATCHING_SCORE
      band_tiepoint_task.REQUESTED_NUMBER_OF_TIEPOINTS = tiepoint_generation_task.REQUESTED_NUMBER_OF_TIEPOINTS
    endelse
    band_tiepoint_task.INPUT_SEED_TIEPOINTS = tiepoint_generation_task.INPUT_SEED_TIEPOINTS
    band_tiepoint_task.SEARCH_WINDOW = tiepoint_generation_task.SEARCH_WINDOW
    band_tiepoint_task.MATCHING_WINDOW = tiepoint_generation_task.MATCHING_WINDOW
    band_tiepoint_task.INTEREST_OPERATOR = tiepoint_generation_task.INTEREST_OPERATOR
    band_tiepoint_task.INPUT_RASTER1 = base_raster
    band_tiepoint_task.INPUT_RASTER2 = band_raster
    band_tiepoint_task.execute, ERROR = error
    
    ;check for an error
    if (error ne '') then begin
      if error.startswith('There are not enough tie points generated.') then begin
        ;check the name of the tie point generation task to see if we really do need to adjust parameters
        if (strupcase(band_tiepoint_task.NAME) eq strupcase('GenerateTiePointsByMutualInformation')) then begin
          message, 'Not enough tie points generated with mutual information task, adjust parameters accordingly.'
        endif else begin
          print, '    ' + 'Not enough tiepoints generated, checking mutual information algorithm...'
        endelse
        was_error = 1
        GOTO, RUN_TASK
      endif else begin
        message, 'Error when generating tiepoints for band ' + strtrim(i+1,2) + ' to band ' + strtrim(reference_band+1,2) + ':' + $
          string(10b) + '  ' + strjoin(error, string(10b))
      endelse
    endif
    
    ;clone task and filter tiepoints
    band_tiepoint_filter_task = ENVITask(tiepoint_filtering_task.name)
    band_tiepoint_filter_task.INPUT_TIEPOINTS = band_tiepoint_task.OUTPUT_TIEPOINTS
    band_tiepoint_filter_task.Execute, ERROR = error
    
    ;check for an error
    if (error ne '') then begin
      if error.startswith('There are not enough tie points generated.') then begin
        ;check the name of the tie point generation task to see if we really do need to adjust parameters
        if (strupcase(band_tiepoint_task.NAME) eq strupcase('GenerateTiePointsByMutualInformation')) then begin
          message, 'Not enough tie points generated with mutual information task, adjust parameters accordingly.'
        endif else begin
          print, '    ' + 'Not enough tiepoints generated, checking mutual information algorithm...'
        endelse
        was_error = 1
        GOTO, RUN_TASK
      endif else begin
        message, 'Error when filtering tiepoints for band ' + strtrim(i+1,2) + ' to band ' + strtrim(reference_band+1,2) + ':' + $
          string(10b) + '  ' + strjoin(error, string(10b))
      endelse
    endif
    
    ;get the XY file coordinates for our tiepoints
    filtered_points = band_tiepoint_filter_task.OUTPUT_TIEPOINTS.Get()
    
    ;display some information about the tiepoints
    refcol = filtered_points.(0)
    refrow = filtered_points.(1)
    warpcol = filtered_points.(2)
    warprow = filtered_points.(3)
    
    ;get the distance between the points
    diff_ref = ((refcol - warpcol)^2 + (refrow - warprow)^2)^.5
    print, '    ' + 'Number of tie points found             :    [ ' + strtrim(n_elements(refcol),2) + ' ]'
    print, '    ' + 'Mean pixel distance between tie-points :    [ ' +  strtrim(mean(diff_ref),2) + ' ] '
    
    ;get how many points we filtered
    npoints = n_elements(refcol)
    if (npoints lt minimum_filtered_tiepoints) then begin
      ;check the name of the tie point generation task to see if we really do need to adjust parameters
      if (strupcase(band_tiepoint_task.NAME) eq strupcase('GenerateTiePointsByMutualInformation'))  OR (was_error eq 1) then begin
        message, 'Not enough tie points after filtering with mutual information task, adjust parameters accordingly.'
      endif else begin
        print, '    ' + 'Not enough tiepoints left over after filtering, checking mutual information algorithm...'
      endelse
      was_error = 1
      GOTO, RUN_TASK
    endif
    
    ;save our result
    filtered_tiepoint_tasks[i] = band_tiepoint_filter_task
    
    ;clean up our virtual rasters
    band_raster.close
  endfor
  
  ;close our subset band raster and clean up
  base_raster.close
  
  ;populate our output filtered tiepoints
  filtered_tiepoints = ptrarr(nbands)
  
  ;generate our tiepoints
  for i=0, nbands-1 do begin
    filteredTask = filtered_tiepoint_tasks[i]
    ;chceck if we need to register our band to the reference band
    if ~obj_valid(filteredTask) then continue
    tiePoints = filteredTask.OUTPUT_TIEPOINTS.get()
    
    filtered_tiepoints[i] = ptr_new(transpose([[tiePoints.COLUMN1],[tiePoints.ROW1],[tiePoints.COLUMN2],[tiePoints.ROW2]]))
  endfor
  
  ;create object for our tiepoints
  output_BandAlignmentTiePoints = BandAlignmentTiePoints(filtered_tiepoints)
end
