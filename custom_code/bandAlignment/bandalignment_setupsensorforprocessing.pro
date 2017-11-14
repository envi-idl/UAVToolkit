;h+
; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.
;h-

;+
; :Description:
;    Main orchestrator for processing data for band-band alignment. This will
;    set up tasks, task parameters, and call the right routines for generating
;    and applying reference tie points.
;    
;    There is a case statement at the beginning of the procedure that contians the
;    defaults for each sensor type.
;
;
;
; :Keywords:
;    PARAMETERS: in, required, type=dictionary
;      Pass in the parameters needed for image-iamge registration. A 
;      dictionary is used for simplicity and convenience to be able to
;      easily add new parameters for use in different routines.
;
; :Author: Zachary Norman - GitLab: znorman-harris
;-
pro BandAlignment_SetUpSensorForProcessing, PARAMETERS = parameters
  compile_opt idl2, hidden

  ;get current ENVI
  e = envi(/CURRENT)

  ;search for image groups
  groups = bandalignment_get_image_groups(parameters.INPUTDIR, parameters.FILE_IDENTIFIERS)
  if (n_elements(groups) eq 0) then begin
    message, 'No image groups found for processing in directory "' + parameters.INPUTDIR + '"'
  endif

  ;initialize our tasks for image-image registration
  ;generic
  correlationTask = ENVITask('GenerateTiePointsByCrossCorrelation')
  correlationTask.MINIMUM_MATCHING_SCORE = parameters.CORRELATION_MATCHING_SCORE
  correlationTask.REQUESTED_NUMBER_OF_TIEPOINTS = parameters.REQUESTED_NUMBER_OF_TIEPOINTS
  parameters['CORRELATION_TASK'] = correlationTask

  ;rigorous
  mutualTask = ENVITask('GenerateTiePointsByMutualInformation')
  mutualTask.MINIMUM_MATCHING_SCORE = parameters.MUTUAL_MATCHING_SCORE
  mutualTask.REQUESTED_NUMBER_OF_TIEPOINTS = parameters.REQUESTED_NUMBER_OF_TIEPOINTS
  parameters['MUTUAL_TASK'] = mutualTask

  ;update overall task parameters based on our input parameters
  bandalignment_set_task_parameters, parameters

  ;tie point filtering task
  filterTask = ENVITask('FilterTiePointsByGlobalTransform')
  parameters['FILTER_TASK'] = filterTask

  ;check what we need to do for our processing
  if keyword_set(parameters.GENERATE_REFERENCE_TIEPOINTS) then begin
    print, 'Finding ideal image group for band-band alignment...'

    ;lets try and find a good image group to use for generating reference tie points
    bandalignment_find_good_image_group, $
      GROUPS = groups, $
      OUTPUT_IMAGE_GROUP = reference_image_group,$
      KAPPA_FILTER = parameters.KAPPA_FILTER,$
      BASEBAND = parameters.BASE_BAND

    ;because we have a good group, write to folder called rigorous
    rigDir = file_dirname(parameters.INPUTDIR) + path_sep() + 'rigorous'
    parameters['RIGOROUS_DIR'] = rigDir
    if file_test(rigDir) then begin
      file_delete, rigDir, /RECURSIVE, /QUIET
      wait, 0.01 ;permissions issues sometimes if we dont wait, not sure why
    endif

    ;copy new files to the right directory so that users can see
    ;which images are picked
    if ~file_test(rigDir) then file_mkdir, rigDir
    file_copy, groups[reference_image_group], rigDir, /OVERWRITE

    ;generate the reference tie points
    BandAlignment_ProcessSensor_GetReferenceTiePoints,$
      groups[reference_image_group], parameters
  endif


  ;check if we want to apply our reference tiepoints
  if keyword_set(parameters.APPLY_REFERENCE_TIEPOINTS) then begin
    ;make sure that we have reference tie points to use
    if ~file_test(parameters.POINTS_FILE) then begin
      message, 'Requested to apply reference tiepoints, but no reference tie points found! Please generate first.'
    endif

    ;check if we need to perform localized histogram color balancing
    if keyword_set(parameters.CO_CALIBRATION) then begin
      ;check if we have reference information about reflectance panels
      ;if so, get the  means and camera exposure settings
      if file_test(parameters.PANELDIR, /DIRECTORY) then begin
        ;search for image group sin our panel directory
        panelGroups = bandalignment_get_image_groups(parameters.PANELDIR, parameters.FILE_IDENTIFIERS)

        ;only process if we find images
        if (n_elements(panelGroups) gt 0) then begin
          ;get the keys for our groups
          keys = panelGroups.keys()
          
          ;check if we have max valules to set
          maxFlag = parameters.hasKey('MAX_PIXEL_VALUE') AND parameters.hasKey('MAX_VALUE_DIVISOR')
          
          ;attempt to extract the reflectance panels
          panel_info = get_reflectance_panels(panelGroups[keys[0]], parameters.SENSOR,$
            MAX_PIXEL_VALUE = maxFlag ? parameters.MAX_PIXEL_VALUE : !NULL,$
            MAX_VALUE_DIVISOR = maxFlag ? parameters.MAX_VALUE_DIVISOR : !NULL,$
            PANEL_REFLECTANCE = parameters.PANEL_REFLECTANCE)
          
          ;save the panel information
          save, panel_info, FILENAME = parameters.PANELDIR + path_sep() + 'panel_info.sav'
        endif
      endif else begin
        panel_info = dictionary()
      endelse

      ;extract scale factors for each group
      get_co_calibration, $
        GROUPS = groups,$
        PANEL_INFO = panel_info
    endif

    ;apply our reference tiepoints to our image groups
    BandAlignment_ApplyReferenceTiePointsToGroups, groups, parameters
  endif
end
