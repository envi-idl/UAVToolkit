;h+
; (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT, see LICENSE.txt for more details.
;h-

;+
;
; :Description:
;    Procedure that will process a folder containing directories of RedEdge data in an
;    automated fashion. It joins directories that contain rededge data, generates,
;    and applies reference tie points for the user. Note that the directories of data
;    should all be with the same sensor and have the same height above the ground
;    for the tie points to be valid.
;
;  :Examples:
;  
;    Here is an example of how you can run this routine with the task UAVBatchRedEdge:
;    
;        ;start ENVI headlessly
;        e = envi(/HEADLESS)
;    
;        ;initialize the uav_toolkit
;        uav_toolkit
;    
;        ;specify our flight folder (contains data folder '000')
;        flightdir = 'C:\data\rededge'
;    
;        ;create our batch task
;        redEdgeTask = ENVITask('UAVBatchRedEdge')
;        redEdgeTask.FLIGHTDIR = flightdir
;        redEdgeTask.execute
;  
;    Here is an example for how you can use this routine directly. Processing with the task is preferable:
;    
;        ;start ENVI headlessly
;        e = envi(/HEADLESS)
;    
;        ;initialize the uav_toolkit
;        uav_toolkit
;    
;        ;specify our flight folder (contains data folder '000')
;        flightdir = 'C:\data\rededge'
;    
;        ;initialize our task
;        alignTask = ENVITask('UAVBandAlignment')
;        alignTask.APPLY_REFERENCE_TIEPOINTS = 1
;    
;        ;generate the reference tiepoints
;        batchRedEdge, FLIGHTDIR = flightdir, BAND_ALIGNMENT_TASK = alignTask
;        
;    Here is an example of how you can use this routine and specify the percent reflectance of each band with
;    the reflectance panels:
;
;        ;start ENVI headlessly
;        e = envi(/HEADLESS)
;
;        ;initialize the uav_toolkit
;        uav_toolkit
;
;        ;specify our flight folder (contains data folder '000')
;        flightdir = 'C:\data\rededge'
;
;        ;create our batch task
;        redEdgeTask = ENVITask('UAVBatchRedEdge')
;        redEdgeTask.FLIGHTDIR = flightdir
;        redEdgeTask.PANEL_REFLECTANCE = [67, 69, 68, 67, 61]
;        redEdgeTask.execute
;
;
; :Keywords:
;    BAND_ALIGNMENT_TASK: in, optional, type=ENVITask
;      Custom task definition for processing the rededge data. A default task is used
;      if none if provided. This allows for customizing any task parameters through a
;      simple interface.
;    FLIGHTDIR: in, required, type=string 
;      The directory that contains folders of RedEdge data.
;      task is used if not specified.
;    PANEL_REFLECTANCE: in, optional, type=float, default=70.0
;      This represents the percent reflectance (0 to 100) of reflectance panel images
;      that are provided in `PANELDIR`. Specify a single value or an array of values
;      that represents the percent reflectance of each band of the reflectance panel.
;      The value should be between 0 and 100. If a scalar is provided, then it is
;      assumed to be a constant value for each band. The order of this array should
;      match the `FILE_IDENTIFIERS`. If you are using Sequioa or RedEdge data, then
;      it is from shortest to longest wavelength.
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro uavBatchRedEdge, FLIGHTDIR = flightdir, BAND_ALIGNMENT_TASK = band_alignment_task, PANEL_REFLECTANCE = panel_reflectance
  compile_opt idl2
  on_error, 2

  ;start ENVI
  e = awesomeGetENVI()
  
  ;restore the uav_toolkit
  uav_toolkit

  ;make sure flightdir was specified
  if ~keyword_set(flightdir) then begin
    message, 'FLIGHTDIR keyword not specified, required!', LEVEL = -1
  endif

  ;make sure our directory exists
  if ~file_test(flightdir, /DIRECTORY) then begin
    message, 'FLIGHTDIR specified, but does not exist!', LEVEL = -1
  endif

  ;search the FLIGHTDIR for other directories or files
  cd, flightdir, current = first_dir
  found = file_search(COUNT = count)
  cd, first_dir

  ;make sure we found files or folders in FLIGHTDIR
  if (count eq 0) then begin
    message, 'No folders or files found in FLIGHTDIR!', LEVEL = -1
  endif

  ;create fully-qualified path
  found = flightdir + path_sep() + found

  ;check which elements of the array "found" are actually directories
  ;and return the index locations of where those directories are in the
  ;array "found"
  idx_dirs = where(file_test(found, /DIRECTORY), count_dirs)

  ;make sure we have found directories
  if (count_dirs eq 0) then begin
    message, 'No directories found in FLIGHTDIR, required!', LEVEL = -1
  endif

  ;subset our found array by the what is actually a directory
  dirs = found[idx_dirs]
  dirs_lc = file_basename(strlowcase(dirs))

  ;find the directories that don't contain
  ; - "_out"
  ; - "rigorous"
  ; - "_temp"

  idx_datadirs = where(~dirs_lc.endsWith('_out') AND ~dirs_lc.endsWith('_temp') AND $
    ~dirs_lc.endsWith('rigorous') AND ~dirs_lc.endsWith('reflectance_panels'), count_data)

  ;check that we have potential directories of data
  if (count_data eq 0) then begin
    message, 'No folders found in FLIGHTDIR passed the filetering step. All folders contain either' + $
      '"_out", "_temp", or "rigorous".', LEVEL = -1
  endif

  ;subset our directories by what likely contains data
  datadirs = dirs[idx_datadirs]

  ;create a list to remember the directories we want to keep
  keepdirs = list()

  ;check to make sure each directory contains image groups, otherwise we skip the
  ;directory since we don't want to use the data within
  prog = awesomeENVIProgress('Finding RedEdge Data')
  prog.setProgress, 'Finding data, please wait', 0, /PRINT
  foreach dir, datadirs, idx do begin
    print, string(9b) + 'Processing directory ' + strtrim(idx + 1,2) + ' of ' + strtrim(count_data,2)

    ;check to make sure we have image groups in our directory
    groups = bandalignment_get_image_groups(dir, ['_1.tif', '_2.tif', '_3.tif', '_5.tif', '_4.tif'])
    
    ;check if we canceled
    if prog.abortRequested() then begin
      message, 'Process stopped by user', LEVEL = -1
    endif
    
    ;skip directory if there are no TIF files
    if (n_elements(groups) gt 0) then keepdirs.add, dir
  endforeach

  ;check to make sure we found directories to work with
  if (n_elements(keepdirs) eq 0) then begin
    message, 'No directories with RedEdge data found in FLIGHTDIR!'
  endif

  ;replace datadirs with our directories
  datadirs = (keepdirs.toArray()).sort()

  ;joing the directories if we have more than one dir
  if (n_elements(datadirs) gt 1) then begin
    prog.setProgress, 'Joining directories, please wait', 0, /PRINT
    join_rededge_datadirs, datadirs
  endif
  
  ;check if we canceled
  if prog.abortRequested() then begin
    message, 'Process stopped by user', LEVEL = -1
  endif
  
  ;close progress
  prog.finish
  
  ;check if we specified a task or not, otherwise make a new one from scratch
  if (band_alignment_task eq !NULL) then begin
    ;create task
    band_alignment_task = ENVITask('UAVBandAlignment')
    
    ;set our other parameters for "batch processing"
    band_alignment_task.GENERATE_REFERENCE_TIEPOINTS = 1
    band_alignment_task.APPLY_REFERENCE_TIEPOINTS = 1
    band_alignment_task.RIGOROUS_ALIGNMENT = 1
  endif
  
  ;set the panel reflectance
  band_alignment_task.PANEL_REFLECTANCE = panel_reflectance
  
  ;make sure that our sensor is correct
  band_alignment_task.SENSOR = 'rededge'
  band_alignment_task.INPUTDIR = datadirs[0]
  
  ;run task
  band_alignment_task.execute

  ;unjoin the micasense data to delete the copies of files we made in the first
  ;numbered directory
  if (n_elements(datadirs) gt 1) then begin
    unjoin_rededge_datadir, datadirs[0]
  endif
end
