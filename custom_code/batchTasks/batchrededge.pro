; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.

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
;        ;main level program - make sure that your path has been updated with the UAV Toolkit
;        ;this folder should be the one that contains the unzipped contents of the sample data.
;        flightdir = 'C:\data\rededge'
;    
;        ;create our batch task
;        rededgeTask = ENVITask('UAVBatchRedEdge')
;        rededgeTask.FLIGHTDIR = flightdir
;        rededgeTask.BAND_ALIGNMENT_TASK = band_alignment_task
;        rededgeTask.execute
;  
;    Here is an example for how you can use this routine directly. Processing with a task is preferable:
;    
;        ;start ENVI headlessly
;        e = envi(/HEADLESS)
;    
;        ;initialize the uav_toolkit
;        uav_toolkit
;    
;        ;specify our flight folder (contains 000)
;        flightdir = 'C:\data\rededge'
;    
;        ;initialize our task
;        band_alignment_task = ENVITask('UAVBandAlignment')
;        band_alignment_task.APPLY_REFERENCE_TIEPOINTS = 1
;    
;        ;generate the reference tiepoints
;        batchRedEdge, FLIGHTDIR = flightdir, BAND_ALIGNMENT_TASK = band_alignment_task
;
;
; :Keywords:
;    FLIGHTDIR: in, required, type=string
;      The directory that contains folders of RedEdge data.
;    BAND_ALIGNMENT_TASK: in, optional, type=EVNITask
;      Custom task definition for processing the rededge data. A default
;      task is used if not specified.
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro batchRedEdge, FLIGHTDIR = flightdir, BAND_ALIGNMENT_TASK = band_alignment_task
  compile_opt idl2

  ;start ENVI
  e = envi(/current)
  if (e eq !NULL) then begin
    message, 'ENVI has not started yet, required!'
  endif
  
  ;restore the uav_toolkit
  uav_toolkit

  ;make sure flightdir was specified
  if ~keyword_set(flightdir) then begin
    message, 'FLIGHTDIR keyword not specified, required!'
  endif

  ;make sure our directory exists
  if ~file_test(flightdir, /DIRECTORY) then begin
    message, 'FLIGHTDIR specified, but is not a real directory!'
  endif

  ;search the FLIGHTDIR for other directories or files
  cd, flightdir, current = first_dir
  found = file_search(COUNT = count)
  cd, first_dir

  ;make sure we found files or folders in FLIGHTDIR
  if (count eq 0) then begin
    message, 'No folders or files found in FLIGHTDIR!'
  endif

  ;create fully-qualified path
  found = flightdir + path_sep() + found

  ;check which elements of the array "found" are actually directories
  ;and return the index locations of where those directories are in the
  ;array "found"
  idx_dirs = where(file_test(found, /DIRECTORY), count_dirs)

  ;make sure we have found directories
  if (count_dirs eq 0) then begin
    message, 'No directories found in FLIGHTDIR, required!'
  endif

  ;subset our found array by the what is actually a directory
  dirs = found[idx_dirs]
  dirs_lc = file_basename(strlowcase(dirs))

  ;find the directories that don't contain
  ; - "_out"
  ; - "rigorous"
  ; - "_temp"

  idx_datadirs = where(~dirs_lc.endsWith('_out') AND ~dirs_lc.endsWith('_temp') AND $
    ~dirs_lc.endsWith('rigorous'), count_data)

  ;check that we have potential directories of data
  if (count_data eq 0) then begin
    message, 'No folders found in FLIGHTDIR passed the filetering step. All folders contain either' + $
      '"_out", "_temp", or "rigorous".'
  endif

  ;subset our directories by what likely contains data
  datadirs = dirs[idx_datadirs]

  ;create a list to remember the directories we want to keep
  keepdirs = list()

  ;check to make sure each directory contains image groups, otherwise we skip the
  ;directory since we don't want to use the data within
  print, 'Finding image groups...'
  foreach dir, datadirs, idx do begin
    print, string(9b) + 'Processing directory ' + strtrim(idx + 1,2) + ' of ' + strtrim(count_data,2)

    ;check to make sure we have image groups in our directory
    groups = bandalignment_get_image_groups(dir, ['_1.tif', '_2.tif', '_3.tif', '_5.tif', '_4.tif'])

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
    print, 'Found multiple directories, joining them together...'
    join_rededge_datadirs, datadirs
  endif
  
  ;check if we specified a task or not, otherwise make a new one from scratch
  if (band_alignment_task eq !NULL) then begin
    ;create task
    band_alignment_task = ENVITask('UAVBandAlignment')
    
    ;set our other parameters for "batch processing"
    band_alignment_task.GENERATE_REFERENCE_TIEPOINTS = 1
    band_alignment_task.APPLY_REFERENCE_TIEPOINTS = 1
  endif
  
  ;make sure that our sensor is correct
  band_alignment_task.SENSOR = 'rededge'
  band_alignment_task.INPUTDIR = datadirs[0]
  
  ;run task
  band_alignment_task.execute

;  ;code for simple debugging that will stop on errors
;  bandalignmenttask, $
;    SENSOR = 'rededge',$
;    INPUTDIR = datadirs[0],$
;    GENERATE_REFERENCE_TIEPOINTS = 1,$
;    APPLY_REFERENCE_TIEPOINTS = 1
  
  ;unjoin the micasense data to delete the copies of files we made in the first
  ;numbered directory
  if (n_elements(datadirs) gt 1) then begin
    unjoin_rededge_datadir, datadirs[0]
  endif
end