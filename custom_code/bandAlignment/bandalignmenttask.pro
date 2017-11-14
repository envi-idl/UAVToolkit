;h+
; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.
;h-

;+
; :Description:
;    Main procedure for running all band-band alignment processes for any data types. See the
;    main readme.md file for details on how to use this routine wrapped as a task. All
;    parameters are also described below so that you can use as a procedure if you want, but
;    intended use is as an ENVI task.
;
;
;
; :Keywords:
;    APPLY_REFERENCE_TIEPOINTS: in, optional, type=boolean, default=false
;      Set to true if you have reference tie points and want to apply them to all of
;      your image groups.
;    APPLY_KAPPA_FILTER: in, optional, type=boolean, default=false
;      If set to true then, when finding reference tie points, the choices for optimal 
;      group will be passed through a simple threshold to make sure that we pick an 
;      image during a straight flight line for generating reference tie points. Only used 
;      if `APPLY_REFERENCE_TIEPOINTS` is set
;    BASE_BAND: in, optional, type=long, default=0
;      Specify the zero-based index for which band you want to use as a reference for
;      generating reference tie points.
;    CO_CALIBRATION: in, optional, type=boolean, default=false
;      When specified, the images will be scaled to one another to account for
;      changes in the ISO and exposure time during a flight. If you want to use
;      reflectance panels then this must be set.
;    CORRELATION_MATCHING_SCORE: in, optional, type=float, default=0.6
;      Set this to the score required by the task GenerateTiePointsByCrossCorrelation to make
;      sure that two points are actual the same information.
;    CORRELATION_SEARCH_WINDOW: in, optional, type=long, default=255
;      Set this to the search window used by the task GenerateTiePointsByCrossCorrelation 
;      to find matching tie points.
;    FILE_IDENTIFIERS: in, optional, type=stringarr
;      Specify the end of each file name that delineates how files belong to a group or not.
;    GENERATE_REFERENCE_TIEPOINTS: in, optional, type=boolean, default=false
;      If set, then reference tie points will be found based on the sensor and `INPUTDIR`.
;    GET_GPS: in, optional, type=boolean, default=true
;      Set to true for only generating a GPS file. Created by default afte the processing
;      has finished.
;    INPUTDIR: in, required, type=string
;      Specify the directory that you want to search for image groups to process.
;    MAKE_ENVI_FILE: in, optional, type=boolean, default=false
;      Turn to true to make a .dat and .hdr file that can be ingested in ENVI. Will likely have a 
;      dummy spatial reference set up.
;    MAX_PIXEL_VALUE: in, optional, type=number
;      Specify the maximum pixel value to be allowed when creating output images. Used in
;      conjunction with `MAX_VALUE_DIVISOR`
;    MAX_VALUE_DIVISOR: in, optional, type=number
;      If `MAX_PIXEL_VALUE` is set, then this represents the scale factor that the
;      data will be divided by if over the maximum. This is used for the RedEdge data
;      to make sure that it is truly 12 bit as it was collected (instead of scaled by
;      a factor of 16).
;    MINIMUM_FILTERED_TIEPOINTS: in, optional, type=long, default=REQUESTED_NUMBER_OF_TIEPOINTS/10
;      Select a threshold for the smallest number of allowed tie points that can be
;      found after filtering. If less than this, then the tie point generation algorithm is ugraded
;      to mutual information if not already set as that.
;    MULTI_CHANNEL: in, optional, tpye=boolean, default=false
;      If set to true, then multi-channel TIFFs will be generated. otherwise multi-page
;      TIFFs will be used.
;    MUTUAL_MATCHING_SCORE: in, optional, type=float, default=0.05
;      Represents the required matching score used by GenerateTiePointsbyMutualInformation.
;    MUTUAL_SEARCH_WINDOW: in, optional, type=long, default=121
;      Represents the search window used by GenerateTiePointsbyMutualInformation.
;    NO_SECONDARY_MATCH: in, optional, type=boolean, default=true
;      When false, image registration is performed a second time before writing data to
;      disk for batch processing. Will add a lot of overhead to the total time.
;    NP_GROUPS: in, optional, type=long, default=nCPU/2, min=1, max=4
;      Set to the number of child processes that you want to process in parallel. Max is 4
;      due to licensing.
;    PANEL_REFLECTANCE: in, optional, type=float, default=70.0
;      This represents the percent reflectance (0 to 100) of reflectance panel images 
;      that are provided in `PANELDIR`. Specify a single value or an array of values 
;      that represents the percent reflectance of each band of the reflectance panel. 
;      The value should be between 0 and 100. If a scalar is provided, then it is 
;      assumed to be a constant value for each band. The order of this array should 
;      match the `FILE_IDENTIFIERS`. If you are using Sequioa or RedEdge data, then 
;      it is from shortest to longest wavelength.
;    PANELDIR: in, optional, default=INPUTDIR/reflectance_panels
;      Specify the directory that contains reflectance panels. The default value is
;      INPUTDIR/reflectance_panels.
;    REFLECTANCE_SCALE_FACTOR: in, optional, type=number, default=50000
;      Set this to the scale factor that will be applied to the output imagery so that we 
;      don't have to use floating point data for reflectance imagery.
;    REQUESTED_NUMBER_OF_TIEPOINTS: in, optional, type=long, default=1500
;      Specify the number of tie points that you want to initially find.
;    SEARCH_WINDOW_FROM_HEIGHT: in, optional, type=boolean, default=false
;      If true, then the search window for the different algorithms is based on the
;      approximate height above the ground for the reference group.
;    SENSOR: in, required, type=string, choices='rededge','sequoia','generic'
;      The type of sensor that will be processed. Default is generic unless otherwise specified.
;
; :Author: Zachary Norman - GitHub: znorman-harris
;-
pro BandAlignmentTask, $
  APPLY_REFERENCE_TIEPOINTS = apply_reference_tiepoints,$
  APPLY_KAPPA_FILTER = apply_kappa_filter,$
  BASE_BAND = base_band,$
  CO_CALIBRATION = co_calibration,$
  CORRELATION_SEARCH_WINDOW = correlation_search_window,$
  CORRELATION_MATCHING_SCORE = correlation_matching_score,$
  GENERATE_REFERENCE_TIEPOINTS = generate_reference_tiepoints,$
  FILE_IDENTIFIERS = file_identifiers,$
  GET_GPS = get_gps,$
  ;HISTOGRAM_COLOR_BALANCING = histogram_color_balancing,$
  INPUTDIR = inputdir, $
  MAKE_ENVI_FILE = make_envi_file,$
  MAX_PIXEL_VALUE = max_value,$
  MAX_VALUE_DIVISOR = max_value_divisor,$
  MINIMUM_FILTERED_TIEPOINTS = minimum_filtered_tiepoints,$
  MULTI_CHANNEL = multi_channel,$
  MUTUAL_MATCHING_SCORE = mutual_matching_score,$
  MUTUAL_SEARCH_WINDOW = mutual_search_window,$
  NO_SECONDARY_MATCH = no_secondary_match,$
  NP_GROUPS = np_groups,$
  PANELDIR = paneldir,$
  PANEL_REFLECTANCE = panel_reflectance,$
  REFLECTANCE_SCALE_FACTOR = reflectance_scale_factor,$
  REQUESTED_NUMBER_OF_TIEPOINTS = requested_number_of_tiepoints,$
  SEARCH_WINDOW_FROM_HEIGHT = search_window_from_height,$
  SENSOR = sensor
  compile_opt idl2, hidden

  catch, err
  if (err ne 0) then begin
    catch, /CANCEL
    help, /LAST_MESSAGE, OUTPUT = o
    print, o
    message, /REISSUE_LAST
  endif

  ;get current session of ENVI
  e = envi(/CURRENT)
  if (e eq !NULL) then begin
    message, 'ENVI has not started yet, required.'
  endif

  ;init dictionary for out parameters
  parameters = dictionary()

  ;find out where we are located
  thisFile = routine_filepath()
  parameters['THIS_FILE'] = thisFile
  thisDir = file_dirname(thisFile)
  parameters['THIS_DIR'] = thisDir

  ;do some simple error catching
  if ~file_test(inputdir, /DIRECTORY) then begin
    message, 'Specified input directory does not exist: "' + inputdir + '"!'
  endif
  
  ;check if our directory is marked or not
  if inputDir.endsWith(path_sep()) then begin
    inputdir = strmid(inputdir, 0, strlen(inputdir)-1)
  endif

  ;==============================================================================================
  ;set up directories for input/output files
  parameters['INPUTDIR'] = inputdir
  parameters['PANEL_REFLECTANCE'] = panel_reflectance
  if (reflectance_scale_factor eq !NULL) then reflectance_scale_factor = 65535us
  parameters['REFLECTANCE_SCALE_FACTOR'] = reflectance_scale_factor
  parameters.REFLECTANCE_TYPE_CODE = parameters.REFLECTANCE_SCALE_FACTOR.typecode
  if (file_identifiers ne !NULL) then parameters['FILE_IDENTIFIERS'] = file_identifiers
  
  if (paneldir eq !NULL) then paneldir = parameters.INPUTDIR + path_sep() + 'reflectance_panels'
  parameters['PANELDIR'] = paneldir

  ;where the band stacked images will be created
  stackeddir = parameters.INPUTDIR + '_out'
  if ~file_test(stackeddir) then file_mkdir, stackeddir
  parameters['OUTPUT_DIR'] = stackeddir

  ;where all temporary files will go
  tempdir = parameters.INPUTDIR + '_temp'
  if ~file_test(tempdir) then file_mkdir, tempdir
  parameters['TEMP_DIR'] = tempdir

  ;where we save the tie_points to (make directory relative to inputdir)
  pointsFile = file_dirname(parameters.INPUTDIR) + path_sep() + 'reference_tie_points.sav'
  parameters['POINTS_FILE'] = pointsFile
  
  ;type of sensor
  if ~keyword_set(sensor) then sensor = 'generic' else sensor = strtrim(strlowcase(sensor),2)
  parameters['SENSOR'] = sensor

  ;==============================================================================================
  ;processing parameters
  case 1 of
    (np_groups eq !NULL): np_groups = !CPU.HW_NCPU/2
    (np_groups lt 1): np_groups = !CPU.HW_NCPU/2
    else:;do nothing
  endcase
  np_groups <= 4
  ;check to see if we are licensed for IDL or not, if not we cannot use the IDL_IDLBridge
  if ~lmgr('idl') then np_groups = 1
  parameters['NP_GROUPS'] = np_groups
  if (clean_temp eq !NULL) then clean_temp = 1
  parameters['CLEAN_TEMP'] = clean_temp
  if keyword_set(generate_reference_tiepoints) then begin
    parameters['GENERATE_REFERENCE_TIEPOINTS'] = 1
  endif else begin
    parameters['GENERATE_REFERENCE_TIEPOINTS'] = 0
  endelse
  if keyword_set(apply_reference_tiepoints) then begin
    parameters['APPLY_REFERENCE_TIEPOINTS'] = 1
  endif else begin
    parameters['APPLY_REFERENCE_TIEPOINTS'] = 0
  endelse
  if (max_value_divisor ne !NULL) then parameters['MAX_VALUE_DIVISOR'] = max_val_divisor
  if (max_pixel_value ne !NULL) then parameters['MAX_PIXEL_VALUE'] = max_val
  if (base_band ne !NULL) then parameters['BASE_BAND'] = base_band
  if (apply_kappa_filter eq !NULL) then apply_kappa_filter = 0
  parameters['KAPPA_FILTER'] = apply_kappa_filter


  ;==============================================================================================
  ;output parameters
  if ~keyword_set(make_envi_file) then make_envi_file = 0
  parameters['MAKE_ENVI_FILE'] = make_envi_file
  if ~keyword_set(multi_channel) then multi_channel = 0
  parameters['MULTI_CHANNEL'] = multi_channel
  
  ;dont use this - broken!
  if ~keyword_set(histogram_color_balancing) then histogram_color_balancing = 0
  parameters['HISTOGRAM_COLOR_BALANCING'] = 0


  ;==============================================================================================
  ;check the sensor type and set defaults for our processing
  case (parameters.SENSOR) of
    'rededge':begin
      parameters['BASE_BAND'] = 2
      parameters['FILE_IDENTIFIERS'] = ['_1.tif', '_2.tif', '_3.tif', '_5.tif', '_4.tif']

      ;specify the metadata for our output scene
      meta = ENVIRasterMetadata()
      meta['band names'] = ['Blue', 'Green', 'Red', 'Red Edge', 'NIR']
      meta['wavelength units'] = 'nm'
      meta['wavelength'] = [450, 550, 650, 750, 850]
      parameters['METADATA'] = meta

      ;check for other parameter values
      if ~parameters.hasKey('SEARCH_WINDOW_FROM_HEIGHT') then parameters['SEARCH_WINDOW_FROM_HEIGHT'] = 1
      if ~parameters.hasKey('CO_CALIBRATION') then parameters['CO_CALIBRATION'] = 1
      
      ;replace with exif['BitsPerSample'] eventually
      if ~parameters.hasKey('MAX_VALUE_DIVISOR') then parameters['MAX_VALUE_DIVISOR'] = 16s
      if ~parameters.hasKey('MAX_PIXEL_VALUE') then parameters['MAX_PIXEL_VALUE'] = 4095
    end
    'sequoia':begin
      parameters['BASE_BAND'] = 1
      parameters['FILE_IDENTIFIERS'] = ['_GRE.TIF', '_RED.TIF', '_REG.TIF', '_NIR.TIF']

      ;specify the metadata for our output scene
      meta = ENVIRasterMetadata()
      meta['band names'] = ['Green', 'Red', 'Red Edge', 'NIR']
      meta['wavelength units'] = 'nm'
      meta['wavelength'] = [550, 650, 750, 850]
      parameters['METADATA'] = meta

      ;check for other parameter values
      if ~parameters.hasKey('SEARCH_WINDOW_FROM_HEIGHT') then parameters['SEARCH_WINDOW_FROM_HEIGHT'] = 0
      if ~parameters.hasKey('CO_CALIBRATION') then parameters['CO_CALIBRATION'] = 0
      requested_number_of_tiepoints = 500
      minimum_filtered_tiepoints = 100
    end
    else:begin
      ;make sure we have information about our panel identifiers
      if ~parameters.hasKey('FILE_IDENTIFIERS') then begin
        message, 'FILE_IDENTIFIERS not specified in parameters, required for generic sensor!'
      endif

      ;set default parameters
      if ~parameters.hasKey('BASE_BAND') then parameters['BASE_BAND'] = 0
      if ~parameters.hasKey('SEARCH_WINDOW_FROM_HEIGHT') then parameters['SEARCH_WINDOW_FROM_HEIGHT'] = 0
      if ~parameters.hasKey('CO_CALIBRATION') then parameters['CO_CALIBRATION'] = 0
    end
  endcase
  

  ;==============================================================================================
  ;image matching parameters
  if (correlation_search_window eq !NULL) then correlation_search_window = 255
  parameters['CORRELATION_SEARCH_WINDOW'] = correlation_search_window
  if (mutual_search_window eq !NULL) then mutual_search_window = 121
  parameters['MUTUAL_SEARCH_WINDOW'] = mutual_search_window
  if (mutual_matching_score eq !NULL) then mutual_matching_score = .05
  parameters['MUTUAL_MATCHING_SCORE'] = mutual_matching_score
  if (correlation_matching_score eq !NULL) then correlation_matching_score = .6
  parameters['CORRELATION_MATCHING_SCORE'] = correlation_matching_score
  if (requested_number_of_tiepoints eq !NULL) then requested_number_of_tiepoints = 1500
  parameters['REQUESTED_NUMBER_OF_TIEPOINTS'] = requested_number_of_tiepoints
  if (no_secondary_match eq !NULL) then no_secondary_match = 1
  parameters['NO_SECONDARY_MATCH'] = no_secondary_match
  if (minimum_filtered_tiepoints eq !NULL) then minimum_filtered_tiepoints = requested_number_of_tiepoints/10
  parameters['MINIMUM_FILTERED_TIEPOINTS'] = minimum_filtered_tiepoints


  ;do some error checking
  if (panel_reflectance eq !NULL) then panel_reflectance = 70.0 else begin
    if (n_elements(panel_reflectance) gt 1) AND (n_elements(panel_reflectance) ne n_elements(parameters['FILE_IDENTIFIERS'])) then begin
      message, 'Number of elements of PANEL_REFLECTANCE does not match the number of bands, required!'
    endif
  endelse

  ;==============================================================================================
  ;generating GPS file
  if keyword_set(get_gps) then begin
    stackeddir = !NULL
    GOTO, getgps
  endif
  
  
  ;save initial ENVI preferences
  tempdir1 = e.Preferences[ 'directories and files:temporary directory' ].Value
  tempdir2 = e.Preferences[ 'directories and files:output directory' ].Value
  tempdir3 = e.Preferences[ 'directories and files:auxiliary file directory' ].Value
  
;  catch, errorstatus
;  if errorstatus ne 0 then begin
;    catch, /cancel
;    help, /LAST_MESSAGE, OUTPUT=err_txt
;    e.Preferences[ 'directories and files:temporary directory' ].Value = tempdir1
;    e.Preferences[ 'directories and files:output directory' ].Value = tempdir2
;    e.Preferences[ 'directories and files:auxiliary file directory' ].Value = tempdir3
;    p = dialog_message(err_txt)
;    return
;  endif

  
  ;set up preferences for and children processes that we might start up
  if thisfile.endswith('.sav') then begin
    init = ['restore, "' + thisfile + '"']
  endif else begin
    init = ['PREF_SET, "IDL_PATH", !PATH + path_sep(/search_path) + "' + thisdir + '",/COMMIT']
  endelse
  
  ;add updated preferences
  init = [init,$
    'e = envi(/headless)',$
    "e.Preferences[ 'directories and files:temporary directory' ].Value = '" + tempdir + "'",$
    "e.Preferences[ 'directories and files:output directory' ].Value = '" + tempdir + "'",$
    "e.Preferences[ 'directories and files:auxiliary file directory' ].Value = '" + tempdir + "'"]
  parameters['INIT'] = init

  ;search only that directory for input files
  print
  print, '##############################################################'
  print, 'Starting processing at : ' + systime()
  print, '##############################################################'
  print

  ;check how we need to process our data
  ;uses the same routine, but just a nice way to validate that we have support for a "sensor"
  ;could probably be re-worked, ok for now
  case (sensor) of
    ;process rededge data!
    'rededge':begin
      BandAlignment_SetUpSensorForProcessing,$
        PARAMETERS = parameters
    end
    
    'sequoia':begin
      BandAlignment_SetUpSensorForProcessing,$
        PARAMETERS = parameters
    end
    
    'generic':begin
      BandAlignment_SetUpSensorForProcessing,$
        PARAMETERS = parameters
    end
    
    else:begin
     message, 'Unknown sensor type of "' + sensor + '"'
    end
  endcase

  ;set path preferences back to originals
  e.Preferences[ 'directories and files:temporary directory' ].Value = tempdir1
  e.Preferences[ 'directories and files:output directory' ].Value = tempdir2
  e.Preferences[ 'directories and files:auxiliary file directory' ].Value = tempdir3 

  ;only generate GPS and CAM file if we aren;t generating reference tie points
  if keyword_set(parameters.APPLY_REFERENCE_TIEPOINTS) then begin
    getgps:
    
    ;check if the output folder exsits
    if file_test(inputdir + '_out', /DIRECTORY) then begin
      generate_gps_file,$
        INPUTDIR = inputdir,$
        FILE_IDENTIFIERS = parameters.FILE_IDENTIFIERS,$
        OUTPUTDIR = inputdir + '_out'
    endif else begin
      generate_gps_file,$
        INPUTDIR = inputdir,$
        FILE_IDENTIFIERS = parameters.FILE_IDENTIFIERS
    endelse
    
    ;return if this is all we wanted
    if keyword_set(get_gps) then return

    ;create the camera file
    generate_cam_file, INPUTDIR = inputdir, OUTPUTDIR = stackeddir
  endif
  
  ;clean up the temporary directory
  print, '  Attempting to clean up any temporary files...'
  file_delete, parameters.TEMP_DIR, /QUIET, /RECURSIVE
  
  ;search only that directory for input files
  print
  print, '##############################################################'
  print, 'Finished processing at : ' + systime()
  print, '##############################################################'
  print
end
