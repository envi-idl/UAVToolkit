;h+
; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.
;h-

;+
;    Procedure to search in input directory for images and create a GPS file for image stitching software to use. 
;    The resulting GPS file will be created in the directory specified by `INPUTDIR` and the exact file name will 
;    be `images.gps`. 
;
;    Note that this procedure will look for roll, pitch, and yaw in the image exif metadata and it will
;    be set to all 0's if not present. Below are a few examples of how you can use this routine to process data.
;
;    ### Example One
;
;    This first example illustrates how to create a GPS file from a single directory of MicaSense imagery:
;
;        inputdir = 'C:\Users\username\images'
;        generate_gps_file,$
;          INPUTDIR = inputdir,$
;          FILE_IDENTIFIERS = ['_1.tif', '_2.tif', '_3.tif', '_5.tif', '_4.tif']
;
;
;    ### Example two
;
;    This second example is what is used in the `BandAlignmentTask` to procedurally generate a GPS file and place it in the
;    location specified by `OUTPUTDIR`. The images found in `OUTPUTDIR` will be compared against all of the images found
;    in `INPUTDIR`. Doing this eliminates any chance that a GPS file will have an image listed that has not actually been
;    processed. This example is working with Parrot Sequoia data:
;
;        inputdir = 'C:\Users\Traininglead\Desktop\tradeshowData\sequoia\data'
;        outputdir = inputdir + '_out'
;        generate_gps_file,$
;          INPUTDIR = flightdir,$
;          FILE_IDENTIFIERS = ['_GRE.TIF', '_RED.TIF', '_REG.TIF', '_NIR.TIF'],$
;          OUTPUTDIR = flightdir + '_out'
;          
;          
;-



;+
; :Keywords:
;    INPUTDIR : in, type=string
;      This required input is the directory that will be searched for images.
;    FILE_IDENTIFIERS: in, required, type=stringarr 
;      Specify the unique file idetifiers that represent the seeparate files that belog to
;      a single image group for processing. Some examples of what this looks like can be
;      found above.
;    ORIENTATION: in, optional, type=int
;      This optional keyword represents the orientation of the camera on the UAV. This parameter is used by OneButton and you can
;      learn more information about the different camera orientations by reading the OneButton user manual.
;
;      If you know this parameter, then setting it correctly will improve the image registration process because the images will
;      be that much closer to their actual location.
;    OUTPUTDIR : in, optional, type=string
;      Specify this keyword to the location of the multi-page TIFFs of the band-stacked versions of the images in `INPUTDIR`. This will make the procedure
;      search for corresponding files between `INPUTDIR` and  OUTPUTDIR for the GPS file creation. This will also make the GPS
;      file be created in the directory specified by OUTPUTDIR.
;
; :Author: Zachary Norman - Github: znorman-harris
;-
pro generate_gps_file,$
  FILE_IDENTIFIERS = file_identifiers,$
  INPUTDIR = inputdir, $
  OUTPUTDIR = outputdir, $
  ORIENTATION = orientation
  compile_opt idl2, hidden

  ;do some simple error checking
  if (inputdir eq !NULL) then message, 'Directory not specified!'
  if ~file_test(inputdir, /DIRECTORY) then message, 'Directory "' + inputdir + '" does not exist!'
  if (file_identifiers eq !NULL) then message, 'FILE_IDENTIFIERS not specified, requried!'
  
  ;specify the output file
  outfile = inputdir + path_sep() + 'images.gps'

  ;search for image groups in our input directory
  groups = BandAlignment_Get_Image_Groups(inputdir, file_identifiers)

  ;make sure we have some
  if (n_elements(groups) eq 0) then begin
    message, 'No groups found for processing in INPUTDIR!'
  endif

  ;get the keys for our groups
  keys = (groups.keys()).toArray()

  ;check if we specified our output directory
  if keyword_set(outputdir) then begin
    ;make sure it exists
    if file_test(outputdir, /DIRECTORY) then begin
      ;update location for our output file
      outfile = outputdir + path_sep() + 'images.gps'
      
      ;find files in our output directory
      outFiles = file_search(outputdir, '*.tif', COUNT = nOut)
      
      ;make sure that we found files to process
      if (nOut gt 0) then begin
        ;find the images we want to porcess
        idxOk = list()
        
        ;make sure each initial group is also in the output folder in case we had
        ;errors along the way
        foreach name, keys do begin
          idxMatch = where(strpos(outFiles, name) ne -1, countMatch)
          if (countMatch gt 0) then begin
            idxOk.Add, idxMatch[0]
          endif
        endforeach
        
        ;check if we have matches to subset by
        if (n_elements(idxOk) gt 0) then begin
          keys = keys[idxOk.toArray()]
        endif
      endif
    endif
  endif

  ;get the number of matches that we have
  nKeys = n_elements(keys)

  ;get the GPS info for each image
  string_out = strarr(13,nKeys)

  ;print information for extra files
  if (nKeys gt 10) then nprint = floor(nKeys/10) else nprint = 5

  print, ''
  print, 'Getting GPS information...'
  print, ''
  if (nKeys eq 1) then add = '' else add = 's'
  print, '  Generating GPS file for ' + strtrim(nKeys,2) + ' file' + add + '...'
  for i=0,nKeys-1 do begin
    ;get information on our scene
    oImageInfo = image_info((groups[keys[i]])[0], /NO_SPATIALREF)

    ;populate strings with GPS information
    string_out[0,i] = oImageInfo.GetGPSInformation()
    string_out[0,i] = keys[i] + '.tif'

    if (nKeys gt 1) then begin
      if ~(i mod nprint) then begin
        print, '    Processed ' + strtrim(i,2) + ' of ' + strtrim(nKeys,2) + ' file' + add + '...'
      endif
    endif
  endfor
  if (nKeys gt 1) then print, '  Processed all ' + strtrim(nKeys,2) + ' file' + add + '!'

  print, ''

  ;write out output string to a .gps file
  openw, lun, outfile, /GET_LUN
  nchar = max(strlen(string_out[0,*]))

  ;do we have a known orientation to add to the files
  if keyword_set(orientation) then begin
    header = ['Image', 'Longitude', 'Latitude', 'Altitude(m)', 'Roll','Pitch', 'Yaw', 'Orientation']
    ;print the file header
    printf, lun, header, $
      format = '(a' + strtrim(nchar,2) + ',3x,a20,3x,a20,3x,a15,3x,a20,3x,a20,3x,a20,3x,a15)'
    if (nKeys eq 1) then nprint = 1 else nprint = (size(string_out, /DIMENSIONS))[1]
    for i=0, nprint-1 do $
      printf, lun, [string_out[0:6,i], strtrim(orientation,2)] , $
      format = '(a' + strtrim(nchar,2) + ',3x,d20.14,3x,d20.14,3x,a15,3x,d20.14,3x,d20.14,3x,d20.14,3x,a15)'
  endif else begin
    header = ['Image', 'Longitude', 'Latitude', 'Altitude(m)', 'Roll','Pitch', 'Yaw']
    ;print the file header
    printf, lun, header, $
      format = '(a' + strtrim(nchar,2) + ',3x,a20,3x,a20,3x,a15,3x,a20,3x,a20,3x,a20)'
    if (nKeys eq 1) then nprint = 1 else nprint = (size(string_out, /DIMENSIONS))[1]
    for i=0, nprint-1 do $
      printf, lun, string_out[0:6,i] , $
      format = '(a' + strtrim(nchar,2) + ',3x,d20.14,3x,d20.14,3x,a15,3x,d20.14,3x,d20.14,3x,d20.14)'
  endelse
  free_lun, lun

  print, 'Output GPS file: ' + outfile
  print, ''

  ;code for creating a telemetry file for use with custom rig ortho code
  ;    ;write out strings to a telemetry file
  ;    outfile = stackeddir + path_sep() + 'group_telemetry.dat'
  ;    openw, lun, outfile, /GET_LUN
  ;    nchar = max(strlen(string_out[0,*]+ '.tif'))
  ;
  ;    header = ['Image', 'Time', 'Lat', 'Lon', 'Alt', 'Roll','Pitch', 'Yaw(gps)', 'Focal_length(m)', 'Image_plane_ps(m)', 'Pixel_size(m)', 'Ground_elevation(m)']
  ;    ;print the file header
  ;    printf,lun, '------ ' + systime(/UTC) + ' -----'
  ;    printf, lun, header, $
  ;        format = '(a' + strtrim(nchar,2) + ',3x,a25,3x,a20,3x,a20,3x,a15,3x,a20,3x,a20,3x,a20,3x,a20,3x,a20,3x,a20,3x,a20)'
  ;    for i=0, nprint-1 do $
  ;        printf, lun, string_out[0,i] + '.tif', string_out[7,i], string_out[2,i], string_out[1,i], string_out[8,i], string_out[4,i], string_out[5,i], string_out[6,i], string_out[9,i], string_out[10,i], string_out[11,i],string_out[12,i],$
  ;        format = '(a' + strtrim(nchar,2) + ',3x,a25,3x,d20.14,3x,d20.14,3x,a15,3x,d20.14,3x,d20.14,3x,d20.14,3x,d20.14, 3x,d20.14, 3x,d20.14, 3x,d20.14)'
  ;    FREE_LUN, lun
end
