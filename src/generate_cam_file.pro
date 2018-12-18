;h+
; Copyright (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT. See LICENSE.txt for additional details and information.
;h-

;+
;
; 
;    Procedure to generate a CAM formatted file for a directory of images. For the input directory,
;    a random sample of all the images will be selected to check for consistent values for the 
;    focal length of the sensor.
;    
;    If the camera make and model are present, then the output CAM file will be named as
;    model.cam and will either be found in the directory specified by `INPUTDIR` or, if `OUTPUTDIR` is
;    set, then it will be found there.
;    
;    ### Example One
;  
;    This first example will search inputdir for files and the CAM file will be generated in outputdir:
;       
;        inputdir = 'C:\Users\username\images'
;        outputdir = 'C:\Users\username\images_out'
;        generate_cam_file, INPUTDIR = inputdir, OUTPUTDIR = outputdir 
;       
;  
;    ### Example Two
;        
;    This second example illustrates how to create a CAM file and have it be generated in inputdir:
;        
;        inputdir = 'C:\Users\username\images'
;        generate_cam_file, INPUTDIR = inputdir
;        
;-

;+
; :Keywords:
;    INPUTDIR: in, required, type=string
;       Directory to be searched for images to get sensor information from. 
;    EXTENSION: in, optional, type=string, default='tif'
;       File extension that will match the files found in `INPUTDIR`. The default value is 'tif' and
;       should be specified if another file extension is used.
;    OUTPUTDIR: in, optional, type=string, default=`INPUTDIR`
;       Set this keyword to the location where you would like the output CAM file to be written to.
;       If you set this keyword and the directory does not exist, then it will be created.
; 
; :Tooltip:
;    Generates a ".cam" formatted file from reading image metadata
; 
; :Author: Zachary Norman
;-
pro generate_cam_file, INPUTDIR = inputdir, EXTENSION = extension, OUTPUTDIR = outputdir
  compile_opt idl2, hidden

  if ~file_test(inputdir) then message, 'Input directory does not exist!'

  ;check we want out cam file to go
  if keyword_set(outputdir) then begin
    if ~file_test(outputdir) then file_mkdir, outputdir
    camfiledir = outputdir
  endif else begin
    camfiledir = inputdir
  endelse

  ;default file extension
  if ~keyword_set(extension) then extension = 'tif'

  cd, inputdir, CURRENT = first_dir
  files = file_search('*.' + extension, COUNT = nfiles)
  cd, first_dir
  if (nfiles eq 0) then message, 'No files found in "' + inputdir + '"'
  
  ;build full path
  files = inputdir + path_sep() + temporary(files)
  
  ;get sensor information, assume all the same
  oImageInfo = obj_new('image_info', files[0])
  result = oImageInfo.Get(['FOCAL_LENGTH_MM', 'MODEL', 'SENSOR_PIXEL_SIZE_MM'])
  
  ;validate information
  if (result.focal_length_mm eq -!DPI) then begin
    print, ' No metadata to calculate focal length, unable to create CAM file, returning'
    return 
  endif
  if (result.SENSOR_PIXEL_SIZE_MM eq -!DPI) then begin
    print, ' No metadata to calculate focal length, unable to create CAM file, returning'
    return
  endif
  
  ;initialize a list to hold the CAM file info
  cam_file = list()

  ;add focal length
  cam_file.add, '#Focal Length (mm)'
  cam_file.add, 'FOCAL ' + strtrim(result.focal_length_mm,2) + 'e+000'

  ;add the pixel size
  cam_file.add, ''
  cam_file.add, '#PixelSize ' + strtrim(result.SENSOR_PIXEL_SIZE_MM*1000d,2) + 'e-003'

  ;convert our list to an array
  cam_file = cam_file.toarray()

  ;check if we have sensor model for CAM file name
  if (result.MODEL ne !NULL) then begin
    outname = strlowcase(result.MODEL) + '.cam'
  endif else begin
    outname = 'camera_info.cam'
  endelse
  outfile = camfiledir + path_sep() + outname

  ;write to disk
  openw, lun, outfile, /GET_LUN
  printf, lun, cam_file, /IMPLIED_PRINT
  free_lun, lun

  ;alert user to location
  print, 'Camera file : ' + outfile
  print, ''
end
