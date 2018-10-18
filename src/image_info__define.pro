;h+
; (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT. See LICENSE.txt for additional details and information.
;h-

;+
; :Private:
;
;
;-

;+
;
;   The purpose of this function is to get the approximate ground elevation in
;   meters above sea level. This function is used internally for most of the other
;   routines in the UAV Toolkit so that approximate pixel sizes can be calculated.
;
;
; :Returns:
;    Height above the ground, meters
;
; :Params:
;    longitude: in, required, type=float/double
;       Longitude (decimal degrees) over the location that you want the ground height
;    latitude: in, required, type=float/double
;       Latitude (decimal degrees) over the location that you want the ground height
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
function image_info_ground_height, longitude, latitude
  compile_opt idl2, hidden

  e = envi(/current)
  if (e eq !NULL) then e = envi(/headless)
  file = Filepath('GMTED2010.jp2', Subdir=['data'], $
    Root_Dir=e.Root_Dir)
  DEMraster = e.openraster(file)

  spatialref = DEMraster.spatialref
  spatialref.ConvertLonLatToMap, longitude, latitude, MapX, MapY
  spatialref.ConvertMapToFile, MapX, MapY, FileX, FileY

  pixel = DEMraster.GetData(SUB_RECT=[FileX, FileY, FileX, FileY])
  DEMraster.close
  return, float(pixel[0])
end


;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    infile: in, required, type=string
;       This required argument should be set to the fully-qualified file path to an
;       image that will have it's metadata searched to create a spatial reference 
;       and get other basic information about the acquisition of the photo.
; 
; :Keywords:
;    PRINT_GPS: in,  optional, type=int, default=0
;       Keyword to set which will not print any information regarding the image position.
;    NO_SPATIALREF: in, optional, type=int, default=0
;       Set this keyword to not create a spatial reference for the `infile`. Doing this
;       won't require using ENVI to get the image dimensions
;
; :Author: Norm3596
;-
function image_info::init, infile, PRINT_GPS = print_gps, NO_SPATIALREF = no_spatialref, NO_PRINT = no_print, ERROR_GPS = error_gps
  compile_opt idl2
;  on_error,2

  ;routine parameters that are hard coded:
  epsg = 4326                            ;epsg code for WGS84 spatial reference
  convert = (180d/(!CONST.R_EARTH*!DPI)) ;convert meters to degrees

  if ~keyword_set(no_spatialref) then begin
    ;grab reference to ENVI, otherwise start it
    e = envi(/current)
    if (e eq !NULL) then e = envi(/headless)
  endif

  ;make sure our input file exists
  if ~file_test(infile) then begin
    message, 'INFILE specified, but does not exist!
;    return, 0
  endif
  
;  if (print_gps eq !NULL) then begin
;    print_gps = 1
;  endif
  
  ;remember the file
  self.FILE = infile
  
  ;give the object some default values
  self.ACQUISITION_TIME = ''
  self.ALTITUDE = -!DPI
  self.CCD_SIZE_MM = [-!DPI, -!DPI]
  self.EXPOSURETIME=-!DPI
  self.FOCAL_LENGTH_MM = -!DPI
  self.HEIGHT_ABOVE_GROUND = -!DPI
  self.GROUND_HEIGHT = -!DPI
  self.GSD = [-!DPI, -!DPI]
  self.IMAGE_SIZE_PX = long([0,0])
  self.ISO = -!DPI
  self.LON_LAT = [-!DPI,-!DPI]
  self.MAKE = ''
  self.MODEL = ''
  self.SENSOR_ORIENTATION = [0d, 0d, 0d]
  self.SENSOR_PIXEL_SIZE_MM = -!DPI
  self.SPATIALREF = obj_new()
  
  meta = read_exif(infile)
  
  if (meta eq !NULL) then begin
    if ~keyword_set(no_print) then begin
      print, 'No image metadata, using fake spatial ref...'
    endif
    void = query_image(infile, info)
    npx = info.DIMENSIONS[0]
    npy = info.DIMENSIONS[1]
    self.IMAGE_SIZE_PX = long([npx, npy])
    GOTO, NO_POSITION_INFORMATION 
  endif

  ;check for the sensor make and model
  if meta.haskey('make') then begin
    sensor_make = strjoin(strsplit(strlowcase(meta.make),/extract),'_')
    self.MAKE = sensor_make
  endif
  if meta.haskey('model') then begin
    sensor_model = strjoin(strsplit(strlowcase(meta.model),/extract),'_')
    self.MODEL = sensor_model
  endif

  ;if there aren't tags for the image width and length, then open as an ENVIRaster
  if ~meta.haskey('ImageWidth') OR  ~meta.haskey('ImageLength') then begin
    ;    raster = e.openraster(infile)
    ;    npx = raster.NCOLUMNS
    ;    npy = raster.NROWS
    ;    raster.close
    void = query_image(infile, info)
    npx = info.DIMENSIONS[0]
    npy = info.DIMENSIONS[1]
  endif else begin
    ;get the image size in pixels
    npx = meta.ImageWidth
    npy = meta.ImageLength
  endelse
  self.IMAGE_SIZE_PX = long([npx, npy])
  
  ;check and see if we have exposure or ISO information
  meta_keys = (meta.keys()).toarray()
  exposure_line = where(meta_keys.contains('ExposureTime') eq 1, yes_exposure)
  if (yes_exposure eq 1) then begin
    self.EXPOSURETIME = meta[meta_keys[exposure_line[0]]]
  endif
  iso_line = where(meta_keys.contains('ISO') eq 1, yes_iso)
  if (yes_iso eq 1) then begin
    self.ISO = meta[meta_keys[iso_line[0]]]
  endif
  
  ;check to make sure we have the necessary GPS location information
  if ~meta.haskey('GPSLongitude') then begin
    if ~keyword_set(no_print) then begin
      print, 'Missing exif tag GPSLongitude, cannnot determine image location, using fake spatial ref...'
    endif else if keyword_set(error_gps) then begin
      message, 'Missing exif tag GPSLongitude, cannnot determine image location, using fake spatial ref...'
    endif
    GOTO, NO_POSITION_INFORMATION
  endif

  if ~meta.haskey('GPSLongitudeRef') then begin
    if ~keyword_set(no_print) then begin
      print, 'Missing exif tag GPSLongitudeRef, cannnot determine image location, using fake spatial ref...'
    endif else if keyword_set(error_gps) then begin
      message, 'Missing exif tag GPSLongitudeRef, cannnot determine image location, using fake spatial ref...'
    endif
    GOTO, NO_POSITION_INFORMATION
  endif

  if ~meta.haskey('GPSLatitude') then begin
    if ~keyword_set(no_print) then begin
      print, 'Missing exif tag GPSLatitude, cannnot determine image location, using fake spatial ref...'
    endif else if keyword_set(error_gps) then begin
      message, 'Missing exif tag GPSLatitude, cannnot determine image location, using fake spatial ref...'
    endif
    GOTO, NO_POSITION_INFORMATION
  endif

  if ~meta.haskey('GPSLatitudeRef') then begin
    if ~keyword_set(no_print) then begin
      print, 'Missing exif tag GPSLatitudeRef, cannnot determine image location, using fake spatial ref...'
    endif else if keyword_set(error_gps) then begin
      message, 'Missing exif tag GPSLatitudeRef, cannnot determine image location, using fake spatial ref...'
    endif
    GOTO, NO_POSITION_INFORMATION
  endif

  ;get lat/lon
  lat = meta.GPSLatitude
  lon = meta.GPSLongitude
  ;account for S/W latitude/longitude
  if (meta.GPSLatitudeRef eq 'S') then lat = -lat
  if (meta.GPSLongitudeRef eq 'W') then lon = -lon
  
  ;convert to decimal degrees
  ddeglat = lat[0] + lat[1]/60d + (lat[2]/3600d)
  ddeglon = lon[0] + lon[1]/60d + (lon[2]/3600d)
  
  ;save in self
  self.LON_LAT = [ddeglon, ddeglat]

  ;checkf or the altitude
  if ~meta.haskey('GPSAltitude') then begin
    if ~keyword_set(no_print) then begin
      print, 'Missing exif tag GPSAltitude, cannnot determine image location, using fake spatial ref...'
    endif else if keyword_set(error_gps) then begin
      message, 'Missing exif tag GPSAltitude, cannnot determine image location, using fake spatial ref...'
    endif
    GOTO, NO_POSITION_INFORMATION
  endif
  
  altitude = meta.GPSAltitude
 
  ;get the height above ground using GMTED2010
  if ~keyword_set(no_spatialref) then begin
    ground_elevation = image_info_ground_height(ddeglon, ddeglat)
    
    ;assume that if altitude is less than ground that the altitude is height
    ;above the ground
    if (altitude lt ground_elevation) then begin
      print, 'Warning - image altitude less than reference DEM elevation'
      ;      altitude += ground_elevation
    endif
    
    self.GROUND_HEIGHT = ground_elevation
    height_above_ground = altitude - ground_elevation
  endif else begin
    height_above_ground = altitude
  endelse
 
  self.ALTITUDE = altitude
  ;do some error catching for the height above ground is negative,
  ;in this case just replace the value with the elevation
  if (height_above_ground lt 0) then begin
    if keyword_set(print_gps) then begin
      print, string(9b) + 'Sensor altitude less than ground height, setting ground height to altitude.'
    endif
    height_above_ground = abs(altitude)
  endif

  self.HEIGHT_ABOVE_GROUND = height_above_ground
  
  if keyword_set(print_gps) then begin
    print, string(9b) + 'Sensor latitude (deg) : [ ' + strtrim(ddeglat,2) + ' ]'
    print, string(9b) + 'Sensor longitude (deg): [ ' + strtrim(ddeglon,2) + ' ]'
    print, string(9b) + 'Sensor altitude (m)   : [ ' + strtrim(altitude,2) + ' ]'
  endif

  ;tie points
  centertiex = npx/2
  centertiey = npy/2

  ;check to make sure we have the focal length
  if ~meta.haskey('FocalLength') then begin
    if ~keyword_set(no_print) then begin
      print, 'Missing exif tag FocalLength, cannnot determine image extents, using fake spatial ref...'
    endif
    GOTO, NO_POSITION_INFORMATION
  endif

  ;assume focal length is in millimeters
  ;not sure how to check the value for units
  focal_length_mm = meta.FocalLength
  self.FOCAL_LENGTH_MM = focal_length_mm
  
  ;convert focal length to meters
  focal_length = focal_length_mm/1000d
 
  if ~meta.haskey('FocalPlaneXResolution') then begin
    if meta.haskey('XResolution') then begin
      FocalPlaneXResolution = meta.XResolution
    endif else begin
      if ~keyword_set(no_print) then begin
        print, 'Missing exif tag FocalPlaneXResolution, cannnot determine pixel sizes, using fake spatial ref...'
      endif
      GOTO, NO_POSITION_INFORMATION
    endelse
  endif else begin
    FocalPlaneXResolution = meta.FocalPlaneXResolution
  endelse
  
  ;make sure we can calculate the pixel sizes
  if ~meta.haskey('FocalPlaneResolutionUnit') then begin
    if meta.haskey('ResolutionUnit') then begin
      units = meta.ResolutionUnit
    endif else begin
      if ~keyword_set(no_print) then begin
        print, 'Missing exif tag FocalPlaneResolutionUnit, cannnot determine pixel sizes, using fake spatial ref...'
      endif
      GOTO, NO_POSITION_INFORMATION
    endelse
  endif else begin
    units = meta.FocalPlaneResolutionUnit
  endelse


  if (units eq 2) AND (FocalPlaneXResolution eq 72) then begin
    message, 'No reliable information to determine sensor parameters, XResolution and ResolutionUnit set at unknown values.'
  endif

;  ;determine pixel size, assume square pixels
;  
;  lon_lat = self.Get('LON_LAT')
;  
;  zone = round((lon_lat[0] + 183)/6.)
;  utm = envi_proj_create(/utm, datum='WGS-84', zone=zone)
;  coord_sy_str = utm.PE_COORD_SYS_STR
;  utm_coord_sys = ENVICoordSys(COORD_SYS_STR = coord_sy_str)
;  
;  stop
  ;check the units for the pixels, convert the pixel size to mm/pixel for creating a CAM file
  case units of
    1:begin
      print, 'Unkown FocalPlaneResolutionUnit, cannnot determine pixel sizes, returning...'
      GOTO, NO_POSITION_INFORMATION
    end
    ;unknown assumes inches
    ;inches (gross)
    2:begin
      resolution = FocalPlaneXResolution*(1/2.54d) ;([pixels/inch] * [inch/cm]) = [pixels/cm]
      sensor_pixel_size = (10d)*(1/resolution);([mm/cm])*([cm/pixel]) = [mm/pixel]
    end
    ;centimeters
    3:begin
      resolution = double(FocalPlaneXResolution) ;([pixels/cm])
      sensor_pixel_size = (10d)*(1/resolution);([mm/cm])*([cm/pixel]) = [mm/pixel]
    end
    ;millimeters
    4:begin
      resolution = FocalPlaneXResolution ;[pixels/mm]
      sensor_pixel_size = (1/resolution);(([mm/pixel]) = [mm/pixel]
    end
  endcase
  
  ;sensor size, millimeters
  CCD_size = [sensor_pixel_size*npx, sensor_pixel_size*npy]
  self.CCD_SIZE_MM = CCD_SIZE
  self.SENSOR_PIXEL_SIZE_MM = sensor_pixel_size

  ;size of the image, meters
  im_xs = (CCD_size[0]/focal_length_mm)*height_above_ground
  im_ys = (im_xs*npy)/npx

  ;calculate pixel size as x size divided by npy and assume square pixels
  ;convert to degrees!!
  xpixsize = (im_xs/npx)
  ypixsize = (im_ys/npy)

  self.GSD = [xpixsize, ypixsize]

  ;convert pixel sizes to degrees
  xpixsize_deg = xpixsize*convert
  ypixsize_deg = xpixsize*convert

  ;create the spatial reference from the metadata
  if ~keyword_set(no_spatialref) then begin
    spatialref = ENVIStandardRasterSpatialRef($
      coord_sys_code = epsg, $
      /GEOGCS,$
      pixel_size = [xpixsize_deg, ypixsize_deg], $
      tie_point_pixel = [centertiex, centertiey], $
      tie_point_map = [ddeglon, ddeglat])
    ;set the image  properties
    self.SPATIALREF = spatialref
  endif


  ;lets check and see if we have sensor orientation values

  ;only check for the Roll, pitch, and yaw if we have the XML packet
  if meta.haskey('XMLPacket') then begin
    ;determine the roll, pitch, and yaw, split the one string based upon return bytes
    rotation_xml = strsplit(strlowcase(string(meta.XMLPacket)), string(10b), /extract)

    ;need to determine if the exif metadata is formatted correctly or not
    ;sometimes we have XML formatted tags othertimes pseudo XML format with quotes
    ;no XML SAX or DOM parser, just find the correct lines
    rollpos = where(rotation_xml.contains('camera:roll') eq 1)
    pitchpos = where(rotation_xml.contains('camera:pitch') eq 1)
    yawpos = where(rotation_xml.contains('camera:yaw') eq 1)

    ;check to see if we actually have roll, pitch, yaw metadata
    if (yawpos eq -1) OR (pitchpos eq -1) OR (rollpos eq -1) then begin
      roll = strtrim(0d,2)
      pitch = strtrim(0d,2)
      yaw = strtrim(0d,2)
    endif else begin
      rollline  = strtrim(rotation_xml[rollpos],2)
      pitchline = strtrim(rotation_xml[pitchpos],2)
      yawline   = strtrim(rotation_xml[yawpos],2)
      ;check to see the format of the XML metadata (some have XML tags others have quotes)
      if (strpos(yawline, '"') eq -1) then begin
        roll  = strmid(rollline,strpos(rollline,'>')+1, strpos(rollline,'<', /REVERSE_SEARCH)-strpos(rollline,'>')-1)
        pitch = strmid(pitchline,strpos(pitchline,'>')+1, strpos(pitchline,'<', /REVERSE_SEARCH)-strpos(pitchline,'>')-1)
        yaw   = strmid(yawline,strpos(yawline,'>')+1, strpos(yawline,'<', /REVERSE_SEARCH)-strpos(yawline,'>')-1)
      endif else begin
        roll  = strmid(rollline,strpos(rollline,'"')+1, strpos(rollline,'"', /REVERSE_SEARCH)-strpos(rollline,'"')-1)
        pitch = strmid(pitchline,strpos(pitchline,'"')+1, strpos(pitchline,'"', /REVERSE_SEARCH)-strpos(pitchline,'"')-1)
        yaw   = strmid(yawline,strpos(yawline,'"')+1, strpos(yawline,'"', /REVERSE_SEARCH)-strpos(yawline,'"')-1)
      endelse
      rollval = double(roll)
      if (rollval gt 50) then begin
        rollval -= 90
        roll = strtrim(rollval,2)
      endif
    endelse
    ;no XML packet
  endif else begin
    roll = strtrim(0d,2)
    pitch = strtrim(0d,2)
    yaw = strtrim(0d,2)
  endelse
  self.SENSOR_ORIENTATION = double([roll, pitch, yaw])
  
  ;check if we have the acquisition time
  if meta.haskey('DateTime') then begin
    ;get the time information in the format for telemetry.dat
    datetime = meta.DateTime
    datetime = strsplit(datetime, /EXTRACT)
    datetime = strjoin([strjoin(strsplit(datetime[0],':',/extract),'-'), datetime[1]],' ')
    self.ACQUISITION_TIME = datetime
  endif 
  

  if (0 eq 1) then begin
    ;there is no spatial reference information, so we need to make a fake one
    NO_POSITION_INFORMATION:
    if ~keyword_set(no_spatialref) then begin
      spatialref = ENVIStandardRasterSpatialRef($
        coord_sys_code = epsg, $
        /GEOGCS,$
        pixel_size = [(1d/(npx > npy)), (1d/(npx > npy))], $
        tie_point_pixel = [0d, 0d], $
        tie_point_map = [0d, 0d])
      ;set the image  properties
      self.SPATIALREF = spatialref
    endif
    self.FAKE_SREF = !TRUE
  endif else begin
    self.FAKE_SREF = !FALSE
  endelse
  
  return,1
end

;+
; :Description:
;    Object method to return information about an INAGE_INFO object. See
;    `tagnames` for specifying the input. 
;    
;    If a requested tag does not exist for the object, then it will not be
;    returned. If your result is equal to !NULL then there were no matches 
;    for the tag names that were requested.
;
; :Params:
;    tagnames: in, required, type=string/stringarr
;       This parameters should be set to one of the parameters defined in 
;       the object definition to be returned. If you only specify one tag
;       to return, then you will just get that value. Otherwise, a structure
;       will be returned with a key for each requested tag that is found.
;       
;       This argument is ignored if the keyword `ALL` is set.
; 
; :Keywords:
;    ALL: in, optional, type=int, default=0
;       Set this keyword to return all of the object properties from 
;-
function image_info::Get, tagnames, ALL = all
  compile_opt idl2
  on_error,2
  
  ntags = n_elements(tagnames)
  tags = strlowcase(tag_names(self))
  
  if keyword_set(all) then begin
    tagnames = tags[3:*]
  endif else begin
    if (tagnames eq !NULL) then message, 'TAGNAMES were not supplied, required argument!'
  endelse
  
  sorted_tags = strlowcase(tagnames[reverse(sort(strlowcase(tagnames)))])
  
  if (n_elements(sorted_tags) eq 1) then begin
    ;check to see which tags we asked for
    tag_idx = (where(tags eq sorted_tags[0], is_tag))[0]
    if (is_tag eq 1) then begin
      return, self.(tag_idx)
    endif else begin
      return, !NULL
    endelse
  endif else begin
    outstruct = !NULL
    foreach tag, sorted_tags do begin
      ;check to see which tags we asked for
      tag_idx = (where(tags eq tag, is_tag))[0]
      if (is_tag eq 1) then begin
        if (outstruct eq !NULL) then begin
          outstruct = create_struct(strupcase(tag), self.(tag_idx))
        endif else begin
          outstruct = create_struct(strupcase(tag), self.(tag_idx), outstruct)
        endelse
      endif
    endforeach
  endelse
  
  return, outstruct
end


;+
; :Description:
;    Function method to return the GPS information for the iamge that the object
;    represents. The returned string array will contain the: image name, longitude,
;    latitude, altitude, roll, pitch, yaw, and acquisition time.
;
; :Keywords:
;    FULL_FILE_PATH: in, optional, type=int, default=0
;       Set this keyword to have the returned GPS information include the fully-qualified file
;       path the the image that the information is for.
;
;-
function image_info::GetGPSinformation, FULL_FILE_PATH = fill_file_path, GPS_ALTITUDE_OFFSET = gps_altitude_offset
  compile_opt idl2
  
  if (gps_altitude_offset eq !NULL) then begin
    gps_altitude_offset = 0.0
  endif else begin
    gps_altitude_offset = float(gps_altitude_offset)
  endelse
  
  string_out = strarr(8)
  
  ;save some information about the image [ filename, longitude, latitude, altitude, orientation]
  if keyword_set(full_file_path) then begin
    string_out[0] = self.FILE
  endif else begin
    string_out[0] = file_basename(self.FILE)
  endelse
  
  ;check to make sure we have GPS tags
  if (self.LON_LAT[0] eq -!DPI) then begin
    message, 'No GPS information found in exif tags for image!'
  endif
  
  string_out[1] = string(self.LON_LAT[0], format = '(d20.14)')
  string_out[2] = string(self.LON_LAT[1], format = '(d20.14)')
  string_out[3] = strtrim(self.ALTITUDE + gps_altitude_offset,2)
  string_out[4] = string(self.SENSOR_ORIENTATION[0], format = '(d20.14)')
  string_out[5] = string(self.SENSOR_ORIENTATION[1], format = '(d20.14)')
  string_out[6] = string(self.SENSOR_ORIENTATION[2], format = '(d20.14)')
  string_out[7] = self.ACQUISITION_TIME
  
  return, string_out
end


;+
; :Description:
;    Simple object method to overload printing.
;
;-
function image_info::_overloadPrint
  tags = tag_names(self)
  output = list()
  
  ;determine the number of white spaces we need to add
  maxlen = max(strlen(tags)) + 6
  
  for i=3,n_elements(tags)-1 do begin
    help, self.(i), OUTPUT=o
    split = strsplit(o, /EXTRACT)
    type = split[1]
    add = ''
    for j=0, maxlen-strlen(tags[i]) do add += ' '
    output.add, string(9b) + strupcase(tags[i]) + add + type
    if ISA(self.(i), 'OBJREF') then begin
      output.add, string(9b) + string(9b) + type
    endif else begin
      output.add, string(9b) + string(9b) + strjoin(strtrim(self.(i),2),'     ')
    endelse
  endfor

  return, strjoin(output.toarray(), string(10b))
end

;+
; :Description:
;    Simple object method to overload implied printing.
;
;-
function image_info::_overloadImpliedPrint, varname
  return, self->image_info::_overloadPrint()
end


;+
; :Description:
;    IMAGE_INFO object definition
;
;-
pro image_info__define
  compile_opt idl2, hidden
  struct = {IMAGE_INFO, $
    INHERITS IDL_Object,$
    ACQUISITION_TIME:'',$          ;image capture time
    ALTITUDE:double(1),$           ;sensor altitude, meters
    CCD_SIZE_MM:dblarr(2),$        ;CCD_SIZE in millimeters
    EXPOSURETIME:double(1),$       ;exposure time
    FAKE_SREF: !FALSE,$
    FILE:'',$                      ;save the file that the object represents
    FOCAL_LENGTH_MM:double(1),$    ;focal length in millimeters
    GSD:dblarr(2),$                ;approximate GSD X,Y in meters
    GROUND_HEIGHT:double(1),$      ;ground elevation, DEM units
    HEIGHT_ABOVE_GROUND:double(1),$ ;sensor height above ground
    IMAGE_SIZE_PX:lonarr(2),$      ;image size in pixels
    ISO:double(1),$                ;image ISO speed
    LON_LAT:dblarr(2),$            ;sensor longitude and latitude
    MAKE:'',$                      ;sensor make
    MODEL:'',$                     ;sensor model
    SENSOR_ORIENTATION:dblarr(3),$ ;sensor orientation, roll, pitch, and yaw
    SENSOR_PIXEL_SIZE_MM:double(1),$  ;sensor pixel size in millimeters
    SPATIALREF:obj_new()}          ;image spatial reference
end
