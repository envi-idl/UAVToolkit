;h+
; Copyright (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT. See LICENSE.txt for additional details and information.
;h-

;+
;  This file contains the code needed to convert the raw pixel values from a 
;  RedEdge image directly to radiance. This workflow was taken from the following 
;  source:
;  
;      [https://support.micasense.com/hc/en-us/articles/115000351194-RedEdge-Camera-Radiometric-Calibration-Model](https://support.micasense.com/hc/en-us/articles/115000351194-RedEdge-Camera-Radiometric-Calibration-Model)
;  
;  It does not currently account for sensor orientation with a DLS, but support is planned.
;  
;  You will need a sensor that has the correct metadata for this function to work without throwing
;  an error during processing and it should be used with a catch block to ensure that the 
;  source program continues running if there are issues.
;  
;  
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-

;+
; :Private:
; 
; :Description:
;    Simple function that validates and returns nested keys
;    from within a hash/distionary object.
;    
;    It will return the value from the hash if it exists, otherwise
;    an error will be thrown.
;    
;    If the item in question is a list, this function converts the list
;    to an array for convenience.
;
; :Params:
;    item: in, required, type=hash/dictionary
;      The hash or dictionary that you want to extract keys from
;    keys: in, required, type=string/string array
;      Specify the key/keys that you want to check and extract
;
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
function extract_metadata, item, keys
  compile_opt idl2
  on_error, 2

  if ~isa(item, 'hash') && ~isa(item, 'dictionary') then begin
    message, 'item is not of type hash or dictionary, required!'
  endif

  foreach key, keys, i do begin
    if (i eq 0) then begin
      if ~item.hasKey(key) then begin
        message, 'XMLPacket metadata does not have expected metadata tags.'
      endif
      meta = item[key]
    endif else begin
      if ~meta.hasKey(key) then begin
        message, 'XMLPacket metadata does not have expected metadata tags.'
      endif
      meta = meta[key]
    endelse
  endforeach
  
  ;return the extracted metadata, handle array conversion
  if isa(meta, 'list') then begin
    return, meta.toArray()
  endif else begin
    return, meta
  endelse
end

;+
; :Private:
; 
; :Description:
;    Returns 2D array which is the correction for vignetting with the sensor.
;
; :Params:
;    coeff: in, required, type=array[6]
;      Contains the 6 polynomial coefficients for correcting vignetting effects
;    dims: in, required, type=array[2]
;      The dimensions of the image in columns, rows.
;
; :Keywords:
;    X: out, optional, type=array
;      The 2D array with X pixel locations
;    Y: out, optional, type=array
;      The 2D array with Y pixel locations
;    R: out, optional, type=array
;      The 2D array that contains the equation sqrt((x-cx)^2  + (y-cy)^2)
;
; :Author: Zachary Norman - GitHub:[znorman-harris](https://github.com/znorman-harris)
;-
function vmap, coeff, dims, CENTER = center, X = x, Y = y, R = r
  compile_opt idl2
  if (n_elements(dims) ne 2) then begin
    message, 'Dims are not two element array, required!'
  endif
  if (n_elements(coeff) ne 6) then begin
    message, 'Coefficients are not six element array, required!'
  endif
  
  ;get center if not specified
  if (n_elements(center) eq 0) then begin
    cX = dims[0]/2 - 1
    cY = dims[1]/2 - 1
  endif else begin
    cX = center[0]
    cY = center[1]
  endelse

  ;create our x and y arrays
  x1d = lindgen(dims[0])
  y1d = lindgen(dims[1])
  
  ;make 2d
  x = lonarr(dims[0], dims[1], /NOZERO)
  y = lonarr(dims[0], dims[1], /NOZERO)
  
  ;populate with values
  for i=0, dims[0]-1 do y[i,*] = y1d
  for j=0, dims[1]-1 do x[*,j] = x1d
  
  r = sqrt((x - cX)^2 + (y - cY)^2)
  return, (1.0/(1.0 + coeff[0]*r + coeff[1]*r^2 + coeff[2]*r^3 + coeff[3]*r^4 + coeff[4]*r^5 + coeff[5]*r^6))
end


;+
; :Description:
;    Function that takes RedEdge data and converts the pixel values directly to radiance if
;    all of the necessary metadata is present. This routine does not account for DLS sensors 
;    and the orientation of the sensor, but that support is planned.
;    
;    This function is overly cautious and makes sure that all metadata is present, otherwise an
;    error is thrown. This should be used with a catch block to ensure that we can process our data
;    correctly.
;
; :Params:
;    file: in, required, type=string
;      Specify the RedEdge file that you want to open and attempt to convert to radiance.
; :Keywords:
;    DAT: in/out, optional, type=array
;      For **input**, specify the image data for the scene that will have the correction applied.
;      
;      For **output**, this keyword contains the original scene data so that you don't need to 
;      read it in multiple times.
;
; :Tooltip:
;    Calibrates a RedEdge image to radiance using sensor metadata
;
; :Author: Zachary Norman - GitHub:[znorman-harris](https://github.com/znorman-harris)
;-
function rededge_to_radiance, file, DAT = dat, VERBOSE = verbose
  compile_opt idl2
  on_error, 2
  
  ;read the image metadata
  exif = read_exif(file)
  
  ;parse the XML packet to get the important, new metadata
  xml = xml_parse(string(exif['xmlpacket']))
  
  ;extract the keys that we are interested in
  meta = extract_metadata(xml, ['x:xmpmeta', 'rdf:RDF', 'rdf:Description'])
  
  ;validate metadata
  if ~isa(meta, /ARRAY) then begin
    message, 'Extracted metadata is not an array as expected, unknown metadata type.'
  endif
  
  ;identify which item in our list is what we want
  micaSenseTag = '%xmlns:MicaSense'
  micaSenseIdx = -1
  cameraTag = '%xmlns:Camera'
  cameraIndex = -1
  dlsTag = '%xmlns:DLS'
  dlsIndex = -1
  
  foreach item, meta, i do begin
    ;extract the metadata tags
    tags = strlowcase((item.keys()).toArray())
    case (1) of
      (where(strlowcase(micaSenseTag) eq tags, /NULL) ne !NULL):begin
        micaSenseIdx = i
      end
      (where(strlowcase(cameraTag) eq tags, /NULL) ne !NULL):begin
        cameraIdx = i
      end
      (where(strlowcase(dlsTag) eq tags, /NULL) ne !NULL):begin
        dlsIdx = i
      end
      else:;do nothing
    endcase
  endforeach
  
  ;validate correct tags
  if (cameraIdx eq -1) then begin
    message, 'XMLPacket metadata is missing camera metadata.'
  endif
  if (micaSenseIdx eq -1) then begin
    message, 'XMLPacket metadata is missing MicaSense metadata.'
  endif
  
  ;extract metadata items
  camMeta = meta[cameraidx]
  micaMeta = meta[micaSenseIdx]
  
  ;extract dark pixels and dark level for scene
  darkPixel = exif['Exif_Image_BlackLevel']
  
  ;this comes from the GitHub page, but is not the correct approach, instead use above.
  ;darkPixel = double(extract_metadata(micaMeta, ['MicaSense:DarkRowValue', 'rdf:Seq', 'rdf:li']))
  darkLevel = darkPixel.mean()
  
  ;extract our calibration coefficients
  cal = double(extract_metadata(micaMeta, ['MicaSense:RadiometricCalibration', 'rdf:Seq', 'rdf:li']))
  
  ;extract basic information about our scene
  exposureTime = float(exif['ExposureTime'])
  gain = float(exif['ISOSpeed'])/100.0
  dims = [exif['ImageWidth'], exif['ImageLength']]
  
  ;get the bits per sample
  bits = exif['BitsPerSample']
  
  ;extract vignetting coefficients
  vCoeff = extract_metadata(camMeta, ['Camera:VignettingPolynomial', 'rdf:Seq', 'rdf:li'])
  vCenter = extract_metadata(camMeta, ['Camera:VignettingCenter', 'rdf:Seq', 'rdf:li'])
  
  ;create vignette map of our correction coefficients
  vMap = vmap(vCoeff, dims, CENTER = vCenter, X = x, Y = y)
  
  ;check if we want to print information to the screen, do this here because we are OK to convert
  ;at this point in the process
  if keyword_set(verbose) then begin
    print, strjoin([exif['Make'], exif['Model'], exif['Software']], ' ')
    print, 'Exposure time     : ', strtrim(exposureTime,2)
    print, 'Imager gain       : ', strtrim(gain,2)
    print, 'Size              : ', strjoin(strtrim(dims,2), 'x') + ' pixels
    print, 'Band name         : ', extract_metadata(camMeta, ['Camera:BandName'])
    print, 'Center wavelength : ', extract_metadata(camMeta, ['Camera:CentralWavelength'])
    print, 'Bandwidth         : ', extract_metadata(camMeta, ['Camera:WavelengthFWHM'])
    print, 'Capture ID        : ', extract_metadata(micaMeta, ['MicaSense:CaptureId'])
    print, 'Flight ID         : ', extract_metadata(micaMeta, ['MicaSense:FlightId'])
  endif
  
  ;read in the TIFF data if we haven't already passed it in
  if ~isa(dat, /ARRAY) then begin
    dat = read_tiff(file)
  endif
  
  ;normalize our data
  datNorm = (float(dat) - darkLevel)/(2.0^bits)
  
  ;convert to radiance
  return, (vmap*float((cal[0]/gain)*(datNorm/(exposuretime + y*(cal[1] - cal[2]*exposureTime)))) > 0)
end
