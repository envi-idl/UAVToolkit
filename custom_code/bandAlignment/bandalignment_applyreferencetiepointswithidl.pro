;h+
; Copyright (c) 2019 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT. See LICENSE.txt for additional details and information.
;h-

;+
; :Private:  
;
;-

;+
; :Description:
;    Procedure that performs the image-image registration in memory using
;    IDL and an RST warping method. It returns the output spatial reference
;    and a pointer contianing the data which can be used to create an output 
;    raster. 
;    
;    For example, after running the procedure you can create a raster like this:
;    
;        ;register our bands using IDL
;        BandAlignment_ApplyReferenceTiePointsWithIDL,$
;          INPUT_RASTER = input_raster,$
;          INPUT_BANDALIGNMENTTIEPOINTS = filtered_tiepoints,$
;          REFERENCE_BAND = reference_band,$
;          OUTPUT_SPATIALREF = output_spatialref,$
;          OUTPUT_DATA_POINTER = output_data_pointer
;        
;        ;save as an ENVI raster
;        newRaster = ENVIRaster(*output_data_pointer, SPATIALREF = output_spatialref)
;        newRaster.save
;    
;    The purpose of providing a pointer is to reduce memory costs with larger scenes and to give
;    users the options to save the data as any desired format such as TIFF, JPEG, PNG, or ENVI.
;
;
; :Keywords:
;    INPUT_RASTER: in, required, type=ENVIRaster
;      Specify the input raster that you want to perform band-band alignment on.
;    INPUT_BANDALIGNMENTTIEPOINTS: in, required, type=BandAlignmentTiePoints
;      Specify the BandAlignmentTiePoints object that contains the tie points generated from
;      the procedure ```idl BandAlignment_GenerateReferenceTiePoints```.
;    OUTPUT_SPATIALREF, out, required, type=ENVISpatialReference
;      Contains the output spatial reference for the band-band aligned datasets.
;    OUTPUT_DATA_POINTER: out, requried, type=pointer
;      This will contain a pointer that references the data that is ready to 
;      be written to disk. The pointer can be accessed with the asterisk to
;      extract the data. i.e.:
;      
;          dat = *output_data_pointer
;      
;      This pointer is used to minimize the amount of memory that is consumed. 
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro BandAlignment_ApplyReferenceTiePointsWithIDL,$
  BAND_DATA_POINTERS = band_data_pointers,$
  INPUT_RASTER = input_raster,$
  INPUT_BANDALIGNMENTTIEPOINTS = input_BandAlignmentTiePoints,$
  OUTPUT_SPATIALREF = output_spatialref,$
  OUTPUT_DATA_POINTER = output_data_pointer
  compile_opt idl2, hidden
  
  ;validate our input
  if ~isa(input_BandAlignmentTiePoints, 'BandAlignmentTiePoints') then begin
    message, 'INPUT_BANDALIGNMENTTIEPOINTS is not a valid BandAlignmentTiePoints object!'
  endif
  if ~isa(input_raster, 'ENVIRASTER') then begin
    message, 'INPUT_RASTER is not a valid ENVIRASTER object!'
  endif
  
  ;get number of bands
  nBands = input_raster.NBANDS
  nPtrs = n_elements(band_data_pointers)
  
  ;make sure that we have RST, otherwise calculate it
  if ~input_BandAlignmentTiePoints.RST_CALCULATED then begin
    input_BandAlignmentTiePoints.GenerateRSTTransform, input_raster
  endif
  
  ;extract information from our tie points
  dims = input_BandAlignmentTiePoints.RST_OUTPUT_DIMENSIONS
  rst = input_BandAlignmentTiePoints.RST_TRANSFORMS
  subs = input_BandAlignmentTiePoints.RST_SUB_RECT

  ;check if we were already provided with our data
  if (nPtrs eq nBands) then begin
    ;preallocate an array to hold our data for warping
    ;get the typecode from our input raster by reading a single pixel
    write_data = make_array(dims[0], dims[1], nbands, $
      TYPE = ((*band_data_pointers[0])[0]).TYPECODE, /NOZERO)
  endif else begin
    ;preallocate an array to hold our data for warping
    ;get the typecode from our input raster by reading a single pixel
    write_data = make_array(dims[0], dims[1], nbands, $
      TYPE = (input_raster.GetData(BANDS = 0, SUB_RECT = [0,0,1,1])).TYPECODE, /NOZERO)
  endelse

  ;register all our bands!
  for i=0,nbands-1 do begin
    ;get the RST transform
    xy1 = *rst[i]
    
    ;fill our data array with values
    ;check if we were already provided with our data
    if (nPtrs eq nBands) then begin
      write_data[*,*,i] = interpolate(*band_data_pointers[i], xy1[0,*], xy1[1,*])
    endif else begin
      write_data[*,*,i] = interpolate(input_raster.GetData(BANDS = i), xy1[0,*], xy1[1,*])
    endelse
  endfor

  ;create a pointer to our image data
  output_data_pointer = ptr_new(write_data, /NO_COPY)

  ;make a spatial reference for our data - this get's within 1/5 of a pixel and
  ;is good enough
  subset = ENVISubsetRaster(input_raster, SUB_RECT = [subs[0],subs[2],subs[1], subs[3]])

  ;get our spatialreference use ENVIHydrate so that we have an
  ;object that does not below to the virtual raster
  ;this keeps it alive when we close the virtual raster
  output_spatialref = ENVIHydrate(subset.SPATIALREF.dehydrate())

  ;close our subset
  subset.close
end
