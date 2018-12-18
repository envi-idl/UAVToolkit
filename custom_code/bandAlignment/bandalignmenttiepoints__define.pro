;h+
; Copyright (c) 2018 Harris Geospatial Solutions, Inc.
; 
; Licensed under MIT. See LICENSE.txt for additional details and information.
;h-

;+
; :Private:
; 
; :Description:
;
;  Object definition for the BandAlignmentTiePoints object. This object uses the ".dot" notation
;  to access and set properties.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-



;+
; :Description:
;    Procedure method that generates the RST transform and creates the subset of the
;    data based on `input_raster` that all of the scenes will have. This information
;    is calculated and stored for faster processing. When applying reference tie 
;    points many times, this information can be saved and used again instead of needing
;    to re-calculate each time.
;
; :Params:
;    input_raster: in, required, type=EVNIRaster
;      The raster that the tie points belong to. Needed to create the mask.
;
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro BandAlignmentTiePoints::GenerateRSTTransform, input_raster
  compile_opt idl2, hidden
  
  ;get tie points
  tie_points = *self.TIE_POINTS

  ;get some info on our input raster
  nbands = input_raster.NBANDS
  ncols = input_raster.NCOLUMNS
  nrows = input_raster.NROWS

  ;get the base and warp bands
  warp = where(ptr_valid(tie_points), nWarp)

  if (nWarp eq 0) then begin
    message, 'No valid tiepoint pointers found. Required to have at least one! Generated correctly?'
  endif

  ;pre-allocate some arrays for in-memory warping and cropping (from Atle)
  rst = ptrarr(nbands)
  tmp = lindgen(1,ncols * nrows)
  xy0 = [tmp mod ncols, tmp / ncols, $
    replicate(1,1,ncols * nrows)]
  mask = replicate(1b, 1, ncols * nrows)

  ;perform band alignment on all images in the image group
  for i=0, nbands-1 do begin
    ;skip if we don't need to warp this band
    if ~ptr_valid(tie_points[i]) then continue

    ;get our tiepoints
    usePoints = transpose(*tie_points[i])
    dims = size(usePoints, /DIMENSIONS)

    ; manually calculate the RST
    aa = transpose([[usePoints[*,0]], $
      [usePoints[*,1]], $
      [replicate(1.0, dims[0])]])
    bb = transpose([[usePoints[*,2]], $
      [usePoints[*,3]], $
      [replicate(1.0, dims[0])]])
    xx = la_least_squares(aa, bb)
    ;save RST calculation
    rst[i] = ptr_new(xx)

    xy1 = matrix_multiply(xx, xy0)
    mask and= xy1[0,*] ge 0
    mask and= xy1[1,*] ge 0
    mask and= xy1[0,*] lt ncols ;npx;compare_image.ncolumns
    mask and= xy1[1,*] lt nrows ;npy;compare_image.nrows
  endfor

  ;preallocate an array to contain the mask
  mask = reform(mask, ncols, nrows, /over)

  ; crop to best subset
  maskdim = size(mask, /dimension)
  subs = [0,maskdim[0]-1,0,maskdim[1]-1]
  h = lonarr(256,4)
  while (subs[1] gt subs[0]) and (subs[3] gt subs[2]) do begin
    h[*,0] = histogram(mask[subs[0]:subs[1],subs[2]])
    h[*,1] = histogram(mask[subs[0]:subs[1],subs[3]])
    h[*,2] = histogram(mask[subs[0],subs[2]:subs[3]])
    h[*,3] = histogram(mask[subs[1],subs[2]:subs[3]])
    if max(h[0,*]) eq 0 then break
    f = 1./rebin([subs[1]-subs[0]+1,subs[3]-subs[2]+1], 4, /sample)
    v = min(h[1,*]*f, direction)
    case direction of
      0: subs[2]++
      1: subs[3]--
      2: subs[0]++
      3: subs[1]--
    endcase
  endwhile

  ;update our mask with where we have pixels turned to "on"
  mask[subs[0]:subs[1],subs[2]:subs[3]] = 2

  ;create default matrix for rotation
  xy0 = [array_indices(mask,where(mask eq 2,count)), $
    replicate(1.0,1,count)]

  ;populate invalid RST transforms with default one about
  idxReplace = where(~ptr_valid(rst), countReplace, COMPLEMENT = idxWarp, NCOMPLEMENT = countWarp)
  if (countReplace gt 0) then foreach idx, idxReplace do rst[idx] = ptr_new(xy0)

  ;do our matrix multiplication for the bands we are going to warp
  if (countWarp gt 0) then foreach idx, idxWarp do rst[idx] = ptr_new(matrix_multiply(*rst[idx], xy0))
  
  ;get the size of the data that has been subsetted
  ns = subs[1]-subs[0]+1
  nl = subs[3]-subs[2]+1

  ;save the output dimensions
  self.RST_OUTPUT_DIMENSIONS = [ns,nl]
  
  ;save output subset
  self.RST_SUB_RECT = subs
  
  ;save the pointer array
  self.RST_TRANSFORMS = ptr_new(rst)
  
  ;set flag that we have RST
  self.RST_CALCULATED = 1
end


;+
; :Description:
;    Get property method that works with the "dot" notation.
;
;
; :Keywords:
;    _REF_EXTRA: in, requried, type=assorted
;     Allowed properties are any object definition parameters.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro BandAlignmentTiePoints::GetProperty, _REF_EXTRA=extra
  compile_opt idl2, hidden
  if ~self.debug then begin
    on_error, 2
  endif

  if (extra ne !NULL) then begin
    ;get self tag names, first three are related to IDL_Object and not needed
    properties = (tag_names(self))[3:*]

    ;check if property we are getting exists
    foreach property, strupcase(extra) do begin
      prop_idx = (where(property eq properties, is_prop))[0]
      ;property exists, return it
      if (is_prop eq 1) then begin
        currentvalue = self.(3 + prop_idx)

        case (1) of
          isa(currentvalue, 'list') OR isa(currentvalue, 'hash'):begin
            (scope_varfetch(property, /REF_EXTRA)) = currentvalue
          end

          (currentValue eq !NULL):begin
            (scope_varfetch(property, /REF_EXTRA)) = !NULL
          end

          isa(currentvalue, 'pointer'):begin
            (scope_varfetch(property, /REF_EXTRA)) = *currentvalue
          end

          else:begin
            (scope_varfetch(property, /REF_EXTRA)) = currentvalue
          end
        endcase
      endif else begin
        message, 'Property "' + property + '" is not a valid object property!'
      endelse
    endforeach
  end
end




;+
; :Description:
;    Init function for the object.
;
; :Params:
;    tie_points: in, optional, type=arr[4,*]
;      If this is not set, then you must set the `SAVE_FILE` keyword. This should be
;      set to the tie points that come out of the bandalignment_generatereferencetiepoints
;      procedure.
;
; :Keywords:
;    SAVE_FILE: in, optional, type=string
;      Se this to an IDL SAVE file on disk that contains a previously saved instance
;      of this object.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
function BandAlignmentTiePoints::init, tie_points, SAVE_FILE = save_file
  compile_opt idl2, hidden

  ;set version for our object
  self.VERSION = '1.0'
  self.RST_CALCULATED = 0
  self.DEBUG = 0

  ;check if we are manually setting properties
  if ~keyword_set(save_file) then begin
    ;validate input
    if ~keyword_set(tie_points) then begin
      message, 'tie_points not specified, requried argument!'
    endif
    
    ;validate dimensions - not needed bc we have pointers
;    dims = size(tie_points, /DIMENSIONS)
;    if (n_elements(dims) gt 2) then begin
;      message, 'tie_points has more than two dimensions, only two allowed of the form [4,n]'
;    endif
;    if (dims[0] ne 4) then begin
;      message, 'First dimension of tie_points is not a four-element array of the form [4,n]'
;    endif
    
    ;save our valid tie points
    self.TIE_POINTS = ptr_new(tie_points)
  endif else begin
    ;validate file
    if ~save_file.endsWith('.sav') then begin
      message, 'SAVE_FILE specified, but does not end with expected ".sav" extension!'
    endif
    
    ;restore
    restore, save_file
    
    ;make sure that we restore what we expect
    if ~isa(bandAlignmentTiePointsObject, 'BandAlignmentTiePoints') then begin
      message, 'SAVE_FILE specified, but does not contain a valid object!'
    endif
    
    ;update self properties
    self.TIE_POINTS = ptr_new(bandAlignmentTiePointsObject.TIE_POINTS)
    self.RST_TRANSFORMS = ptr_new(bandAlignmentTiePointsObject.RST_TRANSFORMS)
    self.RST_OUTPUT_DIMENSIONS = bandAlignmentTiePointsObject.RST_OUTPUT_DIMENSIONS
    self.RST_SUB_RECT = bandAlignmentTiePointsObject.RST_SUB_RECT
    self.RST_CALCULATED = bandAlignmentTiePointsObject.RST_CALCULATED
  endelse
  
  ;valid so return 1
  return, 1
end



;+
; :Description:
;    Wrapper method for overloading implied print which uses the
;    same method as the _overloadPrint method.
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
function BandAlignmentTiePoints::_OverloadImpliedPrint, varname
  return, self->BandAlignmentTiePoints::_overloadPrint()
end



;+
; :Description:
;    Object method for overloading printing and getting some basic information
;    from our crop centers object.
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
function BandAlignmentTiePoints::_OverloadPrint
  tags = tag_names(self)
  sorted = sort(tags[3:*])
  output = list()

  ;determine the number of white spaces we need to add
  maxlen = max(strlen(tags)) + 6

  for k=0,n_elements(sorted)-1 do begin
    i = 3 + sorted[k]

    ;skip other packages
    if isa(self.(i), 'idlpb') then continue
    if isa(self.(i), 'idlpm') then continue
    if isa(self.(i), 'idlpt') then continue

    help, self.(i), OUTPUT=o
    split = strsplit(o, /EXTRACT)
    type = split[1]
    add = ''
    for j=0, maxlen-strlen(tags[i]) do add += ' '
    output.add, string(9b) + strupcase(tags[i]) + add + type

    case 1 of
      isa(self.(i), 'LIST'):begin
        help, self.(i), OUTPUT = o
        type = strjoin((strsplit(o, /EXTRACT))[1:*], ' ')
        output.add, string(9b) + string(9b) + type
      end
      isa(self.(i), 'HASH'):begin
        help, self.(i), OUTPUT = o
        type = strjoin((strsplit(o, /EXTRACT))[1:*], ' ')
        output.add, string(9b) + string(9b) + type
      end
      (self.(i) eq !NULL):begin
        output.add, string(9b) + string(9b) + '!NULL'
      end
      isa(self.(i), 'OBJREF'):begin
        printed = string(self.(i), /IMPLIED_PRINT)
        foreach line, printed do output.add, string(9b) + string(9b) + line
      end
      isa(self.(i), 'POINTER'):begin
        help, *(self.(i)), OUTPUT = o
        split = (strsplit(o, /EXTRACT))[1:-1]
        if isa(split, 'LIST') then split = split.toarray()
        type = strjoin(split[1:*], ' ')
        output.add, string(9b) + string(9b) + type
      end
      else:begin
        output.add, string(9b) + string(9b) + strjoin(strtrim(string(self.(i), /IMPLIED_PRINT),2),'     ')
      end
    endcase
  endfor

  return, strjoin(output.toarray(), string(10b))
end



;+
; :Description:
;    Set property method that works with the ".dot" notation.
;
;
; :Keywords:
;    _REF_EXTRA: in, requried, type=assorted
;      Set keywords that match object properties in the object definition.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro BandAlignmentTiePoints::SetProperty, _REF_EXTRA = extra
  compile_opt idl2, hidden
  on_error, 2

  if (extra ne !NULL) then begin
    ;get self tag names, first three are related to IDL_Object and not needed
    properties = (tag_names(self))[3:*]

    ;check if property we are setting exists
    foreach property, strupcase(extra) do begin
      prop_idx = (where(property eq properties , is_prop))[0]
      ;property exists, set it
      if (is_prop eq 1) then begin
        new_value = scope_varfetch(property, /REF_EXTRA)
        self._SetProperty, property, prop_idx, new_value
      endif else begin
        message, 'Property "' + property + '" is not a valid object property!'
      endelse
    endforeach
  endif
end



;+
; :Description:
;    Underlying set property method for this object so we don't have to have a
;    separate one at init and setproprty
;
; :Params:
;    property: name of property
;    prop_idx: index of property in structure
;    new_value: new value to replace the property with
;
;
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-
pro BandAlignmentTiePoints::_SetProperty, property, prop_idx, new_value
  compile_opt idl2, hidden
  ;check if we have a pointer or not
  currentvalue = self.(3 + prop_idx)
  ;check if we are resetting a certain property which only works for
  ;strings, pointers, and objects otherwise structures are too strict
  ;to try and change the currentvalue
  if (new_value eq !NULL) then begin
    old_value = self.(3 + prop_idx)
    case 1 of
      (old_value.typecode eq 7):  self.(3 + prop_idx) = ''
      (old_value.typecode eq 10): self.(3 + prop_idx) = ptr_new()
      (old_value.typecode eq 11): self.(3 + prop_idx) = obj_new()
      else: BEGIN
        message, 'Cannot set property "' + property + '" to !NULL because the datatype (' + strtrim(type,2) + ') will not allow it!'
      END
    endcase
    ;not setting the variable to !NULL
  endif else begin
    ;special checks for the property that we are setting
    case property of
      else:begin
        ;default set property
        if ISA(currentvalue, 'pointer') then begin
          self.(3 + prop_idx) = ptr_new(new_value)
        endif else begin
          self.(3 + prop_idx) = new_value
        endelse
      endelse
    endcase
  endelse
end


pro BandAlignmentTiePoints__define
  compile_opt idl2, hidden
  
  struct = {BandAlignmentTiePoints, $
      inherits IDL_Object,$
      VERSION:'',$
      DEBUG:1,$
      
      ;ptr to pointer array of tie points for our scene
      TIE_POINTS:ptr_new(),$
      
      ;save information about our RST transform
      RST_CALCULATED:1,$
      RST_SUB_RECT:ulonarr(4),$ ;[xmin, xmax, ymin, ymax] in IDL array coordinates
      RST_TRANSFORMS:ptr_new(), $
      RST_OUTPUT_DIMENSIONS:ulonarr(2) $
    }

end
