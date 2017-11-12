;h+
; (c) 2017 Exelis Visual Information Solutions, Inc., a subsidiary of Harris Corporation.
;h-

;+
; :Description:
;    This function searches a directory for files and groups multiple files together
;    based on unique identifiers. Essentially, this function is useful for grouping
;    images together that represent different bands for a dataset if each band is in 
;    a separate file.
;    
;    This routine is also helpful to re-order the bands by increasing/decreasing wavelengths
;    as is demonstrated in the example below.
;    
;  :Example:
;    To search for MicaSense RedEdge files you can simply use the following (5 comes before 4):
;    
;        dir = 'C:\some\directory\000'
;        ids = ['_1.tif', '_2.tif', '_3.tif', '_5.tif', '_4.tif']
;        groups = BandAlignment_Get_Image_Groups(dir, ids)
;        
;    This same routine can be used to search for other data such as Parrot Sequoia:
;
;        dir = 'C:\some\directory\data'
;        ids = ['_GRE.TIF', '_RED.TIF', '_NIR.TIF', '_REG.TIF']
;        groups = BandAlignment_Get_Image_Groups(dir, ids)
;
; :Params:
;    directory: in, required, type=string
;      Specify the directory that you want to search for imagery
;    uniqueIdentifiers: in, requried, type=stringarr
;      Specify the unique parts of each filename that correspond to the identifier
;      for that file. This should be something like the end of the file. See the
;      examples above for a reference.
;
;
;
; :Author: Zachary Norman - Github: znorman-harris
;-
function BandAlignment_Get_Image_Groups, directory, uniqueIdentifiers
  compile_opt idl2, hidden

  ;get the number of bands
  nBands = n_elements(uniqueIdentifiers)

  ;save search results
  files = orderedhash()

  ;init data structure to store the group information
  groups = orderedhash()

  ;find files in the directory
  cd, directory, CURRENT = first_dir
  dirFiles = file_search(COUNT = nFiles)
  cd, first_dir
  
  ;return empty orderedhash
  if (nFiles eq 0) then return, groups

  ;search for files
  foreach id, uniqueIdentifiers, i do begin
    idxMatch = where(strpos(dirFiles, id) ne -1, countMatch)
    
    ;make sure we found results
    if (countMatch eq 0) then return, groups

    ;save complete files
    files[id] = directory + path_sep() + dirFiles[idxMatch]
    
    ;save base names of first matching ID (any works for finding all other groups)
    if (i eq 0) then baseNames = file_basename(dirFiles[idxMatch], id)
  endforeach
  
  ;find the matches for each base name
  foreach base, baseNames do begin
    ;init list to store matches
    matches = list()
    
    ;loop over each list of files
    foreach fileArr, files do begin
      idx = where(strpos(fileArr, base) ne -1, countIdx)
      if (countIdx eq 1) then begin
        matches.Add, fileArr[idx[0]]
      endif
    endforeach
    
    ;validate that we have anough matches
    if (n_elements(matches) eq nBands) then begin
      groups[base] = matches.toArray()
    endif else begin
      print, '  incomplete group "' + base + '", skipping...'
    endelse
  endforeach
  
  ;return group information
  return, groups
end

dir = 'C:\Users\znorman\Documents\data\opportunities\UAS_Multi'
ids = ['_GRE.TIF', '_RED.TIF', '_NIR.TIF', '_REG.TIF']

dir = 'C:\Users\znorman\Documents\data\opportunities\highland\000'
ids = ['_1.tif', '_2.tif', '_3.tif', '_5.tif', '_4.tif']

groups = BandAlignment_Get_Image_Groups(dir, ids)
end
