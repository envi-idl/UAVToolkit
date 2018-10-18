;+
;  
;  Exact same routine as XML_Parse, but this code is at least 80x faster
;  with XML files due to a one-line change that uses strtrim() rather
;  than strcompress(). 
;  
;  This modification was made because it would take 2.5 seconds to parse
;  a 100KB ENVIROI XML file with only about 5 entries present.
;
; :Author: Zachary Norman - GitHub: [znorman-harris](https://github.com/znorman-harris)
;-

;----------------------------------------------------------------------------
; _awesomeXMLParse_ParseElement_XMLDOMError, filename, line, column, msg
; Blank error handler so that the Dom does not freeze when encountering an error
pro _awesomeXMLParse_ParseElement_XMLDOMError, filename, line, column, msg
  compile_opt idl2, hidden
end

;----------------------------------------------------------------------------
; _awesomeXMLParse_ParseElement
; Parses an XML document and saves it in an IDL hash
;
function _awesomeXMLParse_ParseElement, oElement, isUTF8, IGNORE=ignore
  compile_opt idl2, hidden

  on_error, 2

  ; Define special tags
  attrTag = '%'
  valueTag = '#text'

  ; Get name of node
  nodeName = oElement->GetNodeName()
  ; Ignore comments
  if (Strlowcase(nodeName) eq '#comment') then begin
    return, !NULL
  endif
  ; Ignore ignore
  if (N_ELements(ignore) ne 0) then begin
    if (Total(Strupcase(nodeName) eq ignore) ne 0) then begin
      return, !NULL
    endif
  endif
  ; If the element is a text tag, return
  if (Isa(oElement, 'IDLffXMLDOMText')) then begin
    return, !NULL
  endif

  results = orderedhash()

  ; Get attributes
  oAttributeList = oElement->GetAttributes()
  nAttributes = Obj_Valid(oAttributeList) ? oAttributeList->GetLength() : 0
  if (nAttributes ne 0) then begin
    ; Store attributes in a hash
    attrs = orderedhash()
    for i=0,nAttributes-1 do begin
      oAttribute = oAttributeList->Item(i)
      attrName = oAttribute->GetName()
      attrValue = oAttribute->GetValue()
      if (isUTF8) then begin
        attrValue = I18N_UTF8TOMULTIBYTE(attrValue)
      endif
      results[attrTag+attrName] = attrValue
    endfor
  endif

  ; Get children
  children = orderedhash()
  oNodeList = oElement->GetChildNodes()
  nNodes = Obj_Valid(oNodeList) ? oNodeList->GetLength() : 0
  for i=0,nNodes-1 do begin
    oNode = oNodeList->Item(i)
    ; If the element is a text tag it could be a value
    if (Isa(oNode, 'IDLffXMLDOMText')) then begin
      value = oNode.GetNodeValue()
      if (isUTF8) then begin
        value = I18N_UTF8TOMULTIBYTE(value)
      endif
      ; Check if the value is composed of only tabs, cr's, and lf's
      tempVal = value
      tempVal = tempVal.Replace(String(10b), '')
      tempVal = tempVal.Replace(String(13b), '')
      tempVal = tempVal.Replace(String(9b), '')
      if (strtrim(tempVal,2) ne '') then begin
        ; If we have other items in the tag then denote it with the valueTag,
        ; otherwise just store the value of the tag
        if (results.Count() eq 0) then begin
          results = value
        endif else begin
          results[valueTag] = value
        endelse
      endif
      continue
    endif
    ; Child is not pcdata, check to see if it is a valid node
    childName = oNode->GetNodeName()
    if (isUTF8) then begin
      childName = I18N_UTF8TOMULTIBYTE(childName)
    endif
    ; Ignore comments
    if (Strlowcase(childName) eq '#comment') then begin
      continue
    endif
    ; Get whatever is in the child node
    value = _awesomeXMLParse_ParseElement(oNode, isUTF8, IGNORE=ignore)
    if (N_Elements(value) eq 0) then begin
      continue
    endif
    ; If we have a duplicate child node name then put items in a list, else
    ; put the value into the current child hash
    if (children.HasKey(childName)) then begin
      if (Isa(children[childName], 'LIST')) then begin
        children[childName].Add, value
      endif else begin
        temp = children[childName]
        children[childName] = List([temp, value], /EXTRACT)
      endelse
    endif else begin
      children[childName] = value
    end
  endfor
  ; Move things from the children into the current level results
  if (children.Count() ne 0) then begin
    childnames = children.Keys()
    for i=0,children.Count()-1 do begin
      results[childnames[i]] = children[childnames[i]]
    endfor
  endif

  return, N_Elements(results) eq 0 ? !NULL : results
end

;----------------------------------------------------------------------------
; awesomeXMLParse
; Converts an XML file into a Hash/List IDL variable
; The IGNORE keyword can be set to a string array denoting XML tags that
;   should not be parsed.
;
function awesomeXMLParse, input, IGNORE=ignoreIn
  compile_opt idl2, hidden

  on_error, 2

  if (~Isa(input, /STRING) || (N_Elements(input) ne 1)) then begin
    message, 'Input must be a scalar string.'
    return, !NULL
  endif
  if (File_Test(input, /READ)) then begin
    filename = input
  endif else begin
    strInput = input
  endelse

  ; Set up the ignore variable
  if (N_Elements(ignoreIn) ne 0) then begin
    if (~Isa(ignoreIn, /STRING)) then begin
      message, 'Ignore values must be strings.'
      return, !NULL
    endif
    ignore = Strupcase(ignoreIn)
  endif

  ; Check to see if the encoding is UTF-8
  ; Read file as byte, then convert to ASCII.
  isUTF8 = 0b
  if (Isa(filename)) then begin
    catch, error
    if (error ne 0) then begin
      catch, /CANCEL
      if (N_Elements(unit) gt 0) then begin
        Free_Lun, Temporary(unit)
      endif
      message, /REISSUE_LAST
    endif

    OpenR, unit, filename, /GET_LUN
    fInfo = FStat(unit)
    headerSize = fInfo.size < 250
    header = BytArr(headerSize, /NOZERO)
    ReadU, unit, header
    Free_Lun, unit
    header = StrUpCase(header)
    start = Strpos(header, 'ENCODING="')
    if (start ne -1) then begin
      finish = Strpos(header, '"', start+10)
      enc = Strmid(header, start+10, finish-(start+10))
      if ( (enc eq 'UTF-8') && (!version.os_family EQ 'Windows') ) then begin
        isUTF8 = 1b
      endif
    endif
  endif

  catch, error
  if (error ne 0) then begin
    catch, /CANCEL
    if (Obj_Valid(oDoc)) then begin
      Obj_Destroy, oDoc
    endif
    if (Isa(filename)) then begin
      message, 'Filename must be a valid XML file: '+filename
    endif else begin
      message, 'Input must be valid XML.'
    endelse
  endif

  ; Parse the document
  oDoc = IDLffXMLDOMDocument(FILENAME=filename, /QUIET, SCHEMA_CHECKING=0, $
    STRING=strInput, $
    VALIDATION_MODE=0, $
    MSG_ERROR='_awesomeXMLParse_ParseElement_XMLDOMError')
  if (~Obj_Valid(oDoc)) then begin
    message ; This will get caught above
    return, !NULL
  endif

  ; Get the first element
  oElement = oDoc->GetDocumentElement()
  if (~Obj_Valid(oElement)) then begin
    message ; This will get caught above
    return, !NULL
  endif

  ; Get name of node
  nodeName = oElement->GetNodeName()
  if (isUTF8) then begin
    nodeName = I18N_UTF8TOMULTIBYTE(nodeName)
  endif
  ; Ignore ignore
  if (N_ELements(ignore) ne 0) then begin
    if (Total(Strupcase(nodeName) eq ignore) ne 0) then begin
      catch, /cancel
      message, 'No valid items found in the file: '+filename
      return, !NULL
    endif
  endif

  ; Parse the file
  value = _awesomeXMLParse_ParseElement(oElement, isUTF8, IGNORE=ignore)

  if (N_Elements(value) eq 0) then begin
    Obj_Destroy, oDoc
    catch, /cancel
    if (Isa(filename)) then begin
      message, 'No valid items found in the file: '+filename
    endif else begin
      message, 'No valid items found in the input.'
    endelse
    return, !NULL
  endif

  ; Clean up
  Obj_Destroy, oDoc

  ; Save the results
  results = orderedhash()
  results[nodeName] = value

  return, results
end
