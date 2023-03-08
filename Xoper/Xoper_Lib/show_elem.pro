Pro Show_elem, elem, nospace = nosp

;+
; NAME:
;	SHOW_ELEM.
; PURPOSE:
;	Displays the fields of ELEM, where ELEM is a geometrical element 
;	represented by a structure of type GELEM (see routine MAKE_ELEM).
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	SHOW_ELEM, ELEM [,/NOSPACE]
; INPUTS:
;    ELEM
;	A GELEM structure.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    NOSPACE
;	Switch, if set supresses the printing of a blank line at the beginning.
; OUTPUTS:
;	None.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Calls TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added ID number.
;-

    on_error, 1
    if Type(elem) ne 8 then message, 'Improper element!'

    eltype = ['undefined', 'point', 'line', 'plane']
    if not keyword_set(nosp) then print
    print, format = '("Name:",t16,a,t48,"| ID#:",i)', elem.nam, elem.idn
    print, format = '("Type:",t16,a)', eltype(elem.dim + 1)
    print, 'Location:	', elem.loc
    print, 'Direction:	', elem.dir

    return
end
