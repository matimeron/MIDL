Function Make_point, lv, name = nam, idnum = id

;+
; NAME:
;	MAKE_POINT
; PURPOSE:
;	Generates a GELEM structure representing a point.  For description of 
;	a GELEM structure, see routine MAKE_ELEM.
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = MAKE_POINT ( LV [, keywords])
; INPUTS:
;    LV
;	3 dimensional vector, yields the LOC field of the point.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    NAME
;	Accepts a character string that is used as the NAM field of the point.
;	Default name is "Point".
;    IDNUM
;	Accepts a number that is used as the IDN field of the point.
;	Default value is 1.
; OUTPUTS:
;	Returns a GELEM structure representing the point LV.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Call to MAKE_ELEM (in XOPER_LIB).
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added keyword IDNUM.
;-

    on_error, 1
    if n_elements(lv) ne 3 then message, 'Not a vector!'

    return, Make_elem(0, lv, fltarr(3), name = nam, idnum = id)
end
