Function Make_line, lv, dv, name = nam, idnum = id

;+
; NAME:
;	MAKE_LINE
; PURPOSE:
;	Generates a GELEM structure representing a line.  For description of 
;	a GELEM structure, see routine MAKE_ELEM.
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = MAKE_LINE ( LV, DV [, keywords])
; INPUTS:
;    LV
;	3 dimensional vector, yields the LOC field of the line.
;    DV
;	3 dimensional vector, yields the DIR field of the line.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    NAME
;	Accepts a character string that is used as the NAM field of the line.
;	Default name is "Line".
;    IDNUM
;	Accepts a number that is used as the IDN field of the line.
;	Default value is 1.
; OUTPUTS:
;	Returns a GELEM structure representing a line passing through LV, in 
;	the direction of DV.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	If DV is a zero vector, an "Undefined" GELEM is returned instead of a 
;	line.
; PROCEDURE:
;	Call to MAKE_ELEM (in XOPER_LIB).
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added keyword IDNUM.
;-

    on_error, 1
    if n_elements(lv) ne 3 or n_elements(dv) ne 3 then message, 'Not vectors!'

    return, Make_elem(1, lv, dv, name = nam, idnum = id)
end
