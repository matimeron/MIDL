Function Make_plane, lv, dv, name = nam, idnum = id

;+
; NAME:
;	MAKE_PLANE
; PURPOSE:
;	Generates a GELEM structure representing a plane.  For description of 
;	a GELEM structure, see routine MAKE_ELEM.
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = MAKE_PLANE ( LV, DV [, keywords])
; INPUTS:
;    LV
;	3 dimensional vector, yields the LOC field of the plane.
;    DV
;	3 dimensional vector, yields the DIR field of the plane.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    NAME
;	Accepts a character string that is used as the NAM field of the plane.
;	Default name is "Plane".
;    IDNUM
;	Accepts a number that is used as the IDN field of the plane.
;	Default value is 1.
; OUTPUTS:
;	Returns a GELEM structure representing a plane passing through LV, and
;	orthogonal to DV.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	If DV is a zero vector, an "Undefined" GELEM is returned instead of a 
;	plane.
; PROCEDURE:
;	Call to MAKE_ELEM (in XOPER_LIB).
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added keyword IDNUM.
;-

    on_error, 1
    if n_elements(lv) ne 3 or n_elements(dv) ne 3 then message, 'Not vectors!'

    return, Make_elem(2, lv, dv, name = nam, idnum = id)
end
