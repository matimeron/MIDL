Function Plane_linpo, lin, pt, name = nam, idnum = id

;+
; NAME:
;	PLANE_LINPO
; PURPOSE:
;	Generates a GELEM structure representing the plane that's passing 
;	through the line LIN and the point PT.  For a description of a GELEM 
;	structure, see routine MAKE_ELEM.
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = PLANE_LINPO ( LIN, PT [, keywords] )
; INPUTS:
;    LIN
;	A GELEM structure representing a line.
;    PT
;	Either a 3 dimensional vector or a GELEM structure representing a point
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
;	Returns a GELEM structure representing a plane passing through both LIN
;	and PT.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	If PT is a point on LIN, an "Undefined" GELEM is returned instead of a 
;	line.
; PROCEDURE:
;	Calls MAKE_PLANE (in XOPER_LIB).  Also uses TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added keyword IDNUM.
;-

    on_error, 1
    if not (Type(lin) eq 8 and lin.dim eq 1) then message, 'Not a line!'
    if Type(pt) lt 8 then tpt = pt else $
    if pt.dim eq 0 then tpt = pt.loc else message, 'Not a point!'

    return, Make_plane(tpt, crossp(lin.loc-tpt, lin.dir), name= nam, idnum= id)
end
