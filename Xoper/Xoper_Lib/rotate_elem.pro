Function Rotate_elem, elem, refelem, tet, degrees = deg, name = nam, idnum = id

;+
; NAME:
;	ROTATE_ELEM.
; PURPOSE:
;	ROTATES the geometrical element ELEM around the line REFELEM.  Both 
;	ELEM and REFELEM are GELEM structures (see routine MAKE_ELEM).
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = ROTATE_ELEM (ELEM, REFELEM, TET [, keywords])
; INPUTS:
;    ELEM
;	A GELEM structure
;    REFELEM
;	A GELEM structure representing a line.
;    TET
;	Rotation angle.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    DEGREES
;	If set, the angle is taken in degrees.  Default is radians.
;    NAME
;	Accepts a character string that is used as the NAM field of the result.
;	Default is the name-type of ELEM.
;    IDNUM
;	Accepts a number that is used as the IDN field of the result.
;	Default value is 1.
; OUTPUTS:
;	Returns a GELEM structure representing the result of applying the 
;	rotation to ELEM.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	REFELEM must be a line (i.e. refelem.dim = 1)	
;	line.
; PROCEDURE:
;	Calls ROTATE_OPER and MAKE_ELEM (in XOPER_LIB).  Also calls TYPE from
;	MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added keyword IDNUM.
;-

    on_error, 1
    if Type(elem) ne 8 or Type(refelem) ne 8 then message, 'Improper elements!'
    if refelem.dim ne 1 then message, 'Reference must be a line!'

    dumel = elem
    oper = Rotate_oper(refelem.dir,tet,degrees = deg)
    dumel.loc = oper#(dumel.loc - refelem.loc) + refelem.loc
    dumel.dir = oper#dumel.dir

    return, Make_elem(dumel, name = nam, idnum = id)
end
