Function Project_elem, elem, refelem, complementary = cmp, name= nam, idnum= id

;+
; NAME:
;	PROJECT_ELEM.
; PURPOSE:
;	Projects a geometrical element ELEM into the subspace perpendicular 
;	to (or parallel to) the plane REFELEM.  Both ELEM and REFELEM are GELEM
;	structures (see routine MAKE_ELEM).
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = PROJECT_ELEM(ELEM, REFELEM [, keywords])
; INPUTS:
;    ELEM
;	A GELEM structure
;    REFELEM
;	A GELEM structure representing a plane.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    COMPLEMENTARY
;	If set, the projection is on the plane, otherwise it is normal to the
;	plane.
;    NAME
;	Accepts a character string that is used as the NAM field of the result.
;	Default is the name-type of ELEM.
;    IDNUM
;	Accepts a number that is used as the IDN field of the result.
;	Default value is 1.
; OUTPUTS:
;	Returns a GELEM structure representing the result of applying the 
;	projection to ELEM.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	REFELEM must be a plane (i.e. refelem.dim = 2).
; PROCEDURE:
;	Calls PROJECT_OPER and MAKE_ELEM (in XOPER_LIB).  Also calls TYPE from
;	MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added keyword IDNUM.
;-

    on_error, 1
    if Type(elem) ne 8 or Type(refelem) ne 8 then message, 'Improper elements!'
    if refelem.dim ne 2 then message, 'Reference must be a plane!'

    dumel = elem
    oper = Project_oper(refelem.dir, complementary = cmp)
    dumel.loc = oper#(dumel.loc - refelem.loc) + refelem.loc
    dumel.dir = oper#dumel.dir

    return, Make_elem(dumel, name = nam, idnum = id)
end
