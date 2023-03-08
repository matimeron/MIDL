Function Reflect_elem, elem, refelem, name = nam, idnum = id

;+
; NAME:
;	REFLECT_ELEM.
; PURPOSE:
;	Reflects the geometrical element ELEM in the plane REFELEM.  Both ELEM 
;	and REFELEM are GELEM structures (see routine MAKE_ELEM).
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = REFLECT_ELEM ( ELEM, REFELEM [, keywords] )
; INPUTS:
;    ELEM
;	A GELEM structure
;    REFELEM
;	A GELEM structure representing a plane.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    NAME
;	Accepts a character string that is used as the NAM field of the result.
;	Default is the name-type of ELEM.
;    IDNUM
;	Accepts a number that is used as the IDN field of the result.
;	Default value is 1.
; OUTPUTS:
;	Returns a GELEM structure representing the result of applying the 
;	reflection to ELEM.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	REFELEM must be a plane (i.e. refelem.dim = 2).
; PROCEDURE:
;	Calls REFLECT_OPER and MAKE_ELEM (in XOPER_LIB).  Also calls TYPE from
;	MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added keyword IDNUM.
;-

    on_error, 1
    if Type(elem) ne 8 or Type(refelem) ne 8 then message, 'Improper elements!'
    if refelem.dim ne 2 then message, 'Reference must be a plane!'

    dumel = elem
    oper = Reflect_oper(refelem.dir)
    dumel.loc = oper#(dumel.loc - refelem.loc) + refelem.loc
    dumel.dir = oper#dumel.dir

    return, Make_elem(dumel, name = nam, idnum = id)
end
