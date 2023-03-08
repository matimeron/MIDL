Function Translate_elem, elem, disvec, mult, name = nam, idnum = id

;+
; NAME:
;	TRANSLATE_ELEM.
; PURPOSE:
;	TRANSLATES a geometrical element ELEM in the direction of DISVEC.  
;	ELEM (and possibly DISVEC) is a GELEM structure (see MAKE_ELEM).
; CATEGORY:
;	Geometry, specialized, 3 dimensional.
; CALLING SEQUENCE:
;	Result = TRANSLATE_ELEM ( ELEM, DISVEC, MULT, [, keywords])
; INPUTS:
;    ELEM
;	A GELEM structure.
;    DISVEC
;	Either a 3 dimensional vector or a geometrical element (GELEM) 
;	representing a line or plane.  in the second case MULT must be provided
;    MULT
;	Scalar representing the translation distance.  Not needed if DISVEC is
;	a vector, since in this case the distance is the length of DISVEC.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    NAME
;	Accepts a character string that is used as the NAM field of the result.
;	Default is the name-type of ELEM.
;    IDNUM
;	Accepts a character string that is used as the IDN field of the result.
;	Default value is 1.
; OUTPUTS:
;	Returns a GELEM structure representing the result of applying the 
;	translation to ELEM.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	DISVEC cannot be a point.
; PROCEDURE:
;	Calls MAKE_ELEM (in XOPER_LIB).  Also calls TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;	Modified 25-JULY-1992 by Mati Meron.  Added keyword IDNUM.
;-

    on_error, 1
    if Type(elem) eq 8 then begin
	dumel = elem
	if Type(disvec) eq 8 then begin
	    if disvec.dim gt 0 then dumel.loc = dumel.loc + mult*disvec.dir $
	    else message, 'No direction information, can''t translate!'
	endif else dumel.loc = dumel.loc + disvec
    endif else message, 'Not a proper geometric element!'

    return, Make_elem(dumel, name = nam, idnum = id)
end
