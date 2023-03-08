Function Reflect_oper, r

;+
; NAME:
;	REFLECT_OPER
; PURPOSE:
;	Generates the operator corresponding to reflection in plane 
;	perpendicular to R.
; CATEGORY:
;	Geometry, General.
; CALLING SEQUENCE:
;	Result = REFLECT_OPER (R)
; INPUTS:
;    R
;	Vector, numeric, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Matrix representing the reflection operator.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Uses IDENT_OPER AND PROJECT_OPER from XOPER_LIB.
;	Also uses TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;-

    return, Ident_oper(n_elements(r), type = Type(r)) - 2*Project_oper(r)
end
