Function Project_oper, r, complementary = cmp

;+
; NAME:
;	PROJECT_OPER
; PURPOSE:
;	Generates a projection operator into the subspace parallel (or 
;	perpendicular) to R.
; CATEGORY:
;	Geometry, General
; CALLING SEQUENCE:
;	Result = PROJECT_OPER ( R [,/COMPLEMENTARY )
; INPUTS:
;    R
;	Vector, numeric, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    COMPLEMENTARY
;	If set, the projection operator is into the subspace complementary to
;	R.  Default is the subspace spanned by R.
; OUTPUTS:
;	Returns the projection matrix.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Uses TYPE from MIDL and UNIVEC, IDENT_OPER, from
;	XOPER_LIB.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;-

    on_error, 1
    siz = size(r)
    if siz(0) eq 1 then tr = Univec(r) $
    else if siz(1) eq 1 then tr = Univec(transpose(r)) $
    else message, 'Not a vector!'

    if not keyword_set(cmp) then return, tr#transpose(tr) $
    else return, Ident_oper(n_elements(tr), type = Type(r)) - tr#transpose(tr)
end
