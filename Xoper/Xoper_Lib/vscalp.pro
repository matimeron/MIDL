Function Vscalp, rf, rs

;+
; NAME:
;	VSCALP
; PURPOSE:
;	Finds the scalar product of two vectors.
; CATEGORY:
;	Geometry, General.
; CALLING SEQUENCE:
;	Result = VSCALP ( RF, RS)
; INPUTS:
;    RF, RS
;	Vectors, numeric, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the value of the scalar product.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Vectors must have same length.
; PROCEDURE:
;	Straightforward.  Uses TYPE from MIDL.
;	Works also with complex vectors.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;-

    on_error, 1
    if n_elements(rf) ne n_elements(rs) then message, 'Unequal lengths!'

    if Type(rf) ne 6 or Type(rs) ne 6 then return, total(rf*rs) $
    else return, total(conj(rf)*rs)
end
