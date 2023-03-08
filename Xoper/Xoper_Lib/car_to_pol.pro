Function Car_to_pol, vec

;+
; NAME:
;	CAR_TO_POL
; PURPOSE:
;	Converts a 3 dimensional cartesian vector to polar coordinates.
; CATEGORY:
;	Geometry, 3 dimensional.
; CALLING SEQUENCE:
;	Result = CAR_TO_POL (VEC)
; INPUTS:
;    VEC
;	Vector, numeric, 3 dimensional.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the vector in an (R, Theta, Phi) form.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Only 3 dimensional vectors accepted.
; PROCEDURE:
;	Straightforward.  Uses VNORM from XOPER_LIB and TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;-

    on_error, 1
    if n_elements(vec) ne 3 then message, 'Not a 3-D vector!'

    pvec = make_array(3, type = Type(vec) > 4)
    pvec(0) = Vnorm(vec)
    if pvec(0) ne 0 then begin
	pvec(1) = acos(vec(2)/pvec(0))
	if vec(0) ne 0 or vec(1) ne 0 then pvec(2) = atan(vec(1),vec(0))
    endif

    return, pvec
end
