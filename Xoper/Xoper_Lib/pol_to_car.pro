Function Pol_to_car, vec

;+
; NAME:
;	POL_TO_CAR
; PURPOSE:
;	Converts a 3 dimensional vector from polar to cartesian coordinates.
; CATEGORY:
;	Geometry, 3 dimensional.
; CALLING SEQUENCE:
;	Result = POL_TO_CAR (VEC)
; INPUTS:
;    VEC
;	3 dimensional numeric vector.  Assumed to be in the (R,Theta,Phi) form.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the input vector converted to cartesian coordinates.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Only 3 dimensional vectors accepted.
; PROCEDURE:
;	Straightforward.
; MODIFICATION HISTORY:
;	Created 30-JUNE-1992 by Mati Meron.
;-

    on_error, 1
    if n_elements(vec) ne 3 then message, 'Not a 3-D vector!'

    return, vec(0)*[sin(vec(1))*[cos(vec(2)),sin(vec(2))],cos(vec(1))] 
end
