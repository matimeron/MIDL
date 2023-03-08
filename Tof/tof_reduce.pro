Pro Tof_reduce, z, p, optic = opt, show = sho

;+
; NAME:
;	TOF_REDUCE
; PURPOSE:
;	Converts lengths and potentials into internal TOF variables.
; CATEGORY:
;	TOF specific.
; CALLING SEQUENCE:
;	TOF_REDUCE, Z, P [, keywords]
; INPUTS:
;    Z
;	Vector.  TOF grid coordinates.
;    P
;	Vector.  TOF potentials.  Must be same length as Z.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    OPTIC
;	See corresponding parameter in TOF_COMM.
;    SHOW
;	Ditto
; OUTPUTS:
;	None.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Normalization and call to TOF_COMM.  Also uses CAST from MIDL.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    on_error, 1
    ns = n_elements(z)
    if n_elements(p) ne ns then message, 'Dimension mismatch!'
    x = [0, Cast(z,4)/z(ns-1)]
    y = [0, sqrt(Cast(p,4)/p(ns-1))]
    Tof_comm, x, y, optic = opt, show = sho

    return
end
