Function Tof_wpoint, ty, epars

;+
; NAME:
;	TOF_WPOINT
; PURPOSE:
;	Called through ROOT to find optimal focus or image point.
; CATEGORY:
;	TOF specific.
; CALLING SEQUENCE:
;	Result = TOF_WPOINT(TY, EPARS)
; INPUTS:
;    TY
;	Numerical, value of Y(2).
;    EPARS
;	Two-element vector of the format [Element index, X-eval], where element
;	index of 0 selects focus, 1 selects image.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the value of the selected optical elements for Y(1), Y(2),
;	where Y(1) is calculated from Y(2) to correspond to a minimal time
;	dispertion.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TOF_VARS and TOF_EXT.
; SIDE EFFECTS:
;	Both common blocks are updated.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Calculates Y(1) using a call to SPLIN_EVAL with the spline coefficients
;	TYSPL for the minimum time dispertion curve.  Then calculates the 
;	selected optical element through a call to TOF_OPT.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready
    common Tof_ext, ysmax, nspl, yspl, ntspl, tyspl

    on_error, 1
    if ns lt 4 then message, 'Small TOF, routine not applicable!'
    if not splready then message, 'Run TOF_TCURV first, to create splines!'

    y(1:2) = [Splin_eval(ty,tyspl),ty]

    return, (Tof_opt(epars(1), /new))(epars(0))
end
