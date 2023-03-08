Function Tof3_opcurv, ty, opars

;+
; NAME:
;	TOF3_OPCURV
; PURPOSE:
;	Called through ROOT to find values of Y(1) which yield focal or image
;	points for TOF3.
; CATEGORY:
;	TOF3 specific.
; CALLING SEQUENCE:
;	Result = TOF3_OPCURV ( TY [, OPARS)
; INPUTS:
;    TY
;	Value of Y(1).
; OPTIONAL INPUT PARAMETERS:
;    OPARS
;	Parameter array, including:
;	    OPARS(0) - Index of the desired optical element, 0 for focus 
;	    (default), 1 for image.
;	    OPARS(1) - X value for optical matrix evaluation.  Default is 1.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the value of the selected optical element using the provided 
;	values of Y and XEV (through OPARS).
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	TOF_VARS
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	EPARS(0) must be one of [0, 1, 2, 3]
;	EPARS(1) must be in the [0,1] range.
; PROCEDURE:
;	Calculates the value of X(2), for the provided Y(1) and known X(1),
;	then calls TOF_OPT.  Uses DEFAULT from MIDL.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready

    on_error, 1
    if ns ne 3 then message, 'Not a 3 element TOF!'

    wpars = [0, 1]
    np = n_elements(opars)
    if np gt 0 then wpars(0:np-1) = opars
    dty = double(ty)
    y(1) = ty
    x(2) = dty*(1. + dty)/((2. + dty)*(1. - dty)) $
	    *(2.*x(1)*(1. + dty*(1. + dty))/(dty^3*(1. + dty)) - 1.)
    dum = check_math(0,1)

    return, (Tof_opt(wpars(1), /new))(wpars(0))
end
