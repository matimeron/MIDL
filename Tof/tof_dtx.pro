Function Tof_dtx

;+
; NAME:
;	TOF_DTX
; PURPOSE:
;	Calculates the main part of time dispertion in TOF.
; CATEGORY:
;	TOF specific.
; CALLING SEQUENCE:
;	Result = TOF_DTX()
; INPUTS:
;	None.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the main (space dependent) part of time dispertion in TOF.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	Block TOF_VARS.  See TOF_COMM for details.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Common block TOF_VARS must be initialized before calling TOF_DTX.
; PROCEDURE:
;	STraightforward formula application.
; MODIFICATION HISTORY:
;	Created by Mati Meron.
;-

    common Tof_vars, x, y, dex, dey, ns, eps, alph, beth, opmat, flag, splready

    on_error, 1
    tem = (y(1)^2/alph(1))*(alph(2:ns)/(y(2:ns)*y(1:ns-1)))

    return, 0.5*(1 - total(tem))
end
