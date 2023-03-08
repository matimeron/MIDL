Function Plp_solv_kern, x, par

;+
; NAME:
;		PLP_SOLV_KERN
; VERSION:
;		4.8
; PURPOSE:
;		Kernel function to be used with ROOT.
; CATEGORY:
;		Mathematical (for x-ray purposes).
; CALLING SEQUENCE:
;		Result = PLP_SOLV_KERN( X, PAR)
; INPUTS:
;	X
;		Numeric, otherwise arbitrary.
;	PAR
;		A two element numeric vector.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the value(s) of PAR[0] - X*exp(-PAR[1]*X)
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;		Created 1-MAR-2004 by Mati Meron.
;-

	on_error, 1

	return, FPU_fix(par[0] - x*exp(-par[1]*x))
end