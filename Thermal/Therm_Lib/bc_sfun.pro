Function BC_sfun, x, lam

;+
; NAME:
;		BC_SFUN
; VERSION:
;		0.9
; PURPOSE:
;		"Feed" function to the BC_ROOTS routine.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result = BC_SFUN( X, LAM)
; INPUTS:
;	X
;		Numeric, otherwise arbitrary.
;	LAM
;		Numeric, scalar.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the value of x*sin(x) - lam*cos(x).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.
; MODIFICATION HISTORY:
;		Created FEB-1-2000 by Mati Meron.
;-

	on_error, 1

	return, x*sin(x) - lam*cos(x)
end