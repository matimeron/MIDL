Function Xlax, x

;+
; NAME:
;		XLAX
; VERSION:
;		8.492
; PURPOSE:
;		Calculates X*LOG(ABS(X)).
; CATEGORY:
;		Mathematical function.
; CALLING SEQUENCE:
;		Result = XLAX(X)
; INPUTS:
;	X
;		Numeric, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the values of X*LOG(ABS(X)).  For X=0 returns 0.
;		The result is of the same form and type as the input (but no lower than
;		FLOAT.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Straightforward.  Calls FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2017 by Mati Meron.
;-

	on_error, 1

	res = 0.*x
	dum = where(x ne 0, ndum)
	if ndum gt 0 then res[dum] = x[dum]*alog(abs(x[dum]))

	return, FPU_fix(res)
end