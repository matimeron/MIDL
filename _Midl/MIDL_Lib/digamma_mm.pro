Function Digamma_mm, n

;+
; NAME:
;		DIGAMMA_MM
; VERSION:
;		8.2
; PURPOSE:
;		Calculates the digamma function, for integer values.
; CATEGORY:
;		Mathematical function (general).
; CALLING SEQUENCE:
;		Result = DIGAMMA_MM( N)
; INPUTS:
;	N
;		Numeric, positive, of integer type, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 		None.
; OUTPUTS:
;		Returns the digamma function of N.  Output always of type DOUBLE.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		N must be of integer type and >0.
; PROCEDURE:
;		Straightforward from definition.  Calls ISNUM from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JAN-2013 by Mati Meron.
;-

	on_error, 1
	eucon = 0.5772156649015329d

	if Isnum(n,/int) then begin
		if n gt 0 then begin
			res = - eucon
			if n gt 1 then res = res + total(1d/(1d + dindgen(n-1)))
		endif else message, 'N must be positive!'
	endif else message, 'N must be an integer!

	return, res
end