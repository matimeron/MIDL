Function Skellam, k, l1, l2

;+
; NAME:
;		SKELLAM
; VERSION:
;		8.72
; PURPOSE:
;		Calculates Skellam Distribution values.  This is a distribution of the
;		differences of values drawn from two discrete Poisson distributions.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result = SKELLAM (K, L1, L2)
; INPUTS:
; 	K
; 		Integer type, otherwise arbitrary.
; 	L1
; 		Scalar.
; 	L2
; 		Scalar.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 		None.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None other than those specified for inputs.
; PROCEDURE:
;		Straightforward, from definition. Call CALCTYPE, CAST, FPU_FIX and
;		ISNUM, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JUL-2014 by Mati Meron.
;		Documented 25-DEC-2020 by Mati Meron.
;-

	on_error, 1

	if Isnum(k,/int) then begin
		if n_elements(l1) eq 1 and n_elements(l2) eq 1 then begin
			typ = Calctype(l1,l2)
			lres = alog(beseli(2*sqrt(l1*l2),k,/double)) + $
			0.5*k*alog(1.*l1/l2) - (l1 + l2)
			res = Cast(exp(lres),4,typ,/fix)
		endif else message, 'L1 nd L2 must be scalars!'
	endif else message, 'K must be an integer!'

	return, FPU_fix(res)
end