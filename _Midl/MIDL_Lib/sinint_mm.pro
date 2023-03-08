Function Sinint_mm, x

;+
; NAME:
;		SININT_MM
; VERSION:
;		8.2
; PURPOSE:
;		Calculates the Sine integral, /Integral_0^x{sin(t)/t}
; CATEGORY:
;		Mathematical function (general).
; CALLING SEQUENCE:
;		Result = SININT_MM (X)
; INPUTS:
;	X
;		Numeric, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 		None.
; OUTPUTS:
;		Returns the value(s) of the Sine integral of X.  Output type and form
;		are identical to those of the input (but output type is never lower
;		than FLOAT).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		X must be real.
; PROCEDURE:
; 		Straightforward evaluation using the exponential integral fuction.
;		Calls ABS_MM, CAST, EXPINT_MM, IMAGINARY_MM, SIGN and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JAN-2013 by Mati Meron.
;-

	on_error, 1

	wx = Abs_mm(double(x))
	res = 0*wx
	dum = where(wx ne 0,ndum)
	if ndum gt 0 then res[dum] = !dpi/2 + $
		Imaginary_mm(Expint_mm(dcomplex(0,wx[dum]),1))

	return, Sign(x)*Cast(res,4,Type(x),/fix)
end