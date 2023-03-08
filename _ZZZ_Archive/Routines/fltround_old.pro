Function Fltround_old, x, digits = dig, floor = flo, ceil = cei

;+
; NAME:
;		FLTROUND
; VERSION:
;		8.2
; PURPOSE:
;		Rounding floating numbers to a prescribed number of significant figures.
; CATEGORY:
;		Mathematical Function (General).
; CALLING SEQUENCE:
;		Result = FLTROUND( X [, DIGITS = DIG])
; INPUTS:
;	X
;		Numerical, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	DIGITS
;		Accepts an integer value specifying the number of digits to consider.
;		Default value is 1 digit.
;	/FLOOR															| Only one
;		Switch.  If set, the operation is FLOOR instead of rounding.| of these
;	/CEILING														| two may
;		Switch.  If set, the operation is CEIL instead of rounding.	| be set.
; OUTPUTS:
;		Returns X "rounded" to a specified number of significant places.
;		For example, the return for X = 0.671 will be 0.7 for DIGITS = 1
;		(default) and 0.67 for DIGITS = 2, while comparable results for
;		X = -0.671 will be -0.7 and -0.67, respectively.
;
;		If /FLOOR or /CEIL are set, the rounding is in a fashion akin to this of
;		the IDL functions FLOOR and CEIL for integers.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		For complex X the "rounding" applies to the real part only, while the
;		imaginary part is being zeroed.
; PROCEDURE:
;		Straightforward.  Calling DEFAULT and ONE_OF from MIDL.
; MODIFICATION HISTORY:
;		Created 5-MAY-2008 by Mati Meron.
;		Modified 20-JAN-2013 by Mati Meron.  Internal change.
;-

	on_error, 1

	dig = Default(dig,1,/dtyp) > 1
	whi = One_of(flo,cei)
	res = x
	noz = where(x ne 0, nnoz)
	if nnoz gt 0 then begin
		fac = 10.^(1 - dig + floor(alog10(abs(x[noz]))))
		case whi of
			-1	:	res[noz] = fac*round(x[noz]/fac,/l64)
			0	:	res[noz] = fac*floor(x[noz]/fac,/l64)
			1	:	res[noz] = fac*ceil(x[noz]/fac,/l64)
		endcase
	endif

	return, res
end