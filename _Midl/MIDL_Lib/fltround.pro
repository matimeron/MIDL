Function Fltround, x, digits = dig, dec_digits = dec, floor = flo, ceil = cei

;+
; NAME:
;		FLTROUND
; VERSION:
;		8.2
; PURPOSE:
;		Rounding floating numbers to a prescribed number of significant digits,
;		or prescribed number of digits after the decimal point.
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
;	DIGITS															| Only one
;		Accepts an integer value specifying the number of digits	| of these 
;		to consider.  Default value is 1 digit.						| two may
;	DEC_DIGITS														| be set.
;		Accepts an integer value specifying the number of digits	| Default is
;		after the decimal point.  Default value is 0 digits.		| DIGITS.
;
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
;		Straightforward.  Calling CALCTYPE, CAST, DEFAULT, ONE_OF and SIGN, 
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 5-MAY-2008 by Mati Meron.
;		Modified 20-JAN-2013 by Mati Meron.  Internal change.
;		Modified 15-FEB-2013 by Mati Meron.  Added keyword DEC_DIGITS.
;-

	on_error, 1

	wha = One_of(dig,dec) > 0
	whi = One_of(flo,cei)
	if wha then begin
		rx = floor(abs(x),/l64)
		fx = abs(x) - rx
		fac = 10d^Default(dec,0,/dtyp)
		case whi of
			-1	:	res = round(fx*fac,/l64)/fac
			0	:	res = floor(fx*fac,/l64)/fac
			1	:	res = ceil(fx*fac,/l64)/fac
		endcase
		res= Sign(x)*(rx + res)
	endif else begin
		dig = Default(dig,1,/dtyp) > 1
		whi = One_of(flo,cei)
		res = x
		noz = where(x ne 0, nnoz)
		if nnoz gt 0 then begin
			fac = 10d^(1 - dig + floor(alog10(abs(x[noz]))))
			case whi of
				-1	:	res[noz] = fac*round(x[noz]/fac,/l64)
				0	:	res[noz] = fac*floor(x[noz]/fac,/l64)
				1	:	res[noz] = fac*ceil(x[noz]/fac,/l64)
			endcase
		endif
	endelse

	return, Cast(res,4,Calctype(0.,x))
end