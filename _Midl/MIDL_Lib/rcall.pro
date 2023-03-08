Function Rcall, fun, x, par, mult = mlt, infi = inf, _extra = _e

;+
; NAME:
;		RCALL
; VERSION:
;		8.3
; PURPOSE:
;		An interface to the IDL function CALL_FUNCTION, for use in ROMBERG and 
;		ROOT.
; CATEGORY:
;		Programming (function call).
; CALLING SEQUENCE:
;		Result = RCALL( FUN, X [, PAR] [,keywords])
; INPUTS:
;	FUN
;		Character value representing an existing IDL function.  The function
;		must comply with the IDL standard of being able to accept an array
;		input and return an array output.  The calling sequence for the
;		function must be either
;			Result = FUN(x)
;		or
;			Result = FUN(x, more)
;		where X is the variable and MORE may be any single entity (scalar,
;		array, structure etc.) used to pass additional parameters to the
;		function.
;		In addition, the function may accept any number of keywords.
;	X
;		Numerical, arbitrary (other then possible limitations of FUN).
; OPTIONAL INPUT PARAMETERS:
;	PAR
;		An arbitrary value or variable which is passed to the function FUN.
; KEYWORD PARAMETERS:
; 	MULT
; 		Numeric scalar, multiplies the value of the returned function.
;	/INFI
;		Switch.  If set, the input and output are transformed to enable 
;		integration to infinite in ROMBERG.  The transformation includes:
;			X --> TAN(X)
;			Result --. (1 + TAN(X)^2)*Result
;	_EXTRA
;		A formal keyword used to pass additional keywords to FUN.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the calculated value of FUN (scaled if INFI is set).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Calls the function FUN, optionally with parameteres and keywords.
;		Uses DEFAULT and FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;		Created 5-MAY-2014 by Mati Meron.
;-

	on_error, 1

	ifl = keyword_set(inf)
	if ifl then wx = tan(x) else wx = x
	wha = [n_elements(par),n_elements(_e)] gt 0
	case total(wha*[1,2],/pre) of
		0	:	res = call_function(fun,wx)
		1	:	res = call_function(fun,wx,par)
		2	:	res = call_function(fun,wx,_extra=_e)
		3	:	res = call_function(fun,wx,par,_extra=_e)
	endcase

	wmlt = Default(mlt,1)
	if ifl then wmlt = (1 + wx^2)*wmlt

	return, FPU_fix(wmlt*res)
end