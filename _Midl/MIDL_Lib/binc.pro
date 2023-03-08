Function Binc, x, n

;+
; NAME:
;		BINC
; VERSION:
;		8.47
; PURPOSE:
;		Calculates "renormalized" Bessel functions of the first kind.
; CATEGORY:
;		Mathematical function.
; CALLING SEQUENCE:
;		Result = BINC ( X, N)
; INPUTS:
;	X
;		Numeric, otherwise arbitrary.
;	N
;		Nonnegative scalar.  Should be integer (if not then rounded downwards
;		to an integer on input).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the values of the Bessel function J_N(X), multiplied by
;		(2/X)^N*Factorial(N).  The purpose of this normalization is to yield
;
;				BINC(0,N) = 1 for all N.
;
;		The result is of the same form and type as the input (but no lower than
;		FLOAT.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the restriction on N mentioned above.
; PROCEDURE:
; 		Straightforward.  Calls DEFAULT, FPU_FIX and ISNUM, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUL-2016 by Mati Meron.
;-

	on_error, 1

	eps = (machar(doub=Isnum(x,/doub))).xmin
	nn = Default(n,0,/dtyp) > 0
	fac = factorial(nn,/ul64)
	if nn eq 0 then del = 0 else del = 2*(eps*fac)^(1./nn)
	res = 0.*x + 1
	dum = where(abs(x) gt del, ndum)
	if ndum gt 0 then res[dum] = fac*(2./x[dum])^nn*beselj(x[dum],nn)

	return, FPU_fix(res)
end