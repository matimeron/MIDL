Function Poleval, x, coef, deriv = der, quotient = qcoef

;+
; NAME:
;		POLEVAL
; VERSION:
;		8.46
; PURPOSE:
;		Evaluates a polynomial function according to the formula:
;			F = c(0) + c(1)*X + ... + c(n)*X^N
;		Similar to the library routine POLY.  The difference is that when the
;		keyword QUOTIENT is used, the routine returns, in QCOEF, the values of
;		the coefficients of the quotient polynomial.  In other words, given the
;		coefficients of a polynomial P, and a value Xc, the function returns
;		the value P(Xc), and in QCOEF are returned the coefficients of the
;		polynomial Q(X) = P(X)/(X - Xc).  Note that unless P(Xc) is 0, the
;		division has a remainder.
; CATEGORY:
;		Mathemetical function (general).
; CALLING SEQUENCE:
;		Result = POLEVAL( X, COEF [, QUOTIENT = QCOEF])
; INPUTS:
;	X
;		Numeric, otherwise arbitrary.
;	COEF
;		Numeric vector.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	DERIV
; 		Integer (or rounded down to integer on input) scalar, if given a 
; 		derivative, of order DER, of the polynomial function is evaluated.
;	QUOTIENT
;		An optional output parameter.  See below.
; OUTPUTS:
;		Returns the value of the polynomial at X.  The result has the same
;		structure and number of elements as X and the same type as the higher
;		of X and COEF.
; OPTIONAL OUTPUT PARAMETERS:
;	QUOTIENT
;		The name of the variable to receive the quotient polynomial.  The
;		quotient is an array with one more dimension than X.  For example, if
;		X is given as an array with dimensions (10,8,64) and the order of the
;		polynomial is 4 then the dimensions of the quotient will be (10,8,64,4).
;		QCOEF(4,5,6,*) will then contain the coefs. of P(X)/(X - X(4,5,6)), etc.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Standard Horner evaluation.  Uses the functions DEFAULT and FPU_FIX
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 15-NOV-1991 by Mati Meron.
;		Modified 10-SEP-1998 by Mati Meron.  Underflow filtering added.
;		Checked for operation under Windows, 30-JAN-2001, by Mati Meron.
;		Modified 10-JUN-2016 by Mati Meron.  Added keyword DERIV.
;-

	on_error, 1
	wcoef = Default(coef,0)
	np = n_elements(wcoef) - 1

	if Isnum(der) then begin
		ndr = Cast(der,3,3) > 0
		if ndr le np then begin
			for i = 1, ndr do begin
				wcoef = (wcoef*lindgen(np+1))[1:*]
				np = np - 1
			endfor
		endif else begin
			wcoef = 0
			np = 0
		endelse
	endif

	lq = np > 1
	xsiz = size(x)
	nd = xsiz[0]
	nx = xsiz[nd+2]

	if nd eq 0 then begin
		qsiz = [1,lq,xsiz[1],nx*lq]
		res = wcoef[np]
	endif else begin
		qsiz = [nd + 1, xsiz[1:nd], lq, xsiz[nd+1], nx*lq]
		res = wcoef[np]*make_array(size = xsiz, value = 1)
	endelse
	qcoef = make_array(size = qsiz)

	for i = np - 1, 0, -1 do begin
		qcoef(nx*i:nx*(i+1)-1) = res
		res = res*x + wcoef[i]
	endfor

	return, FPU_fix(res)
end
