Function Convol_pow, x, q, range = r, inverse = inv

;+
; NAME:
;		CONVOL_POW
; VERSION:
;		7.09
; PURPOSE:
;		Calculates forward or inverse power sum transform.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result = CONVOL_POW( X, Q [, keywords])
; INPUTS:
;	X
;		Numerical vector.
;	Q
;		Numerical scalar, the multiplier.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	RANGE
;		Numerical scalar, specifies range of summation.  Details in PROCEDURE,
;		below.
;	/INVERSE
;		Switch.  Specifies that inverse transformation is to be performed.
; OUTPUTS:
;		Returns a numerical vector of same length as X, forward (or backward,
;		if /INVERSE is set) transformed.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The absolute value of Q must be less than 1.
; PROCEDURE:
;		The forward transformation is RES_i = (1-Q)*sum_0^i{X_k*Q^(i-k)} or, if
;		RANGE is given, RES_i = (1-Q)*sum_(i-RANGE)^i{X_k*Q^(i-k)}.  In the
;		case of non_integer range, a correction for the fractional part is
;		appended.  The inverse transformation reverses the effects of the
;		forward one, recovering the original sequence.
;		Calls CALCTYPE, CAST and DEFAULT, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-OCT-2008 by Mati Meron.
;		Modified 25-JAN-2009 by Mati Meron.  Internal changes.
;-

	on_error, 1

	typ = Calctype(x,q,r,def=4)
	n = n_elements(x)
	dx = Cast(x,5)
	if abs(q) ge 1 then message, '|Q| must be < 1!'
	dq = Cast(q,5)
	r = 0 > Default(r,n) < n
	ir = floor(r)
	zr = (ir eq 0)
	buf = [dblarr(ir+2),dx]
	j = lindgen(n) + ir + 2
	dr = Cast(r - ir,5)
	qpr = dq^ir
	a = (1 - dr)^2/2
	b = dr^2/2
	if keyword_set(inv) then begin
		ytem = (dx - dq*shift([dx,0],1))/(1-dq)
		for i = 0, n-1 do buf[j[i]] = (ytem[i] + $
		qpr*(b*dq^2*buf[i] + (1-a-b)*dq*buf[i+1] + a*(1-zr)*buf[i+2]))/(1-a*zr)
		res = buf[ir+2:*]
	endif else begin
		res = dx
		qpow = reverse(dq^(lindgen(ir+1)))
		for i = 0, n-1 do res[i] = qpr*(b*dq*buf[i+1] - a*buf[i+2]) + $
		total(buf[i+2:j[i]]*qpow)
		res = (1-dq)*res
	endelse
	res = Cast(res,typ,typ,/fix)
	sca = 2*max(abs(res))

	return, res + sca - sca
end