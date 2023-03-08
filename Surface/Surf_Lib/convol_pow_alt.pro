Function Convol_pow_alt, x, q, range = r, inverse = inv, exact = exc

;+
; NAME:
;		POWSUM
; VERSION:
;		7.09
; PURPOSE:
;		Calculates forward or inverse power sum transform.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result = POWSUM( X, Q [, keywords])
; INPUTS:
;	X
;		Numerical vector.
;	Q
;		Numerical scalar, the multiplier.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	RANGE
;		Integer scalar, specifies range of summation.  Details in PROCEDURE,
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
;		RANGE, if given, must be 2 or more.
; PROCEDURE:
;		The forward transformation is RES_i = (1-Q)*sum_0^i{X_k*Q^(i-k)} or, if
;		RANGE is given, RES_i = (1-Q)*sum_(i-RANGE+1)^i{X_k*Q^(i-k)}.
;		The inverse transformation reverses the effects of the forward one,
;		recovering the original sequence.
;		Calls DEFAULT and FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;		Created 5-OCT-2008 by Mati Meron.
;-

	on_error, 1

	typ = Calctype(x,q,r,def=4)
	n = n_elements(x)
	dx = Cast(x,5)
	if abs(q) ge 1 then message, '|Q| must be < 1!'
	dq = Cast(q,5)
	r = 0 > Default(r,n) < n
	ir = floor(r)
	buf = [dblarr(ir+2),dx]
	j = lindgen(n) + ir + 2
	dr = Cast(r - ir,5)
	qpr = dq^ir
	fco = dr^2
	sco = (1 - dr)^2
	if keyword_set(inv) then begin
		ytem = 2/(1-dq)*(dx - dq*shift([dx,0],1))
		if keyword_set(exc) then begin
			for i = 0, n-1 do buf[j[i]] = ytem[i] - buf[j[i]-1] + $
			qpr*(fco*dq*buf[i] + (1- fco + dq*(1- sco))*buf[i+1] + sco*buf[i+2])
		endif else begin
			for i = 0, n-1 do buf[j[i]] = ytem[i] + $
			qpr*(dr*dq*buf[i+1] + (1-dr)*buf[i+2])
			buf = (5*(buf + shift(buf,-1)) - (shift(buf,1) + shift(buf,-2)))/16
		endelse
		res = buf[ir+2:*]
	endif else begin
		res = dx
		qpow = reverse(dq^(lindgen(ir+1)))
		for i = 0, n-1 do res[i] = qpr*(fco*buf[i+1] - sco*buf[i+2]) + $
		total(buf[i+2:j[i]]*qpow) + total(buf[i+2:j[i]-1]*qpow[1:*])
		res = (1-dq)/2*res
	endelse

	return, Cast(res,typ,typ,/fix)
end