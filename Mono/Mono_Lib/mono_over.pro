Function Mono_over, a, b, c

;+
; NAME:
;		MONO_OVER
; VERSION:
;		4.3
; PURPOSE:
;		Calculates the two-crystal overlap function.
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = MONO_OVER( A [, B [, C]])
; INPUTS:
;	A
;		Numeric, otherwise arbitrary.  Only the absolute value matters.
; OPTIONAL INPUT PARAMETERS:
;	B
;		Numeric, otherwise arbitrary.  Only the absolute value matters.
;	C
;		Numeric, otherwise arbitrary.  Only the absolute value matters.
;
;		Comment: If any of A, B, C are vectors, they must be of same length.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the value(s) of the overlap function (details in the
;		"Truncated Gaussian Integral" writeup).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		See comment following the input variables.
; PROCEDURE:
;		Stright from the definitions in the writeup.  Calls CALCTYPE, CAST,
;		ERFINT_MM and ERRORF_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 15-APR-1999 by Mati Meron.
;		Rewritten 10-MAY=2002 by Mati Meron.
;-

	on_error, 1
	typ = Calctype(0.,a,b,c,default=4)
	sfac = 1/sqrt(8d)

	n = [n_elements(a),n_elements(b),n_elements(c)]
	fl = n gt 0
	if total(fl) gt 0 then begin
		nlo = min(n, max = nhi) > 1
		if nhi gt nlo then begin
			dum = where(n gt nlo and n lt nhi, ndum)
			if ndum gt 0 or nlo gt 1 then message, 'Dimensional mismatch!'
		endif
	endif else message, 'Missing input!'

	if n[0] lt nhi then wa = replicate(abs(Cast(a,5)),nhi) $
	else wa = abs(Cast(a,5))
	if fl[1] then if n[1] lt nhi then wb = replicate(abs(Cast(b,5)),nhi) $
	else wb = abs(Cast(b,5))
	if fl[2] then if n[2] lt nhi then wc = replicate(abs(Cast(c,5)),nhi) $
	else wc = abs(Cast(c,5))

	res = 1/(wa > 1)

	case total(fl) of
		1	:
		2	:	begin
					w = where(wa eq 0 and wb gt 0, nw)
					if nw gt 0 then begin
						bet = sfac/wb[w]
						res[w] = Errorf_mm(bet)
					endif
					w = where(wa gt 0 and wb gt 0, nw)
					if nw gt 0 then begin
						bet = sfac/wb[w]
						alp = wa[w]*bet
						res[w]= Erfint_mm(bet+alp,bet-alp)/(2*alp)
					endif
				end
		3	:	begin
					w = where(wa eq 0 and wb eq 0, nw)
					if nw gt 0 then res[w] = 0.5d*(1d + Sign(1 - 2*wc[w]))
					w = where(wa gt 0 and wb eq 0, nw)
					if nw gt 0 then res[w] = $
					((((1 - 2*wc[w] + wa[w])/2) < wa[w] < 1) > 0)/wa[w]
					w = where(wa eq 0 and wb gt 0, nw)
					if nw gt 0 then begin
						bet = sfac/wb[w]
						gam = 2*wc[w]*bet
						res[w] = (Errorf_mm(bet+gam) + Errorf_mm(bet-gam))/2
					endif
					w = where(wa gt 0 and wb gt 0, nw)
					if nw gt 0 then begin
						bet = sfac/wb[w]
						alp = wa[w]*bet
						gam = 2*wc[w]*bet
						res[w] = (Erfint_mm(bet+alp+gam,bet-alp+gam) + $
						Erfint_mm(bet+alp-gam,bet-alp-gam))/(4*alp)
					endif
				end
	endcase

	return, Cast(res,typ,typ,/fix)
end