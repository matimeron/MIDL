Function Gen_corr, x, y, corv, iterate = itr

;+
; NAME:
;		GEN_CORR.
; VERSION:
;		8.15
; PURPOSE:
;		Generates correlated random variable sets.
; CATEGORY:
;		Statistical function.
; CALLING SEQUENCE:
;		Result = GEN_CORR( X , Y, CORV [, ITERATE = ITR])
; INPUTS:
;	X
;		Numeric vector.
;	Y
;		Numeric vector, same length as X.
;	CORV
;		Required correlation value.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	ITERATE
;		Integer scalar, specifies the maximum number of iteration.  If not 
;		given, defaults to CEIL(SQRT(N)).
; OUTPUTS:
;		Returns the vector Y rearranged so as that the correlation of the
;		result with X is approximately CORV.  Since the original values of Y are
;		only rearranged, not changed, the CORV is usually not obtained exactly,
;		but the deviation is typically 0(1/N) where N is the number of elements
;		of X (and Y).
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The length of X and Y must be the same.
;		ABS(CORV) <= 1.
; PROCEDURE:
; 		Selective rearrangement of Y to bring the correlation value with X close
; 		to the desired value.  Calls CM_CORR from LIQ_LIB.  Calls DEFAULT, 
; 		SHUFFLE and SIGN from MIDL.
; MODIFICATION HISTORY:
;		Created 15-FEB-2002 by Mati Meron.
;		Streamlined 30-MAR-2012 by Mati Meron.
;-

	on_error, 1

	n = n_elements(x)
	if n_elements(y) ne n then message, 'Length mismatch!' else res = y
	ind = lindgen(n)
	ncor = Default(corv,0.)
	if abs(ncor) gt 1 then message, 'Illegal correlation value!'
	witr = Default(itr,ceil(sqrt(n)),/dtyp) > 1

	repeat begin
		pcor = CM_corr(x,res)
		sgn = Sign(ncor-pcor,/noz)
		nn = round(n*sgn*(ncor - pcor)/((1 - sgn*pcor) > 1./n)) < n
		if nn le 1 then break
		pind = (Shuffle(ind))[0:nn-1]
		sx = sort(x[pind])
		sr = sort(res[pind])
		pres = (res[pind])[sr]
		if sgn lt 0 then pres = reverse(pres)
		res[pind] = pres[sort(sx)]
		witr = witr - 1
	endrep until witr eq 0

	return, res
end