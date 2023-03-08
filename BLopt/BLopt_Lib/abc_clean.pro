Function ABC_clean, abc, eps

;+
; NAME:
;		ABC_CLEAN
; VERSION:
;		8.714
; PURPOSE:
;		Cleans up an ABC-type distribution.
; CATEGORY:
;		Optics ABC-formalism calculations.
; CALLING SEQUENCE:
;		Result = ABC_CLEAN ( ABC [, EPS])
; INPUTS:
;	ABC
;		An {ABC} type structure.
; OPTIONAL INPUT PARAMETERS:
;	EPS
;		Scalar, smallness parameter.  If not given, default value is generated
;		internally.
; KEYWORD PARAMETERS:
;		None.
;; OUTPUTS:
;		Returns an ABC structure, with values smaller in absolute value than EPS
;		replaced with zeroes.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls STREQ and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUNE-2017 by Mati Meron.
;		Documented 5-JUL-2019 by Mati Meron.
;-

	on_error, 1

	if Streq(tag_names(abc,/str),'abc') then begin
		res = abc
		weps = Default(eps,16*Toler(/double)) > 0
		abarr = abs(res.amat)
		dum = where(abarr lt weps*max(abarr),ndum)
		if ndum gt 0 then res.amat[dum] = 0
		abarr = abs(res.bvc0)
		dum = where(abarr lt weps*max(abarr),ndum)
		if ndum gt 0 then res.bvc0[dum] = 0
		abarr = abs(res.bvc1)
		dum = where(abarr lt weps*max(abarr),ndum)
		if ndum gt 0 then res.bvc1[dum] = 0
	endif else message, 'Primary input must be an ABC structure!'

	return, res
end