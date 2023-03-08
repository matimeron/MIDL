Function Wrin_det, alp, kl, reduced = red

;+
; NAME:
;		WRIN_DET
; VERSION:
;		6.4
; PURPOSE:
;		Calculating the characteristic determinant of the "wrinkles LDE".
; CATEGORY:
;		Wrinkles function.
; CALLING SEQUENCE:
;		Result = WRIN_DET( ALP, KL [, REDUCED = RED])
; INPUTS:
;	ALP
;		The Alpha value from "Wrinkle Math".  Must be >= 1
;	KL
;		Scalar, the product KL from "Wrinkle Math".
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/REDUCED
;		Switch.  If set, a "reduced value of the determinant is calculated, one
;		which is simpler and sufficient for finding zeroes.
; OUTPUTS:
;		Returns the calculated value(s) of the determinant, in same format as
;		ALPHA
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Alpha must be >= 1.
; PROCEDURE:
;		Straightforward application of the results from "Wrinkle Math".  Calls
;		CALCTYPE, CAST and SP_BESELJ from MIDL.
; MODIFICATION HISTORY:
;		Created 10-DEC-2007 by Mati Meron.
;-

	on_error, 1

	if (size(kl))[0] ne 0 then message, 'KL must be a scalar!'
	typ = Calctype(alp,kl)

	chip = kl*sqrt((alp+1)/2d)
	chin = kl*sqrt((alp-1)/2d)

	res = Sp_beselj(chip,0)^2 - Sp_beselj(chin,0)^2
	if not keyword_set(red) then res = (4*chip*chin)^2*res

	return, Cast(res,typ,typ,/fix)
end