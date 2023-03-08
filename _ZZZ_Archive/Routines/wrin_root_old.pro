Function Wrin_root_old, alp, kl, eps = eps, stat = sts, _extra = _e

;+
; NAME:
;		WRIN_ROOT
; VERSION:
;		6.4
; PURPOSE:
;		Calculating the roots of the characteristic determinant of the
;		"wrinkles LDE".
; CATEGORY:
;		Wrinkles function.
; CALLING SEQUENCE:
;		Result = WRIN_ROOT( ALP, KL [, keywords])
; INPUTS:
;	ALP
;		The Alpha value from "Wrinkle Math".  Given as either a single value,
;		in which case roots in the range [1,Alpha] are sought, or as a 2-element
;		vector specifying the search range.
;	KL
;		Scalar, the product KL from "Wrinkle Math".
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	EPS
;		Scalar specifying the accuracy required in root calculation.  See EROOT
;		for details.
;	STAT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the determinant root values in the provided Alpha range.
; OPTIONAL OUTPUT PARAMETERS:
;	STAT
;		Scalar or vector (with as many elements as the number of roots found),
;		specifying the calculation status.  See EROOT for details.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Alpha must be >= 1.
; PROCEDURE:
;		Calls (indirectly) WRIN_DET, as well as DEFAULT, EROOT and TOLER from
;		MIDL.
; MODIFICATION HISTORY:
;		Created 10-DEC-2007 by Mati Meron.
;-

	on_error, 1

	case n_elements(alp) of
		1	:	walp = [1.,alp]
		2	:	walp = alp
		else:	message, "Alpha must've 1 or 2 elements!"
	endcase
	if min(walp) lt 1 then message, 'Invalid range!'
	weps = Default(eps,2*Toler(alp))

	res = ERoot('wrin_det',walp,weps,par=kl,multi=-1,stat=sts,_extra=_e)

	return, res
end