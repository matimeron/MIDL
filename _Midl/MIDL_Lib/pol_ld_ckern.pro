Function Pol_ld_ckern, phi, coef, radius = rad, center = cen, _extra = _e

;+
; NAME:
;		POL_LD_CKERN
; VERSION:
;		8.15
; PURPOSE:
; 		Evaluates the logarithmic derivative of a polynomial on points of a
; 		circle in the complex plane.  Serves as an integration kernel for the
; 		function POL_ROOTNUM.
; CATEGORY:
;		Mathematical function.
; CALLING SEQUENCE:
;		Result = POL_LD_CKERN( PHI, COEF [, keywords])
; INPUTS:
;	PHI
;		Numeric, otherwise arbitrary (but used modulo 2*PI).
;	COEF
;		Numeric vector containing the polynomial coefficients.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	RADIUS
;		Real scalar, the radius of the circle in the complex plane.  Mandatory.
;	CENTER
;		Real or complex scalar, the center of the circle in the complex plane.
;		Defaults to (0,0).
;	_EXTRA
;		A formal keyword, not to be used directly.
; OUTPUTS:
;		Returns the values of the polynomial at CENTER + RADIUS*exp(i*PHI).  The
;		format of the result is same as this of PHI and the type is complex or
;		dcomplex, depending on the inputs.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls CALCTYPE, CAST, DEFAULT and POLEVAL from MIDL.
; MODIFICATION HISTORY:
;		Created 25-APR-2012 by Mati Meron.
;-

	on_error, 1

	typ = Calctype(phi,coef,complex(0))
	nc = n_elements(coef)
	if nc eq 1 then dcoef = [0] else dcoef = (coef*findgen(nc))[1:*]

	eiphi = exp(dcomplex(0,phi))
	z = Default(cen,0.) + rad*eiphi
	num = Poleval(z,dcoef)
	den = Poleval(z,coef)
	res = make_array(size=size([z]))
	noz = where(den ne 0, nnoz)
	if nnoz gt 0 then res[noz] = rad*eiphi[noz]*num[noz]/(2*!dpi*den[noz])
	
	return, Cast(res,typ,typ,/fix)
end