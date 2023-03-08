Function BC_roots, lam, nrt, eps

;+
; NAME:
;		BC_ROOTS
; VERSION:
;		0.9
; PURPOSE:
;		Generates the thermal series for back-cooled geometry.
; CATEGORY:
;		Thermal calculations
; CALLING SEQUENCE:
;		Result = BC_ROOTS( LAM [, NRT, EPS])
; INPUTS:
;	LAM
;		Scalar parameter.
; OPTIONAL INPUT PARAMETERS:
;	NRT
;		Integer, specifies number of elements in series.
;	EPS
;		Scalar, specifies precision.  Default is machine precision.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the first NRT roots of the equation X*tan(X) = LAM.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Uses calls to ROOT, from MIDL, for low values of the series, and an
;		asymptotic expansion for high values.  Also calls CAST, DEFAULT and
;		TYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 1-FEB-2000 by Mati Meron.
;-

	on_error, 1
	nrt = Default(nrt, 1l, /dtype)
	typ = Type(lam) > 4
	teps = 2*Toler(type=typ)
	weps = Default(eps, teps, /dtype) > teps
	nlm = ceil((lam^2/weps)^0.25) > 1

	res = Root('BC_sfun', (nlm<nrt)*[0,!dpi], weps, /rel, par = lam, mult= -1)
	if nlm lt nrt then begin
		tem = !dpi*(nlm + dindgen(nrt - nlm))
		res = [res, tem + lam/tem]
	endif

	return, Cast(res,typ,typ,/fix)
end