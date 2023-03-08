Function Powft01, k, n

;+
; NAME:
;		POWFT01
; VERSION:
;		7.09
; PURPOSE:
;		Calculates a partial (0ver [0,1]) FT of x^n.
; CATEGORY:
;		Mathemetical Function (specialized).
; CALLING SEQUENCE:
;		Result = POWFT01 (K, N)
; INPUTS:
;	K
;		Numerical, otherwise arbitrary.
;	N
;		Scalar, the power of x in the integration.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the value(s) of Integral_0^1{x^N*exp(-i*K*x).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, calls CALCTYPE, CAST, GAMMA_MM and IGAMMA_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 10-JUL-2009 by Mati Meron.
;-

	on_error, 1

	typ = Calctype(k,n,complex(0))
	ic = dcomplex(0,1)
	eps = sqrt(n)*((machar(/doub)).xmin*(n+1))^(1./(n+1)) > (machar(/doub)).eps
	res = 1d/(n+1) - ic*k/(n+2)
	dum = where(abs(k) gt eps,ndum)
	if ndum gt 0 then res[dum] = $
	Igamma_mm(ic*k[dum],n+1)*Gamma_mm(n+1)/(ic*k[dum])^(n+1)

	return, Cast(res,typ,typ,/fix)
end