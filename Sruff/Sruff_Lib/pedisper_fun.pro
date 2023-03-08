Function PEDisper_fun, x, hwid

;+
; NAME:
;		EDISPER_FUN
; VERSION:
;		8.492
; PURPOSE:
;		Calculates the "finite width" correction to the dispersion integral part
;		of the scattering factor.
; CATEGORY:
;		Mathematical, X-ray specific.
; CALLING SEQUENCE:
;		Result = EDISPER_FUN( X, HWID)
; INPUTS:
;	X
;		Numeric, otherwise arbitrary.
;	HWID
;		Scalar.  The half-width of an absorption edge.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 		None.
; OUTPUTS:
;		Returns an approximation to the integral of a correction function
;		accross the edge (details elswhere).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		X should be positive.
; PROCEDURE:
;		Utilizing an analytical approximation to the integral above.  Uses CAST,
;		TOLER, TYPE and XLAX, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2017 by Mati Meron.
;-

	on_error, 1

	if min(x) lt 0 then message, 'X must be nonnegative!'
	sml = Toler(1d)

	delx = !dpi*hwid/2
	dx = double(x)
	res = -0.5*alog(abs((1-dx)/(1+dx)) > sml)
	pp = 1 + dx + delx
	pn = 1 + dx - delx
	np = 1 - dx + delx
	nn = 1 - dx - delx
	res = -(Xlax(pp) - Xlax(pn) - Xlax(np) + Xlax(nn))/(4*delx) - $
	0.5*alog(abs((1-dx)/(1+dx)) > sml)

	return, Cast(res,4,Type(x),/fix)
end