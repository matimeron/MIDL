Function Sslit, wavl, bpars, ssl

;+
; NAME:
;		SSLIT
; VERSION:
;		4.2
; PURPOSE:
;		Generates the space-slit transformation for a beam.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = SSLIT( WAVL, BPARS, SSL)
; INPUTS:
;	WAVL
;		Wavelength (meter)
;	BPARS
;		Beam parameters, a 4-element vector corresponding to XPARS or YPARS in
;		an OPBEAM structure.
;	SSL
;		Slit size (meter)
; OPTIONAL INPUT PARAMETERS:
;		None
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the beam parameters as modified by the space slit.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Modifies the beam parameters according to eqs. 23-24 in Opt_Rel.
; MODIFICATION HISTORY:
;		Created 20-AUG-2001 by Mati Meron.
;-

	on_error, 1

	res = bpars
	wsq = ssl^2
	etas = wavl^2/(wsq*bpars[2])

	hzs = bpars[1] + bpars[2]*bpars[3]^2
	temf = wsq/(wsq + hzs)
	tems = (wsq + bpars[1])/(wsq + hzs) + etas

	res[0] = sqrt(temf)*bpars[0]
	res[1] = temf*(bpars[1] + etas*hzs)/tems
	res[2] = tems*bpars[2]
	res[3] = temf/tems*bpars[3]

	return, res
end