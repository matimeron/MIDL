Function Aslit, wavl, bpars, asl

;+
; NAME:
;		ASLIT
; VERSION:
;		4.2
; PURPOSE:
;		Generates the angle-slit transformation for a beam.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = ASLIT( WAVL, BPARS, ASL)
; INPUTS:
;	WAVL
;		Wavelength (meter)
;	BPARS
;		Beam parameters, a 4-element vector corresponding to XPARS or YPARS in
;		an OPBEAM structure.
;	ASL
;		Slit size (radian)
; OPTIONAL INPUT PARAMETERS:
;		None
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the beam parameters as modified by the angle slit.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Modifies the beam parameters according to eqs. 26-27 in Opt_Rel.
; MODIFICATION HISTORY:
;		Created 20-AUG-2001 by Mati Meron.
;-

	on_error, 1

	res = bpars
	wsq = asl^2
	etas = wavl^2/(wsq*bpars[1])

	tem = wsq/(wsq + bpars[2])

	res[0] = sqrt(tem)*bpars[0]
	res[1] = (1 + etas)*bpars[1]
	res[2] = tem*bpars[2]

	return, res
end