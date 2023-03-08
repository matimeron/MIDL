Function Difus, wavl, bpars, difl

;+
; NAME:
;		DIFUS
; VERSION:
;		4.2
; PURPOSE:
;		Generates the diffuser transformation for a beam.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = DIFUS( WAVL, BPARS, DIFL)
; INPUTS:
;	WAVL
;		Wavelength (meter)
;	BPARS
;		Beam parameters, a 4-element vector corresponding to XPARS or YPARS in
;		an OPBEAM structure.
;	DIFL
;		Diffuser's correlation length (meter)
; OPTIONAL INPUT PARAMETERS:
;		None
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the beam parameters as modified by the diffuser.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Modifies the beam parameters according to eq. 21 in Opt_Rel.
; MODIFICATION HISTORY:
;		Created 20-AUG-2001 by Mati Meron.
;-

	on_error, 1

	res = bpars
	tem = (wavl/difl)^2 + bpars[2]

	res[1] = bpars[1] + (tem - bpars[2])*bpars[2]*bpars[3]^2/tem
	res[2] = tem
	res[3] = bpars[2]*bpars[3]/tem

	return, res
end