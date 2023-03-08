Function Focus, wavl, bpars, focl

;+
; NAME:
;		FOCUS
; VERSION:
;		4.2
; PURPOSE:
;		Generates the focusing transformation for a beam.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = FOCUS( WAVL, BPARS, FOCL)
; INPUTS:
;	WAVL
;		Wavelength (meter).
;	BPARS
;		Beam parameters, a 4-element vector corresponding to XPARS or YPARS in
;		an OPBEAM structure.
;	FOCL
;		Focal length (meter).
; OPTIONAL INPUT PARAMETERS:
;		None
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the beam parameters as modified by the focusing.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Modifies the beam parameters according to eq. 12 in Opt_Rel.
; MODIFICATION HISTORY:
;		Created 20-AUG-2001 by Mati Meron.
;-

	on_error, 1

	res = bpars
	tem = bpars[1] + bpars[2]*(focl - bpars[3])^2

	res[1] = focl^2*bpars[1]*bpars[2]/tem
	res[2] = tem/focl^2
	res[3] = focl*(bpars[2]*bpars[3]*(focl - bpars[3]) - bpars[1])/tem

	return, res
end