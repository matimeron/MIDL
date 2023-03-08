Function Fprop, wavl, bpars, dist

;+
; NAME:
;		FPROP
; VERSION:
;		4.2
; PURPOSE:
;		Generates the free propagation transformation for a beam.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = FPROP( WAVL, BPARS, DIST)
; INPUTS:
;	WAVL
;		Wavelength (meter).
;	BPARS
;		Beam parameters, a 4-element vector corresponding to XPARS or YPARS in
;		an OPBEAM structure.
;	DIST
;		Propagation distance (meter).
; OPTIONAL INPUT PARAMETERS:
;		None
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the beam parameters, modified by the propagation.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Modifies the beam parameters according to eq. 10 in Opt_Rel.
; MODIFICATION HISTORY:
;		Created 20-AUG-2001 by Mati Meron.
;-

	on_error, 1

	res = bpars
	res[3] = res[3] + dist

	return, res
end