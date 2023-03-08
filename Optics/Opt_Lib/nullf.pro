Function Nullf, wavl, bpars, dum

;+
; NAME:
;		NULLF
; VERSION:
;		4.2
; PURPOSE:
;		Generates the null transformation for a beam, i.e. does nothing.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = NULLF( WAVL, BPARS [,DUM])
; INPUTS:
;	WAVL
;		Wavelength (meter).  Not used.
;	BPARS
;		Beam parameters, a 4-element vector corresponding to XPARS or YPARS in
;		an OPBEAM structure.
; OPTIONAL INPUT PARAMETERS:
;	DUM
;		Anything, ignored.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns a copy of the input.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Does nothing, exists for completeness purposes only.
; MODIFICATION HISTORY:
;		Created 20-AUG-2001 by Mati Meron.
;-

	on_error, 1

	return, bpars
end