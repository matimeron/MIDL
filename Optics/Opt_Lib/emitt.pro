Function Emitt, bsec, wavl, internal = int, relative = rel

;+
; NAME:
;		EMITT
; VERSION:
;		4.2
; PURPOSE:
;		Calculates emittance of a beam.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		Result = EMITT( BSEC, WAVL [, INTERNAL = INT] [,RELATIVE = REL])
; INPUTS:
;	BSEC
;		Standard beam cross-section (see BEAMSEC_DEFINE).  A 4-element float
;		vector, includes in order, the following:
;			0	:	relative intensity.
;			1	:	square of source linear size.
;			2	:	square of source angular size.
;			3	:	current distance from source.  Note that this distance is
;					from the "optical source", not the physical one.
;
;		Comment:	BSEC can also be a 2D array with dimensions [4,N].  In such
;					case each row, BSEC[*,i] is taken as one beam cross-section
;					and the result returned is a vector of length N.
;	WAVL
;		Wavelength, in meters (always).  Mandatory if /RELATIVE is set, not
;		needed otherwise.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/INTERNAL
;		Switch.  Selects between internal (meter, radian) and external
;		(micron, microradian) units.
;	/RELATIVE
;		Switch.  If set, the ratio emittance/wavelength is returned.
; OUTPUTS:
;		If /RELATIVE is set, returns the emittance in wavelength units, else
;		returns emittance in micron*microradian (or meter*radian if /INTERNAL
;		is set).  If BSEC is a 2D array (see above), the result is a vector
;		with one entry per each row of BSEC.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		Block BEAM_STUFF, as defined in INIT_OPT.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Follows Eq. 2 from SOR.  Calls FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;		Created 20-AUG-2001 by Mati Meron.
;		Modified 25-MAY-2002 by Mati Meron.
;-

	common beam_stuff, exs, npomax, mmicr, mwlen
	on_error, 1

	if not keyword_set(rel) then begin
		if keyword_set(int) then mult=1. else mult = 1./mmicr^2
	endif else mult = 1./wavl

	return, FPU_fix(mult*sqrt(bsec[1,*]*bsec[2,*]))
end