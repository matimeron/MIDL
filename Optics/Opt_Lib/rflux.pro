Function Rflux, bsec, dum, internal = idum, relative = rdum

;+
; NAME:
;		RFLUX
; VERSION:
;		4.2
; PURPOSE:
;		Calculates relative flux of a beam.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		Result = RFLUX( BSEC)
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
; OPTIONAL INPUT PARAMETERS:
;	DUM
;		"Consistency" parameter, does nothing.
; KEYWORD PARAMETERS:
;	/INTERNAL
;		Switch.  "Consistency" keyword, does nothing.
;	/RELATIVE
;		Switch.  "Consistency" keyword, does nothing.
; OUTPUTS:
;		Returns the flux of the beam, relative to the initial flux.  If BSEC is
;		a 2D array (see above), the result is a vector with one entry per each
;		row of BSEC.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;		Created 20-AUG-2001 by Mati Meron.
;		Modified 25-MAY-2002 by Mati Meron.
;-

	on_error, 1

	return, FPU_fix(bsec[0,*])
end
