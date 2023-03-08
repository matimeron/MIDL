Function Rbril, bsec, wavl, internal = idum, relative = rdum

;+
; NAME:
;		RBRIL
; VERSION:
;		6.0
; PURPOSE:
;		Calculates relative brilliance of beam.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		Result = RBRIL( BSEC [,WAVL])
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
;	WAVL
;		Wavelength, in meters (always).  If not given, defaults to 0.
; KEYWORD PARAMETERS:
;	/INTERNAL
;		Switch.  "Consistency" keyword, does nothing.
;	/RELATIVE
;		Switch.  "Consistency" keyword, does nothing.
; OUTPUTS:
;		Returns the brilliance of the beam, relative to the initial brilliance.
;		If BSEC is a 2D array (see above), the result is a vector with one entry
;		per each row of BSEC.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Follows the procedure in SOR, part 7.  Calls EMITT and RFLUX.  Calls
;		FPU_FIX and ISNUM from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2007 by Mati Meron.
;-

	on_error, 1

	flx = Rflux(bsec)
	wafl = Isnum(wavl)
	if Isnum(wavl) then begin
		emt = Emitt(bsec,wavl,/rel)
		res = bsec[0,*]/sqrt(emt^2 - 1)
	endif else begin
		emt = Emitt(bsec)
		res = bsec[0,*]/emt
	endelse

	return, FPU_fix(res/res[0])
end
