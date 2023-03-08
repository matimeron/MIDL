Function Colen, bsec, wavl, internal = int, relative = rel

;+
; NAME:
;		COLEN
; VERSION:
;		4.2
; PURPOSE:
;		Calculates transverse coherence length of a beam.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		Result = COLEN( BSEC, WAVL [, INTERNAL = INT] [,RELATIVE = REL])
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
;		Wavelength, in meters (always).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/INTERNAL
;		Switch.  Selects between internal (meter, radian) and external
;		(micron, microradian) units.
;	/RELATIVE
;		Switch.  If set, the ratio of coherence length to transverse beam size
;		is returned.
; OUTPUTS:
;		If /RELATIVE is set, returns the transverse coherence length normalized
;		to the transverse size of the beam.  Else, returns the non-normalized
;		coherence length, in microns (or meter if /INTERNAL is set).  If BSEC
;		is a 2D array (see above), the result is a vector with one entry per
;		each row of BSEC.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Follows Eq. 9 from SOR.  Calls EMITT and HSIZE.
; MODIFICATION HISTORY:
;		Created 20-AUG-2001 by Mati Meron.
;		Modified 25-MAY-2002 by Mati Meron.
;-

	on_error, 1

	res = 1./Emitt(bsec,wavl,/rel,int=int)
	if not keyword_set(rel) then res = res*Hsize(bsec,internal=int)

	return, res
end