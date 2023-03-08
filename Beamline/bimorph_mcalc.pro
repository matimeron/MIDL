Function Bimorph_mcalc, mat, base, angle = ang, mir_loc = mlc, $
	flat = flt, collimate = col, focus_to = foc, show = sho, _extra = _e

;+
; NAME:
;		BIMORPH_MCALC
; VERSION:
;		8.13
; PURPOSE:
;		Calculating voltages for a bimorph mirror.
; CATEGORY:
;		Bimorph mirror specific.
; CALLING SEQUENCE:
;		Result = BIMORPH_MCALC( MAT, BASE, keywords)
; INPUTS:
;	MAT
;		Bimorph response matrix, an [N,N] array.
;	BASE
;		A [2,N} or [3,N] array representing the mirror slope profile absent
;		applied voltages.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ANGLE
;		Mirror angle, in mr.
;	MIR_LOC
;		Numeric scalar location of the mirror(s) measured in meters, from the 
;		source.  For two mirrors the midpoint should be used.
;	/FLAT													|
;		Switch.  Specifies that a flat mirror is required.	| One and only one
;	/COLLIMATE												| of these keywords 
;		Switch.  Specifies that a collimating mirror is		| must be used.
;		required.											|
;	FOCUS_TO												|
;		Numeric scalar, the required location of the focus.	|
;	/SHOW
;		Switch.  If set, the calculated corrected mirror slope profile is
;		displayed.
;	_EXTRA
;		Formal keyword used to pass keywords to imbedded routines.  Not to be 
;		used directly.
; OUTPUTS:
;		Returns the set of voltages required to bring the bimorph mirror to the
;		required shape.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Assuming linear voltage-curvature relationship, calculates voltages by
; 		inverting the response matrix.  Calls DEFAULT, DIF, ONE_OF and
; 		SVD_INVERT, from MIDL.
; 		
; 		Comment:	Considered obsolete, at present.
; MODIFICATION HISTORY:
;		Created 15-NOV-2011 by Mati Meron.
;-

	on_error, 1

	hstet = sin(1e-3*ang)/2
	mlc = Default(mlc,32.5)

	whi = One_of(flt,col,foc,val=val)
	case whi of
		0	:	irad = 0
		1	:	irad = hstet/mlc
		2	:	irad = hstet/mlc + hstet/(foc-mlc)
		else:	message, 'Missing focusing type info!'
	endcase

	bslp = reform(base[1,*])
	rslp = 1e3*irad*reform(base[0,*])
	volt = SVD_invert(mat,_extra=_e)##(rslp - bslp)

	if keyword_set(sho) then begin
		plot, base[0,*], bslp + mat##volt - rslp
		print
		print, '	Maximal difference = ', max(abs(Dif(volt,/lin)))
	endif

	return, volt
end