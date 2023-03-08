Pro IABC__define

;+
; NAME:
;		IABC__DEFINE
; VERSION:
;		8.714
; PURPOSE:
;		Defines the {IABC} structure used with Gaussian ABC-type distributions.
;		The {IABC} structure represents a partially integrated (over spatial or
;		angular coordinates only) ABC-type distribution.
; CATEGORY:
;		Initialization.
; CALLING SEQUENCE:
;		None.
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Standard.  Defines the structure ABC, including:
;
;			TYPE	:	Character string.
;			AMP		:	Amplitude, double precision scalar.
;			AMAT	:	A-matrix, 2x2, double precision.
;			BVC		:	B-vector, length-2, double precision.
;			CON		:	C-scalar, double precision.
; MODIFICATION HISTORY:
;		Created 15-JUNE-2017 by Mati Meron.
;		Documented 5-JUL-2019 by Mati Meron.
;-

	on_error, 1

	dum = {iabc, type: '', amp: 0d, amat: dblarr(2,2), bvc: dblarr(2), con: 0d}

	return
end