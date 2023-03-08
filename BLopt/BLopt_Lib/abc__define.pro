Pro ABC__define

;+
; NAME:
;		ABC__DEFINE
; VERSION:
;		8.714
; PURPOSE:
;		Defines the {ABC} structure used with Gaussian ABC-type distributions.
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
;			AMP		:	Amplitude, double precision scalar.
;			AMAT	:	A-matrix, 4x4, double precision.
;			BVC0	:	B-vector, length-4, double precision.
;			BVC1	:	B-vector, length-4, double precision.
;			CON0	:	C-scalar, double precision.
;			CON2	:	C-scalar, double precision.
; MODIFICATION HISTORY:
;		Created 15-JUNE-2017 by Mati Meron.
;		Documented 5-JUL-2019 by Mati Meron.
;-

	on_error, 1

	dum = {abc, amp: 0d, $
	amat: dblarr(4,4), bvc0: dblarr(4), bvc1: dblarr(4), con0: 0d, con2: 0d}

	return
end