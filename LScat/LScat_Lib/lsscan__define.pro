Pro Lsscan__define

;+
; NAME:
;		LSSCAN__DEFINE
; VERSION:
;		8.14
; PURPOSE:
;		Defines the {LSSCAN} sub-structure used with LS files.
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
;		Standard.  Defines the structure LSSCAN, including:
;
;			SFILE	:	Name of the scan file, string.
;			PRES	:	The surface pressure value, float.
;			ORD		:	The scan diffraction order, long integer.
;			DIR		:	The measurement direction, long, 0 for horizontal,
;						1 for vertical.
;			DSIGN	:	The direction sign, long, 1 for "up" or "right", 
;						-1 for "down" or "left".
;			AMP		:	The fitted peak amplitude value, float.
;			AMP_ERR	:	The error of AMP, float.
;			CENT	:	The fitted peak center value, float.
;			CENT_ERR:	The error of CENT, float.
;			HWID	:	The fitted half-width value, float.
;			HWID_ERR:	The error of HWID, float.
; MODIFICATION HISTORY:
;		Created 25-NOV-2011 by Mati Meron.
;-

	on_error, 1

	dum = {lsscan, sfile: '', pres: 0., ord: 0l, dir: 0l, dsign: 0l, $
		amp: 0., amp_err: 0., cent: 0., cent_err: 0., hwid: 0., hwid_err: 0.}

	return
end