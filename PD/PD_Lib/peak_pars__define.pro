Pro Peak_pars__define

;+
; NAME:
;		USER__DEFINE
; VERSION:
;		8.46
; PURPOSE:
;		Defines the {PEAK_PARS} structure.
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
;		Standard.  Defines the structure PEAK_PARS, including:
;
;			FILE	:	The name of the data file, string.
;			SCAN	:	Scan number, applicable with SPEC data files.  Long.
;			FRAME	:	Frame number, applicable with SPEC data files.  Long.
;			FIT		:	Type of fit ('gau', 'lor' or 'voi'), string.
;			RANGE	:	Fitting (one-sided) range.  2-element vector, long.
;			PEAK_LOC:	Peak location, 2-element vector, long.
;			PEAK_VAL:	Value at peak, float.
;			MEAN_SET:	Flag, set to 1 when mean location is evaluated.  Long.
;			MEAN_LOC:	Mean peak location (if evaluated).  2-elem vector, float
;			CENTER	:	Fitted peak center.  2-element vector, float.
;			CENTERR	:	The fit errors of CENTER.  2-element vector, float.
;			SIGMA	:	Fitted peak sigma values.  2-element vector, float.
;			SIGERR	:	The fit errors of SIGMA.  2-element vector, float.
; MODIFICATION HISTORY:
;		Created 5-APR-2016 by Mati Meron.
;-

	on_error, 1

	dum = {peak_pars, file: '', scan: 0l, frame: 0l, fit: '', range: lonarr(2),$
	peak_loc: lonarr(2), peak_val: 0., mean_set: 0l, mean_loc: fltarr(2), $
	center: fltarr(2), centerr: fltarr(2), sigma: fltarr(2), sigerr: fltarr(2)}

	return
end