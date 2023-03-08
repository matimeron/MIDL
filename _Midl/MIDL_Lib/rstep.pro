Function Rstep, x, location = loc, width = wid, reverse = rev

;+
; NAME:
;		RSTEP
; VERSION:
;		8.716
; PURPOSE:
;		Generates a "rounded" step function.
; CATEGORY:
;		Mathematical, general.
; CALLING SEQUENCE:
;		Result = RSTEP (X, [ LOCATION = LOC [, WIDTH = WID [, REVERSE = REV]]])
; INPUTS:
;	X
;		Numerical, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	LOCATION
;		Numeric scalar, specifies the step's location.  Defaults to 0.
;	WIDTH
;		Numeric scalar, specifies the step's width.  More exactly, within a
;		range of WIDTH around LOCATION the function value changes from ~ 0.24 to
;		~ 0.76 (out of full range of [0,1].
;		The default value of WIDTH is 1.
;	REVERSE
;		Switch.  If set, the function descends from 1 to 0.  In default
;		operation it ascendes from 0 to 1.
; OUTPUTS:
;		Returns the value(s) of an approximate step function.  See details in
;		PROCEDURE, below.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Approximates the step function as
;		(1 + ERF((X - LOCATION)/WIDTH)/2.  Calls DEFAULT, ERRORF_MM and FPU_FIX,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 20-NOV-2019 by Mati Meron.
;-

	on_error, 1

	wloc = Default(loc,0.,/dtyp)
	wwid = Default(wid,1.,/dtyp)
	if keyword_set(rev) then sgn = -1 else sgn = 1

	res = (1 + Errorf_mm(sgn*(x-wloc)/wwid))/2

	return, FPU_fix(res)
end