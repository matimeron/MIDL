Function Approx, x, y, threshold= tre, mult= mlt, complement= comp, global= glb

;+
; NAME:
;		APPROX
; VERSION:
;		8.1
; PURPOSE:
;		Compares scalars or arrays for "approximate equality".  The inputs
;		qualify as "approximately equal" if the absolute value of their
;		difference is less then the threshold.
; CATEGORY:
;		Mathematical Function (general).
; CALLING SEQUENCE:
;		Result = APPROX( X, Y [, THRESHOLD = TRE])
; INPUTS:
;	X, Y
;		Numeric variables, nearly arbitrary but with the following constraints:
;		either:
;			1)	One of X, Y is a scalar.
;		or
;			2)	X, and Y are both arrays of the same size and dimensionality.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	THRESHOLD
;		Accepts value(s) to serve as the threshold for comparison.  Optional,
;		the default threshold is 2*TOLER()*(ABS(X) + ABS(Y)), enough to cover
;		roudoff errors.  If provided, the value must satisfy same constaints as
;		X and Y (see above).
;	TMULT
;		If given and > 1, multiplies the default threshold.  If threshold is 
;		provided, TMULT is ignored.
;	COMPLEMENT
;		Switch.  If set, reverses the action of APPROX.
;	GLOBAL
;		Switch.  If set, the comparison is for global approximate equality, with
;		a scalar result, 1b if the comparison is true for *all* elements else 0b
; OUTPUTS:
;		Returns 1b for all locations where X and Y are approximately equal, 0b
;		elsewhere.  If COMPLEMENT is set, the return is reversed to 0b for
;		approximately equal, 1b elsewhere.
;
;		Note:	If GLOBAL is set, the result is scalar, 1b if all the compared
;		values are approximately equal, otherwise 0b.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than those specified in INPUTS.
; PROCEDURE:
;		Straightforward.  Calls ABS_MM, ARREQ, DEFAULT,ISNUM and TOLER from
;		MIDL.
; MODIFICATION HISTORY:
;		Created 30-SEP-2005 by Mati Meron.
;		Modified 1-JUL-2006 by Mati Meron.  Added keyword COMPLEMENT.
;		Modified 20-OCT-2007 by Mati Meron.  Added keyword GLOBAL.
;		Modified 5-SEP-2011 by Mati Meron.  Added keyword MULT.
;-

	on_error, 1

	if Arreq(x,0,/no) or Arreq(y,0,/no) or Arreq(x,y,/no) then begin
		doub = Isnum(x,/double) and Isnum(y,/double)
		wmlt = Default(mlt,1.,/dtyp)
		wtre = Default(tre,2*wmlt*Toler(doub=doub)*(Abs_mm(x)+Abs_mm(y)),/dtyp)
		if not (Arreq(wtre,0,/no) or Arreq(wtre,x,/no) or Arreq(wtre,y,/no)) $
		then message, 'Threshold format incompatible with input!'
		res = Abs_mm(x - y) le wtre
		if keyword_set(comp) then res = 1b - res
		if keyword_set(glb) then res = min(res)
	endif else message, 'X and Y formats are incompatible!'

	return, res
end