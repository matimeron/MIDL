Function Parext, x, y, yer, extval= exv, signature= sig, error= err, coef = coe

;+
; NAME:
;		PAREXT
; VERSION:
;		8.72
; PURPOSE:
;		Calculates the location of a minimum of a function approximated as
;		parabola.
; CATEGORY:
;		Mathematical utility.
; CALLING SEQUENCE:
;		Result = PAREXT( X, Y [, YER], [keywords])
; INPUTS:
; 	X
; 		Numeric vector, length >= 3.
;	Y
;		Numeric vector, must be same length as X.
; OPTIONAL INPUT PARAMETERS:
;	YER
;		Numeric vector, if provided must be same length as X.
; KEYWORD PARAMETERS:
;	EXTVAL
;		Optional output, see below.
;	SIGNATURE
;		Optional output, see below.
;	ERROR
;		Optional output, see below.
;	COEF
;		Optional Output, see below.
; OUTPUTS:
;		Returns the location of the extremum of the function represented by Y.
; OPTIONAL OUTPUT PARAMETERS:
;	EXTVAL
;		Returns the estimated value of the function at the extremum.
;	SIGNATURE
;		Returns -1 if the extremum is a minimum, or +1 if it is a maximum.
;	ERROR
;		If the dimension of the inputs is >3, returns the error value of the
;		minimum location, else returns !NULL.
;	COEF
;		Returns the quadratic fit coefficients.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Fits the input X, Y values to a parabola and find the fit's minimum.
;		Calls CALCTYPE, CAST, CODIMS, ISNUM, LINFIT_MM and SIGN, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-FEB-2019 by Mati Meron.
;		Modified 20-MAR-2020 by Mati Meron.  Internal changes.
;-

	on_error, 1

	if Codims(x,y,yer,/same,dim=dim) then begin
		if n_elements(dim) eq 1 and dim ge 3 then begin
			typ = Calctype(0.,x,y)
			if Isnum(yer) then wei = 1d/yer else wei = []
			coe = Linfit_mm(1d*x,1d*y,wei,ord=2,error=cerr)
			if coe[2] ne 0 then res = -coe[1]/(2*coe[2]) $
			else res = (machar()).xmax/coe[1]
			exv = Cast(coe[0] - coe[1]^2/(4*coe[2]),typ,typ)
			sig = -Sign(coe[2])
			if dim eq 3 then err = [] $
			else err = Cast( $
			sqrt((coe[1]*cerr[2])^2 + (coe[2]*cerr[1])^2)/(2*coe[2]^2),typ,typ)
		endif else message, 'Inputs must be vectors of length >=3!'
	endif else message, 'Inputs must be of same length!'

	return, Cast(res,typ,typ,/fix)
end