Function RK4_step, dfun, dx, x = x, y = y, param = par, deriv = der, _extra = _e

;+
; NAME:
;    	RK4_STEP
; VERSION:
;		8.21
; PURPOSE:
;		Performs a single step of 4-th order Runge-Kutta integration of ODE(s).
; CATEGORY:
;		Mathematical function (general).
; CALLING SEQUENCE:
;		Result = RK4_STEP( DFUN, DX, X = X, Y = Y [, keywords])
; INPUTS:
;	DFUN
;		Character value representing an existing IDL function.  The function
;		must accept two variables, X and Y, with X being a scalar and Y scalar
;		or vector.  The calling sequence for the function must be either
;			Result = DFUN(x,y)
;		or
;			Result = DFUN(x,y,extra)
;		where X and Y are the variables and EXTRA may be any single entity 
;		(scalar, array, structure etc.) used to pass additional parameters to 
;		the function.  Also, the function must be defined with the _EXTRA 
;		keyword, even if it is not used.
;		
;		The result returned by the function must be of same dimension as Y.
;	DX
;		Scalar, integration step size.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	X
;		The independent variable, must be a scalar.  Mandatory.
;	Y
;		The dependent variable, may be scalar of vector.  Mandatory.
;	PARAM
;		An arbitrary value or variable which is passed to the function DFUN.
;	DERIV
;		Scalar or vector, the derivative(s) of the dependent variable(s) with
;		respect to X, at the starting point.  If given, must be of same format
;		as Y.  If not given, it is calculated internally.
;	_EXTRA
;		A formal keyword used to pass additional keywords to DFUN.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the increment of Y over the interval [X, X + DX].  The result 
;		is of same format as Y.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The restriction mentioned above for the function DFUN, i.e.
;			1)	The function must be written so that it accepts keywords, at
;				least the _EXTRA keyword.
;			2)	The output of DFUN must be in same format as Y.
; PROCEDURE:
;		Using the 4th order Runge-Kutta method, as presented in "Numerical 
;		Recipes in C" Chapt 16.1, 2nd edition.  Classical Runge-Kutta was 
;		chosen over embedded one, as the better transparency was deemed worth
;		some loss of efficiency.
;		Calls CALCTYPE and CAST from MIDL.
; MODIFICATION HISTORY:
;		Created 15-MAY-2013 by Mati Meron.
;-

	on_error, 1

	typ = Calctype(x,y,0.,def=4)
	if n_elements(x) eq 1 then wx = x[0] else message, 'X must be a scalar!'
	pfl = n_elements(par) ne 0
	if n_elements(der) eq 0 then begin
		if pfl then der = call_function(dfun,wx,y,par,_extra=_e) $
		else der = call_function(dfun,wx,y,_extra=_e)
	endif
	a = dx*der
	if pfl then b = dx*call_function(dfun,wx+dx/2.,y+a/2.,par,_extra=_e) $
	else b = dx*call_function(dfun,wx+dx/2.,y+a/2.,_extra=_e)
	if pfl then c = dx*call_function(dfun,wx+dx/2.,y+b/2.,par,_extra=_e) $
	else c = dx*call_function(dfun,wx+dx/2.,y+b/2.,_extra=_e)
	if pfl then d = dx*call_function(dfun,wx+dx,y+c,par,_extra=_e) $
	else d = dx*call_function(dfun,wx+dx,y+c,_extra=_e)

	return, Cast((a+2*b+2*c+d)/6.,typ,typ,/fix)
end