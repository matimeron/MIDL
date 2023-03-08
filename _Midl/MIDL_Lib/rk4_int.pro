Function RK4_int, dfun, range, eps, relative = rel, y_ini = yin, $
	step = stp, npoints = npt, at_most = atm, param = par, progress = prg, $
	keep_all = kep, xres = xrs, _extra= _e

;+
; NAME:
;    	RK4_INT
; VERSION:
;		8.42
; PURPOSE:
;		Performs a 4-th order Runge-Kutta integration of ODE(s).
; CATEGORY:
;		Mathematical function (general).
; CALLING SEQUENCE:
;		Result = RK4_INT( DFUN, RANGE [, EPS], Y_INI = YIN, [keywords])
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
;	RANGE
;		Two element vector, integration range.  Note that integration proceeds
;		from RANGE[0] to RANGE[1], not necessarily from smaller to larger.
; OPTIONAL INPUT PARAMETERS:
;	EPS
;		Allowed error, in each step.  Default is around 1e-7 for 
;		single-precision integration and 2e-16 for double-precision.  EPS is 
;		understood to represent absolute error unless the /RELATIVE is set.
; KEYWORD PARAMETERS:
;	/RELATIVE
;		Switch.  If set, EPS represent the allowed relative integration error.
;	Y_INI
;		The initial Y value(s), scalar or vector.
;	STEP													|
;		Initial integration step.							|	Only one
;	NPOINTS													|	of these two
;		Number of evaluation points.  If given, the initial	|	may be given.
;		step is given by (Range[1] - RANGE[0])/(NPOINTS - 1)|
;	
;		Note:	If neither STEP nor NPOINTS is given, an initial step value
;				is generated internally.
;		Note:	The initial step value may be adjusted during the integration,
;				upwards or downwards, as needed.
;	/AT_MOST
;		Switch.  If set, the value of the integration step cannot be adjusted
;		to size bigger than the initial one.
;		
;		Note:	If the initial integration step is generated internally, AT_MOST
;				is automatically set to "off".
;	PARAM
;		An arbitrary value or variable which is passed to the function DFUN.
;	/PROGRESS
;		Switch.  If set, consecutive values of X and DX are printed to the 
;		screen.
;	/KEEP_ALL
;		Switch.  If set, the values of Y in all the intermediate points are kept
;		and returned.  Default is returning just the final value(s).
;	XRES
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass additional keywords to DFUN.  Not to be
;		used directly.
; OUTPUTS:
;		Two possibilities:
;			1)  KEEP_ALL not set - Returns the final value(s) of Y, 
;				corresponding to the initial Y_INI and the derivative function 
;				DFUN, in the same format as Y.
;			2)	KEEP_ALL set - Returns an NxM array, where N is the length of Y
;				and M is the total number of X points used (including RANGE[0]
;				and RANGE[1].
; OPTIONAL OUTPUT PARAMETERS:
; 	XRES
; 		Returns either the final X value used (i.e. RANGE[1], trivially) or,
; 		if KEEP_ALL is set, a vector of all the X values used.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None, other than restrictions on DFUN.  See RK4_STEP for details.
; PROCEDURE:
;		Using the 4th order Runge-Kutta method, with adaptable step size, as 
;		presented in "Numerical Recipes in C" Chapt 16.1, 2nd edition.  
;		Classical Runge-Kutta was chosen over embedded one, as the better 
;		transparency was deemed worth some loss of efficiency.
;		Calls CALCTYPE, CAST, DEFAULT, ISNUM, ONE_OF, RK4_STEP, SIGN and TOLER,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 15-MAY-2013 by Mati Meron.
;		Modified 5-SEP-2013 by Mati Meron.  Internal changes.
;		Modified 10-MAY-2015 by Mati Meron.  Internal changes.
;-

	on_error, 1

	span = Cast(range[1] - range[0],4)
	sgn = Sign(span)
	if Isnum(yin) then typ = Calctype(span,yin,0.) $
	else message, 'Missing initial values!'
	typ = Calctype(span,yin,0.)
	deps = Toler(type=typ)
	weps = Default(eps,deps)
	little = (machar(double=(typ eq 5 or typ eq 9))).xmin
	refl = keyword_set(rel)
	prfl = keyword_set(prg)
	ny = n_elements(yin)
	nyfl = 0
	wx = Cast(range[0],5)
	wy = Cast(yin,5)
	wha = One_of(stp,npt)
	case wha of
		-1	:	dx = sgn*abs(Cast(10,typ)^ceil(alog10(sqrt(Toler(typ=typ)))))
		0	:	dx = sgn*abs(stp)
		1	:	dx = span/((npt>2) - 1)
	endcase
	atfl = keyword_set(atm) and (wha ge 0)
	if atfl then stlim = abs(dx)		

	rx = range[1] - wx[-1]
	while sgn*rx gt 0 do begin
		der = !null
		if abs(dx) ge (abs(rx) - weps) then dx = rx
		dy1 = RK4_step(dfun,dx,x=wx[-1],y=wy[*,-1],par=par,der=der,_extra=_e)
		tem= RK4_step(dfun,dx/2,x=wx[-1],y=wy[*,-1],par=par,der=der,_extra=_e)
		dy2 = tem + $
			RK4_step(dfun,dx/2,x=wx[-1]+dx/2,y=wy[*,-1]+tem,par=par,_extra=_e)
		del = dy2 - dy1
		if not nyfl then begin
			nyfl = n_elements(del) eq ny
			if not nyfl then message, 'Y dimensional discrepancy!'
		endif
		rat = abs(del)/weps
		if refl then rat = rat/((abs(wy[*,-1]) + abs(dx*der)) > little)
		mrat = max(rat)
		if mrat le 1 then begin
			dy = dy2 + del/15
			wy = [[wy],[wy[*,-1]+dy]]
			wx = [wx,wx[-1]+dx]
			rx = rx - dx
			if prfl then print, wx[-1], dx
		endif
		if mrat gt 0 then begin
			lot = floor(alog(1/mrat)/(5*alog(2)))
			dx = dx*2.^lot
			if atfl then dx = dx*(stlim/abs(dx) < 1)
		endif
	endwhile

	xrs = Cast(wx,typ,typ)
	res = Cast(reform(wy),typ,typ,/fix)
	if not keyword_set(kep) then begin
		wxrs = xrs[-1]
		res = reform(res[*,-1])
		if ny eq 1 then res = res[0]
	endif

	return, res
end