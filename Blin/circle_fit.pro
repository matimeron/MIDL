Function Circle_fit, x, y, ser, eps = eps, guess = gue, show_fit = shf, $
	radius = rad, error = cer, rerror = rer, status = sta, _extra= _e

;+
; NAME:
;		CIRCLE_FIT
; VERSION:
;		8.0
; PURPOSE:
;		Fits the input to a circle.
; CATEGORY:
;		Mathematical, geometrical fitting.
; CALLING SEQUENCE:
;		Result = CIRCLE_FIT( X, Y, [, SER] [, keywords])
; INPUTS:
;	X
;		Numeric, A vector (scalar is considered to be a vector of length 1), an
;		[2,*] array or a [3,*] array.
; OPTIONAL INPUT PARAMETERS:
;	Y
;		Numeric, acceptable only when X is a vector, in which case Y has to be
;		a vector of same length.
;	SER
;		Numeric, same restrictions as for Y.  Taken as the values of the
;		statistical y-errors.
;
;		Note:	If only one input is provided then:
;				1)	If it is a vector, it is taken to be Y and X is generated
;					internally with a spacing of 1.  SER is taken as 0.
;				2)	If it is a [2,*] array, it is split into X and Y vectors.
;					SER is taken as 0.
;				3)	If it is a [3,*] array, it is split into X, Y and SER.
; KEYWORD PARAMETERS:
;	EPS
;		Smallness parameter, used to determine convergence.  Default is machine
;		precision, according to data type, as given by TOLER.
;	GUESS
;		A 3-element vector, containing initial guess of the result in the form
;		of [Radius,X_center,Y_center].  If not given, an initial guess is
;		generated internally.
;	/SHOW_FIT
;		Switch.  If set, the input values and the fitted circle are plotted to
;		the screen.
;	RADIUS
;		Optional output, see below.
;	ERROR
;		Optional output, see below.
;	RERROR
;		Optional output, see below.
;	STATUS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to FGH_EXT, as well as
;		plotting keywords.  Not to be used directly.  Keywords of interest are
;
;		TRY	:			Specifes number of iteration tries before giving up.
;						Default is 10.
;		/PROGRESS	:	Switch.  If set, the iteration progress is printed to
;						the screen.
; OUTPUTS:
;		Returns a 2-element numeric vector containing the coordinates of the
;		center of the fitted circle, in x,y order.
; OPTIONAL OUTPUT PARAMETERS:
;	RADIUS
;		Returns the radius of the fitted circle.
;	ERROR
;		A 2-element vector containing the errors of the output, in same order.
;	RERROR
;		Returns the fit error of the radius.
;	STATUS
;		Scalar output, shows the status of the iteration.  Value of 0 means
;		"did not converge".  If this occurs, can try with higher value of TRY.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Estimates the initial value of the parameters, then refines the
;		estimate using nonlinear Least Squares fitting.  Calls CIRCLE_FUN.
;		Calls CALCTYPE, DEFAULT, ELLIPSE_MM, FGH_EXT, SPHERE_SOLVE, SPLIT_XY 
;		and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-APR-2009 by Mati Meron.
;		Modified 25-AUG-2010 by Mati Meron.  Replaced ELLIPSE with ELLIPSE_MM.
;-

	on_error, 1
	fnam = 'circle_fun'

	n = Split_xy(x,y,ser,x=wx,y=wy,z=wser,inpz=inz)
	if n ge 3 then begin
		typ = Calctype(wx,wy)
		feps = Default(eps,Toler(wx))
		s = sort(x)
		wx = wx[s]
		wy = wy[s]
		if inz then wei = 1/((wser[s] > feps)^2)
		if n_elements(gue) eq 3 then begin
			sta = 1b
			ipar = gue
		endif else begin
			ind = [0l,(n-1)/2,n-1]
			cen= Sphere_solve(transpose([[wx[ind]],[wy[ind]]]),rad=rad,stat=sta)
			ipar = [rad,cen]
		endelse
		if sta then begin
			if n gt 3 then begin
				dum = Circle_fun(ipar,wx,wy)
				rpar = FGH_ext(fnam,feps,x_ini=ipar,/min,/sum,wei=wei,$
				error=err,stat=sta,_extra=_e)
				rad = rpar[0]
				rer = err[0]
				cen = rpar[1:2]
				cer = err[1:2]
			endif
			if keyword_set(shf) then begin
				plot, wx, wy, /nodata, _extra = _e
				plots, wx, wy, psym = 1, col = !pcol.red, _extra = _e
				Ellipse_mm, cen= cen, rad= [rad,rad], col= !pcol.green, noclip=0
			endif
		endif else message, 'Not a circle!', /con
	endif else message, 'At least 3 points are needed!'

	return, cen
end