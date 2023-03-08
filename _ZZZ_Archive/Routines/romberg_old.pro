Function Romberg_old, fun, range, eps, infi = inf, relative = rel, params = par, $
	complex= cmp, try= ntry, verify= ver, error= erv, status= stat, _extra= _e

;+
; NAME:
;    	ROMBERG
; VERSION:
;		8.45
; PURPOSE:
;		Performs high precision numerical integration.
; CATEGORY:
;		Mathematical function (general).
; CALLING SEQUENCE:
;		Result = ROMBERG( FUN, RANGE [, EPS [, keywords]])
; INPUTS:
;	FUN
;		Character value representing an existing IDL function.  The function
;		must comply with the IDL standard of being able to accept an array
;		input and return an array output.  The calling sequence for the
;		function must be either
;			Result = FUN(x)
;		or
;			Result = FUN(x, more)
;		where X is the variable and MORE may be any single entity (scalar,
;		array, structure etc.) used to pass additional parameters to the
;		function.
;		In addition, the function may accept any number of keywords.
;	RANGE
;		Depending on the setting of INFI (see below), as follows:
;			INFI not set-	RANGE is a 2-element vector specifying the 
;							integration range.  Mandatory.
;			INFI set	-	Range is a scalar specifying lower integration
;							limit.  Optional (default is 0).
; OPTIONAL INPUT PARAMETERS:
;	EPS
;		Allowed integration error.  Default is around 1e-7 for single-precision
;		integration and 2e-16 for double-precision.  EPS is understood to
;		represent absolute error unless the keyword RELATIVE is set.
; KEYWORD PARAMETERS:
; 	/INFI
; 		Switch.  If set, The upper limit of the integration is infinity.  In
; 		this case RANGE needs to contain just a single value, the lower 
; 		integration limit.  If RANGE is not provided, the lower limit is taken
; 		to be 0.
; 		
; 		Note:	If the integral to infinity of FUN doesn't exist in absolute
; 				sense, the result return by ROMBERG will be random and 
; 				upredictable.  Some care needs to be exercised.
;	/RELATIVE
;		Switch.  If set, EPS represent the allowed relative integration error.
;	PARAMS
;		An arbitrary value or variable which is passed to the function FUN.
;	/COMPLEX
;		Switch.  If set, complex integration is performed, even if the range
;		and parameters are real.  If either RANGE or PARAMS is of type DOUBLE,
;		the integration will be DOUBLE COMPLEX.
;	TRY
;		Normally ROMBERG exits, with STATUS = 2, if the calculation error
;		starts to grow before the convergence criterion is satisfied.  Setting
;		TRY to an integer > 0 specifies the number of additional attempts at
;		convergence before exit (useful with ill-conditioned functions).  The
;		default value is 0.
;	/VERIFY
;		Switch.  Normally ROMBERG exits when the estimated integration error
;		drops below EPS.  When /VERIFY is set, however, two consecutive error
;		values below EPS are required (useful with ill conditioned functions).
;	ERROR
;		Optional output, see below.
;	STATUS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass additional keywords to FUN.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the value of the integral.  The result is always a scalar. The
;		numerical type of the result (floating, double-precision or complex) is
;		determined by the type of values returned by FUN.
; OPTIONAL OUTPUT PARAMETERS:
;	ERROR
;		The name of the variable to receive the estimated integration error.
;		If RELATIVE is set the error returned is relative.
;	STATUS
;		The name of the variable to receive integration status information.
;		Possible values are:
;			0 - Integration didn't converge.
;			1 - OK.
;			2 - Integration converged, but with precision worse then specified.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Enhanced Romberg integration, using the extended midpoint rule and
;		Neville's interpolation algorithm.  The process is iterated untill
;		either the desired accuracy is achieved, the maximal allowed number of
;		steps is exceeded or further iterations cause the error to grow instead
;		of diminishing (the last can be postponed using the TRY keyword).  The
;		procedure can handle functions with an integrable singularity at one
;		(or both) end of the integration interval.
;		Uses CALCTYPE, CAST, DEFAULT, FPU_FIX, ISNUM, RCALL and TOLER from MIDL.
; MODIFICATION HISTORY:
;		Created 15-FEB-1992 by Mati Meron.
;		Modified 20-JAN-1994 by Mati Meron.  Added keyword TRY.
;		Modified 5-OCT-1998 by Mati Meron.  Underflow filtering added.
;		Checked for operation under Windows, 30-JAN-2001, by Mati Meron.
;		Modified 10-DEC-2002 by Mati Meron.  Enabled zero integration range.
;		Modified 20-JAN-2006 by Mati Meron.  Added keyword /VERIFY.
;		Modified 5-MAY-2014 by Mati Meron.  Added the capacity to pass keywords
;		to the called function (this was handled, before, by the separate
;		routine EROMBERG).  Also, added integration to infinity option.
;		Modified 20-FEB-2016 by Mati Meron.  Added keyword COMPLEX.
;-

	on_error, 1
	stat = 0

	isdob = Isnum(range,/doub,typ=ftyp) or Isnum(par,/doub,typ=ptyp)
	if keyword_set(cmp) then ftyp = 6 + 3*isdob $
	else ftyp = Calctype(ftyp,ptyp,4,def=4,/types)
	etyp = 4 + isdob
	sinf = machar(double = isdob)
	eps = abs(Default(eps,Toler(type = ftyp),/dtype))

	ifl = keyword_set(inf)
	if ifl then begin
		if n_elements(range) le 1 then begin
			wrange = atan(Default(range,0.,low=etyp))
			if isdob then wrange = [wrange,!dpi/2] else wrange = [wrange,!pi/2]
		endif else message, 'Upper limit not needed for infinite range!'
	endif else begin
		if n_elements(range) eq 2 then wrange = Cast(range,4+isdob) $
		else message, 'Two values required for finite range!'
	endelse

	rex = wrange[1] - wrange[0]
	xc = wrange[0] + 0.5*rex
	relf = keyword_set(rel)
	ntr = Default(ntry,0,/dtype) > 0
	verfl = keyword_set(ver)
	vcount = 0
	fc = Rcall(fun,xc,par,inf=ifl,mult=rex/3,_extra=_e)

	if abs(rex) gt 0 then begin
		kmax = floor(-alog(Toler()*(0.5 + abs(xc/rex)))/alog(3)) > 0
		err = make_array(kmax + 1,type = etyp, value = sinf.xmax)
	endif else begin
		kmax = 0l
		err = make_array(kmax + 1,type = etyp)
	endelse
	p = make_array(kmax + 1,type = ftyp)
	q = p
	res = p
	k = 0
	kl = 1

	cofl = 1
	while (k lt kmax and cofl) do begin
		k = k + 1
		rex = rex/3
		xl = xc - rex
		xr = xc + rex
		fl = total(Rcall(fun,xl,par,inf=ifl,mult=rex,_extra=_e))
		fr = total(Rcall(fun,xr,par,inf=ifl,mult=rex,_extra=_e))
		xc = reform(transpose([[xl],[xc],[xr]]),3l^k)
		p[k] = 2*fc - fl - fr
		q[k] = fc + fl + fr
		fc = q[k]/3

		if p[k] eq 0 then begin
			res[k] = q[k]
			err[k] = 0
		endif else begin
			l = k
			while l gt kl do begin
				l = l - 1
				if p[l] eq p[k] then q[l] = q[l+1] else $
				q[l] = (p[l]*q[l+1] - p[k]*q[l])/(p[l] - p[k])
				nerr = abs(q[l+1] - q[l])
				if nerr gt err[k] then kl = l + 1 else err[k] = nerr
	    	endwhile
			res[k] = q[kl]
			if relf and err[k] lt abs(res[k]) then err[k] = err[k]/abs(res[k])
			if k gt kl + 1 then begin
				if err[k-1] lt err[k] < err[k-2] and ntr eq 0 then begin
					k = k - 1
					kmax = k
					stat = 2
				endif else ntr = ntr - 1
			endif
		endelse
		if err[k] lt eps then begin
			if verfl then begin
				if vcount then cofl = 0 else vcount = vcount + 1
			endif else cofl = 0
		endif else vcount = 0
	endwhile

	erv = FPU_fix(err[k])
	if erv lt eps then stat = 1
	return, FPU_fix(res[k])
end