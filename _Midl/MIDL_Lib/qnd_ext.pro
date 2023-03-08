Function QND_ext, fun, eps, x_ini = xin, params = par, step = stp, try = try, $
	min = mno, max = mxo, finegrad = fig, sumsq= ssq, yvals= yvl, weights= wei,$
	improve= imp, progress= prg, warn= wrn, extval=evl, error=err, status=sta,$
	_extra= _e

;+
; NAME:
;		QND_EXT
; VERSION:
;		8.42
; PURPOSE:
;		Finds extrema, max or min, of functions.
; CATEGORY:
;		Mathematical, general.
; CALLING SEQUENCE:
;		Result = QND_EXT( FUN [, EPS] [keywords])
; INPUTS:
;	FUN
;		Character value representing an existing IDL function.  Depending on
;		the setting of the keyword SUMSQ (see below), the function must satisfy
;		the following requirements:
;
;		SUMSQ not set:
;
;		1)	Accept vector input X and return scalar value.
;
;		SUMSQ set:
;
;		1)	Accept vector input X (size N) and return a vector output (size M).
;			M and N can be 1 as special case.
;
;		In addition FUN may accept one additional input (beyond X) of
;		arbitrary form, and an unlimited number of keywords.  Thus the general
;		form of the FUN defining line should be
;
;		Function FUN, X [, PAR] [, extra keywords]
; OPTIONAL INPUT PARAMETERS:
;	EPS
;		Smallness parameter, used to determine convergence.  Default is machine
;		precision, according to data type, as given by TOLER.  Rougly speaking,
;		the error of the result (assuming the procedure converged) is of the
;		order of SQRT(EPS).
; KEYWORD PARAMETERS:
;	X_INI
;		Initial X vector, to start the iteration.  Mandatory.
;	PARAMS
;		An arbitrary optional value or variable which is passed to the function
;		FUN.  No restrictions on type and form and no checking is done.
;	STEP
;		Initial size of iteration step.  Default value is 1.
;	TRY
;		Specifies how many consecutive times to ignore evidence of divergence
;		(meaning, instances where the new function increment is larger than the
;		previous one.  Default is the bigger of 3 and N_ELEMENTS(X_INI).
;	/MIN											|	At most one of these
;		Switch.  If set, only minima are sought.	|	may be specified.  If
;	/MAX											|	none is, the default
;		Switch.  If set, only maxima are sought.	|	is /MIN.
;	/FINEGRAD
;		Switch.  If set, double sided gradient estimates are evaluated.  This
;		may improve convergence, paying with speed.  See also /IMPROVE, below.
;	/SUMSQ
;		Switch.  If set, "sum squared" evaluation of the function is performed.
;		See details in FGH_FUN.
;	YVALS
;		Comparison vector for "sum squared" purposes, used within FGH_FUN when
;		SUMSQ is set. See details in FGH_FUN.
;	WEIGHTS
;		Numeric weights vector, used within FGH_FUN when SUMSQ is set. See
;		details in FGH_FUN.
;	/IMPROVE
;		Switch.  If set and if /FINEGRAD is not set, the calculation switches
;		to /FINEGRAD mode after an extremum is found, trying to improve it.
;	/PROGRESS
;		Switch.  If set, best function value obtained in each iteration is
;		printed to the screen.
;	/WARN
;		Switch.  If set, and if the calculation fails to converge, a warning
;		message is printed to the screen.
;	EXTVAL
;		Optional output, see below.
;	ERROR
;		Optional output, see below.
;	STATUS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to additional keywords to FUN.  Not to be used
;		directly.
; OUTPUTS:
;		Returns the extremum location, as vector, if found, else returns the
;		last location evaluated.
;		
;		Note:	If X_INI is scalar, the output will be a scalar as well.
; OPTIONAL OUTPUT PARAMETERS:
;	EXTVAL
;		Returns the value of the function at the extremum or (if not
;		converging), the last value calculated.
;	ERROR
;		Returns the iteration error, as estimated from the last iteration step.
;	STATUS
;		Returns iteration status information.  Possible values are:
;			0	:	Not converging.
;			-1	:	Converged to minimum.
;			1	:	Converged to maximum.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		QND_EXT is a simplified version of the routine FGH_EXT, to be used in
;		cases where only the function values (and not the Gradient and Hessian)
;		are available.  It uses alternating gradient estimation steps and 1-dim
;		optimization along the gradient direction from the previous step.
;		Calls CALCTYPE, CAST, DEFAULT, FGH_FUN, ISNUM, ONE_OF and TOLER, from 
;		MIDL.
; MODIFICATION HISTORY:
;		Created 10-MAY-2009 by Mati Meron.
;		Modified 25-JUN-2010 by Mati Meron.  Internal changes.
;		Modified 30-MAY-2015 by Mati Meron.  Internal changes.
;-

	on_error, 1

	x = Cast(xin,4)
	n = n_elements(x)
	typ = Calctype(x,0.)
	deps = 2*Toler(x)
	big = (machar(double=Isnum(x,/double))).xmax
	weps = Default(eps,deps,/dtyp) > deps
	wstp = Default(stp,1.,/dtyp)
	if One_of(mno,mxo) eq 1 then extyp = 1 else extyp = -1

	comp = big
	try = Default(try,3 > n) > 0
	rcmax = 30
	ntry = (gtry = 0)
	sta = 0
	fval = make_array(3,typ=typ)
	uvec = (pval = (mval = make_array(n,typ=typ)))
	uvec[n-1] = 1

	fifl = keyword_set(fig)
	fval[0] = FGH_fun(fun,x,par=par,sum=ssq,yva=yvl,wei=wei,_extra=_e)
	done = 0
	repeat begin
		rcount = 0
		repeat begin
			cor = -2
			repeat begin
				if fifl then mval = make_array(n,typ=typ) else mval = fval[0]
				for i = 0, n-1 do begin
					uvec = shift(uvec,1)
					pval[i]= FGH_fun(fun,x+wstp*uvec,par=par,$
						sum=ssq,yva=yvl,wei=wei,_extra=_e)
					if fifl then mval[i]= FGH_fun(fun,x-wstp*uvec,par=par,$
						sum=ssq,yva=yvl,wei=wei,_extra=_e)
				endfor
				grd = (pval - mval)/((1+fifl)*wstp)
				gnorm = sqrt(total(grd^2))
				if gnorm eq 0 then begin
					gtry = gtry + 1
					wstp = 2*wstp
				endif else begin
					gtry = 0
					dx = extyp*wstp*grd/gnorm
				endelse
			endrep until (gtry eq 0 or gtry gt try)
			if gtry gt try then break

			fval[1] = $
				FGH_fun(fun,x+dx,par=par,sum=ssq,yva=yvl,wei=wei,_extra=_e)
			fval[2] = $
				FGH_fun(fun,x+2*dx,par=par,sum=ssq,yv=yvl,we=wei,_extra=_e)
			if extyp eq -1 then fcomp = min(fval,ind) else fcomp = max(fval,ind)
			fir = total(fval*[3,-4,1])/2.
			sec = total(fval*[1,-2,1])
			if extyp*sec lt 0 then begin
				cor = fir/sec
				if cor lt 0 then begin
					x = x + ind*dx
					fval[0] = fcomp
					if not fifl then fifl= 1 else if ind eq 0 then wstp= wstp/2
					rcount = (0 < (-rcount)) - 1
				endif
			endif else begin
				x = x + ind*dx
				fval[0] = fcomp
				if ind eq 0 then wstp = 2*wstp
				rcount = (0 > (-rcount)) + 1
			endelse
			if abs(rcount) ge rcmax then break
		endrep until cor ge 0
		if gtry gt try or abs(rcount) ge rcmax then begin
			evl = fval[0]
			break
		endif

		err = cor*dx
		abserr = sqrt(total(err^2))
		evl = FGH_fun(fun,x+err,par=par,sum=ssq,yva=yvl,wei=wei,_extra=_e)
		ncomp = extyp*(evl - fcomp)/((abs(evl) + abs(fcomp))> deps)
		done = ncomp ge 0 and ncomp lt weps
		if done then begin
			if keyword_set(imp) and not fifl then begin
				done = 0
				fifl = 1
			endif
			x = x + err
			fval[0] = evl
			err = abserr
			sta = extyp
		endif else begin
			if (abs(ncomp) lt comp) then ntry = 0 else ntry = ntry + 1
			comp = abs(ncomp)
			if ncomp ge 0 then begin
				x = x + err
				fval[0] = evl
				wstp = abserr/2
			endif else begin
				x = x + ind*dx
				fval[0] = fcomp
				if ind eq 0 then wstp = (wstp < abserr)/2 
			endelse
			if wstp lt (weps > sqrt(weps*total(x^2))) then begin
				done = 1
				evl = fval[0]
				err = abserr
				sta = extyp
			endif
		endelse
		if keyword_set(prg) then print, evl, '	', ncomp
	endrep until (done or ntry gt try)

	if not done then begin
		if abs(rcount) ge rcmax then x = (err = !null) else err = abserr
		if keyword_set(wrn) then message, 'Failed to converge.', /con
	endif

	if Isnum(x) then begin
		x = Cast(x,typ,typ,/fix)
		if (size(xin))[0] eq 0 then x = x[0]		
	endif

	return, x
end