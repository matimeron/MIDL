Function Root_old, fun, range, eps, relative = rel, params = par, multi = mult, $
	error = ervl, status = stat, done = don, _extra = _e

;+
; NAME:
; 		ROOT
; VERSION:
;		8.3
; PURPOSE:
;		Finds roots of real functions.
; CATEGORY:
;		Mathematical function (general).
; CALLING SEQUENCE:
;		Result = ROOT( FUN, RANGE [, EPS [, keywords]])
; INPUTS:
;	FUN
;		Character value representing an existing IDL function.  The function
;		must return scalar values, when provided with scalar input.  It is not
;		necessary for FUN to be able to accept an array input and return an
;		array output.  The calling sequence for the function must be either
;			Result = FUN(x)
;		or
;			Result = FUN(x, extra)
;		where X is the variable and EXTRA may be any single entity (scalar,
;		array, or structure) used to pass additional parameters to the function.
;		In addition, the function may accept any number of keywords.
;	RANGE
;		Two element vector, search range.
; OPTIONAL INPUT PARAMETERS:
;	EPS
;		Allowed error.  Default is machine precision, according to data type,
;		as given by TOLER.  EPS is understood to represent absolute error
;		unless the keyword RELATIVE is set.  The allowed error is dynamically
;		adjusted during calculation.
; KEYWORD PARAMETERS:
;	/RELATIVE
;		If set, EPS represent the allowed relative (to the size of RANGE) error.
;	PARAMS
;		An arbitrary value or variable which is passed to the function FUN.
;	MULTI
;		Specifies multiplicity of search (i.e. what is the maximal number of
;		roots to look for.  Default is 1.  If MULTI is set to -1 (or any
;		negative number) the search is unlimited.
;	ERROR
;		Optional output, see below.
;	STATUS
;		Optional output, see below.
;	DONE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to additional keywords to FUN.  Not to be used
;		directly.
; OUTPUTS:
;		Returns a vector containing the location(s) of the root(s).  The
;		numerical type of the result (floating, or double) is determined by the
;		numerical type of RANGE.  If no root is found, returns machine max.
; OPTIONAL OUTPUT PARAMETERS:
;	ERROR
;		The name of the variable to receive the estimated error of the root
;		location.  Returned as vector, same length as the function output.
;	STATUS
;		The name of the variable to receive search status information.
;		Returned as vector, same length as the function output.
;		Possible values are:
;			0 - No root found.
;			1 - OK.
;			2 - Search converged, but the result appears to be a singularity.
;				This may happen with nonsingular functions if the precision
;				demands are set to high.
;	DONE
;		The name of the variable to receive search completion information.
;		Possible values are:
;			0 - Indicates that the number of roots prescribed by MULTI has been
;				found without all of RANGE being searched.  This does not
;				necessarily mean that there are more roots.  Disabled for
;			MULTI = 1, or negative.
;			1 - Indicates that search has been completed, i.e. all the roots
;				that could be found have been found.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Uses Golden section search  for minima of ABS(FUN(X)).  When an
;		interval where F(X) changes sign is identified, uses weighted interval
;		halving to pinpoint the root.
;		Uses CALCTYPE, CAST, DEFAULT, FPU_FIX, ISNUM, RCALL and TOLER from MIDL.
; MODIFICATION HISTORY:
;		Created 15-FEB-92 by Mati Meron.
;		Modified 15-APR-92 by Mati Meron.  Added Quadratic Interpolation to
;		speed up convergence for smooth functions.
;		Modified 25-JAN-94 by Mati Meron.  Added multi-root search capability.
;		Quadratic Interpolation replaced by a weighted halving to increase
;		robustness without loosing speed.
;		Modified 5-OCT-1998 by Mati Meron.  Underflow filtering added.
;		Checked for operation under Windows, 30-JAN-2001, by Mati Meron.
;		Modified 5-MAY-2014 by Mati Meron.  Implemented keyword transfer to FUN
;		through _EXTRA, thus combining the old ROOT and EROOT.
;-

	on_error, 1

	if n_elements(range) ne 2 then message, 'Improper range!'
	isdob = Isnum(range,/doub,typ=rtyp) or Isnum(par,/doub,typ=ptyp)
	rtyp = Calctype(rtyp,ptyp,4,def=4,/types)
	rlis = Cast(range,4+isdob)
	if rlis[0] gt rlis[1] then rlis = reverse(rlis)
	sinf = machar(double=isdob)
	deps = Toler(type=rtyp)
	weps = abs(Default(eps,deps,/dtype))
	if keyword_set(rel) then weps = weps*(rlis[1] - rlis[0])
	weps = weps > deps*max(abs(rlis))

	rlis = rlis + 2*[-weps,weps]
	ervl = [weps,weps]
	stat = intarr(2)
	pfl = n_elements(par) ne 0
	cmul = Default(mult,1,/dtype)
	if cmul eq 1 then mfl = 0 else mfl = 1
	gold = (sqrt(Cast(5,rtyp,rtyp)) - 1)/2
	ncur = 1

	while ncur lt n_elements(rlis) and cmul ne 0 do begin
		csta = 0
		x = rlis[ncur-1:ncur] + (ervl[ncur-1:ncur] > weps)*[2,-2]
		if (x[1] - x[0]) ge weps then begin
			f = x
			f[0] = Rcall(fun,x[0],par,_extra=_e)
			f[1] = Rcall(fun,x[1],par,_extra=_e)
			x = x([0,1,1])
			f = f([0,1,1])
			if f[0]*f[1] gt 0 and mfl then begin
				x[1] = (1 - gold)*x[0] + gold*x[2]
				f[1] = Rcall(fun,x[1],par,_extra=_e)
				if f[0]*f[1] gt 0 then begin
					repeat begin
						x = x([0,0,1,2])
						f = f([0,0,1,2])
						x[1] = (1 - gold)*x[0] + gold*x[2]
						f[1] = Rcall(fun,x[1],par,_extra=_e)
						if f[0]*f[1] gt 0 then begin
							comp = f[2]/f[1]
							if stat[ncur-1] ne 0 then comp = $
								comp*(x[1] - rlis(ncur-1))/(x[2] - rlis[ncur-1])
							if stat[ncur] ne 0 then comp = $
								comp*(x[1] - rlis[ncur])/(x[2] - rlis[ncur])
							if comp gt 1 then ind = [0,1,2] else ind = [3,2,1]
							x = x[ind]
							f = f[ind]
						endif else csta = 1
					endrep until csta or abs(x[2] - x[0]) lt weps
				endif else csta = 1
			endif else csta = f[0]*f[1] le 0
		endif

		if csta then begin
			if f[0]*f[1] eq 0 then begin
				if f[0] eq 0 then x[1] = x[0]
				cerv = 0
			endif else begin
				ceps = weps
				cerv = abs(x[1] - x[0])
				cfl = 0
				pow = 0.5
				while cerv ge ceps do begin
					ind =  [0,0,1] + cfl
					x = x[ind]
					f = f[ind]
					wf = abs(f([2,0]))^pow
					wfx = x([0,2])*wf
					x[1] = (wfx[0] + wfx[1])/(wf[0] + wf[1])
					f[1] = Rcall(fun,x[1],par,_extra=_e)
					if f[1] ne 0 then begin
						cfl = f[0]*f[1] gt 0
						nerv = abs(x[cfl+1] - x[cfl])
						erat = nerv/cerv
						if erat lt 0.5 then pow = 2*pow < 1. else pow = pow/2
						if erat eq 1 then ceps = 2*ceps
						cerv = nerv
					endif else cerv = 0
				endwhile
				if f[1] ne 0 and (f[1] - f[0])*(f[1] - f[2]) gt 0 then csta = 2
			endelse
			rlis = FPU_fix([rlis[0:ncur-1],x[1],rlis[ncur:*]])
			ervl = FPU_fix([ervl[0:ncur-1],cerv,ervl[ncur:*]])
			stat = [stat[0:ncur-1],csta,stat[ncur:*]]
			cmul = cmul - 1
			if not mfl then ncur = 3
		endif else ncur = ncur + 1
	endwhile

	if ncur lt n_elements(rlis) and cmul eq 0 then don = 0 else don = 1
	found = where(stat ne 0, nfound)
	if nfound gt 0 then begin
		rlis = rlis[found]
		ervl = ervl[found]
		stat = stat[found]
	endif else begin
		rlis = sinf.xmax
		ervl = abs(range[1] - range[0])
		stat = 0
	endelse

	return, rlis
end