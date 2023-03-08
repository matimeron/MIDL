Function X2_fit, t, x2, range = ran, dval = dvl, fval = fvl, offset = off, $
	show_fit = shf, sqt = sqt, fit = fit, chi = chi, error = err, status = sta,$
	_extra = _e

;+
; NAME:
;		X2_FIT
; VERSION:
;		5.2
; PURPOSE:
;		Fits the input to a function of the form of 2*D*T/(1 + D/F*sqrt(T)) + C.
; CATEGORY:
;		Mathematical, general.
; CALLING SEQUENCE:
;		Result = X2_FIT( T, X2, [, keywords])
; INPUTS:
;	T
;		Numeric, A vector (scalar is considered to be a vector of length 1), or
;		an [2,*] array.
; OPTIONAL INPUT PARAMETERS:
;	X2
;		Numeric, acceptable only when T is a vector, in which case X2 has to be
;		a vector of same length.
;
;		Note:	If only one input is provided then:
;				1)	If it is a vector, it is taken to be X2 and T is generated
;					internally with a spacing of 1.
;				2)	If it is a [2,*] array, it is split into T and X2 vectors.
; KEYWORD PARAMETERS:
;	RANGE
;		Specifies a T-range for fitting purposes.  Normally given as a
;		element vector containing the [min,max] of the range.  If given as
;		a single value, the fitting is from this value up.  If not given, the
;		whole range of T is used.
;	DVAL												|
;		An optional *fixed* value for the D parameter.	|	No more than one
;	FVAL												|	of these two can
;		An optional *fixed* value for the F parameter.	|	be used.
;	OFFSET
;		An optional *fixed* value for the offset (C) parameter.
;	/SHOW_FIT
;		Switch.  If set, the fit parameters and their errors are printed to the
;		screen and both input values and the fit are plotted.
;	/SQT
;		Switch.  If set and /SHOW_FIT is set, the data is plotted as a function
;		of SQRT(T).
;	FIT
;		Optional output, see below.
;	CHI
;		Optional output, see below.
;	ERROR
;		Optional output, see below.
;	STATUS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to FGH_EXT, as well as
;		plotting keywords.  Not to be used directly.  Keywords of interest are
;
;		TRY	:			Specifes number of iteration tries before giving up.
;						Default is 10.
;		/PROGRESS	:	Switch.  If set, the iteration progress is printed to \
;						the screen.
; OUTPUTS:
;		Returns a 3-element numeric vector containing the parameters of the
;		best fit function, in the following order:
;
;		0-element	: The D value, for the function.
;		1-element	: The F value, for the function.
;		2-element	: The offset from 0 value of the function.
; OPTIONAL OUTPUT PARAMETERS:
;	FIT
;		Returns the values of the fitted function for T using the calculated
;		parameters.
;	CHI
;		Returns the calculated Chi squared per degree of freedom.
;	ERROR
;		List of the errors of the result parameters, in same order as the
;		result.
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
;		Fits the input to a function of the form of
;			2*D*T/(1 + D/F*sqrt(T)) + Off .
;		Calls X2_FUN.  Calls DEFAULT, FGH_EXT, FPU_FIX, ISNUM, LINFIT_MM,
;		ONE_OF, SPLIT_XY and TRUCOL, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-MAY-2004 by Mati Meron.
;		Modified 1-OCT-2004 by Mati Meron.  Added keywords DVAL and FVAL.
;		Modified 1-DEC-2005 by Mati Meron.  Added keyword OFFSET.
;-

	on_error, 1
	bas = replicate('Power_mm',3)
	par = [1,2,4]
	fnam = 'X2_fun'

	n = Split_xy(t,x2,x=wt,y=wx2)
	fit = 0*wx2
	dum = where(wt gt 0 and wx2 gt 0, ndum)
	if ndum gt 0 then begin
		wt = wt[dum]
		wx2 = wx2[dum]
		wr = Default(ran,[0,max(wt)])
		if n_elements(wr) eq 1 then wr = [wr,max(wt)]
		ddum = where(wt ge wr[0] and wt le wr[1],nddum)
		if nddum gt 0 then begin
			lpar = Linfit_mm(1/sqrt(wt[ddum]),1./wx2[ddum],wx2[ddum], $
				base=bas,param=par,resid=lchi,error=lerr)
			msk = [1,1,1]
			fixd = One_of(fvl,dvl,val=val)
			if fixd ge 0 then begin
				lpar[fixd] = 1./(2*val)
				msk[fixd] = 0
			endif
			if Isnum(off) then begin
				lpar[2] = off
				msk[2] = 0
			endif
			fpar = FGH_ext(fnam,/min, $
				x_ini=[lpar[0:1]>0,-lpar[2]/lpar[1]^2], par=wt[ddum], $
				mask=msk,/sum,yvals=wx2[ddum],wei=1/wx2[ddum]^2,$
				error=ferr,stat=sta,chi=chi,_extra=_e)
			par = [1/(2*fpar[[1,0]]),fpar[2]]
			err = [ferr[[1,0]]/(2*fpar[[1,0]]^2),ferr[2]]
			fit[dum] = call_function(fnam,fpar,wt)

			if keyword_set(shf) then begin
				nam = '	' + ['D','F','Off'] + '	= '
				print
				print,'	Chi squared = ', chi, form = '(a,g13.6)'
				print
				for i = 0, 2 do print, nam[i], par[i], err[i], $
				form = '(a,g13.6,"	(",g13.6,")")'
				print

				tru = Trucol([[4,0,0],[0,4,0]])
				if keyword_set(sqt) then pwt = sqrt(wt) else pwt = wt
				plot, pwt, wx2, /nodata, _extra = _e
				oplot, pwt, wx2, col = tru[0], line = 2, thi = 2
				oplot, pwt[ddum], wx2[ddum], col = tru[0], thi = 2
				oplot, pwt, fit[dum], col = tru[1]
			endif
		endif else message, 'Unacceptable range!'
	endif else message, 'Nothing to fit!'

	return, FPU_fix(par)
end