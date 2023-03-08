Function Qchain_fit, x, y, gval = gvl, der = der, $
	c_ini = cin, mask = msk, weights = wei, status = sta, _extra = _e 

;+
; NAME:
;		QCHAIN_FIT
; VERSION:
;		8.15
; PURPOSE:
;		Finds the curvature parameters of a QCHAIN function (see QCHAIN).
; CATEGORY:
;		Mathematical Function (specialized).
; CALLING SEQUENCE:
;		Result = QCHAIN_FIT (X, Y, GVAL = GVL, [, /DER, keywords])
; INPUTS:
;	X
;		Numerical vector.  Mandatory.
;	Y
;		Numerical vector, same length as X.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	GVAL
;		Numeric vector, containing the "grid" values.  Mandatory.  The number
;		of entries in GVAL must be >= 2 and <= n_elements(x) + 1.
;	/DER
;		Switch.  If set, the input Y is assumed to represent a derivative of a
;		QCHAIN function.  By default Y is assumed to represent a QCHAIN function
;	C_INI
;		A set of initial curvature values to start the fitting.  Optional.
;	MASK
;		Fitting mask.  See FGH_EXT for details.  Default is no mask.
;	WEIGHTS
;		Fitting weights.  See FGH_EXT for details.  Default is no weights.
;	STATUS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to FGH_EXT.  Not to be used
;		directly.
; OUTPUTS:
; 		Returns of vector of length n_elements(GVAL) - 1 representing the local
; 		curvatures of the QCHAIN function.  See the CURV keyword in QCHAIN.
; OPTIONAL OUTPUT PARAMETERS:
;	STATUS
;		Returns iteration status information.  Possible values are:
;			0	:	Not converging and/or Hessian indicates no extremum.
;			-1	:	Converged to minimum.
;			1	:	Converged to maximum.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Fits the X-Y inputs to a QCHAIN function using the local curvatures as
; 		the fit parameters.  Calls QCHAIN_FUN from BLINE_LIB.  Calls CALCTYPE, 
; 		CAST, DEFAULT, FGH_EXT and HOW_MANY from MIDL.
; MODIFICATION HISTORY:
;		Created 10-FEB-2012 by Mati Meron.
;-

	on_error, 1

	np = n_elements(x)
	if np eq n_elements(y) then typ = Calctype(x,y,0.) $
	else message, 'Input mismatch!'

	gl = min(gvl,max=gh)
	if gl eq gh then message, 'Constant GVAL not acceptable!'
	s = sort(gvl)
	g = Cast(gvl[s],typ)
	ng = n_elements(g)
	if np lt ng-1 then message, "insufficient data!

	wder = keyword_set(der)
	wha = How_many(fir=cin,sec=msk,whi=whi)
	if whi eq 2 then message, "Can't have MASK without C_INI!"
	wcin = Default(cin,replicate(0.,ng-1),low=typ)

	res = FGH_ext('Qchain_fun',x_ini=wcin,par=x,gval=gvl,der=wder,$
		/min,mask=msk,/sum,yvals=y,wei=wei,stat=sta,_extra=_e)

	return, Cast(res,typ,typ,/fix)
end