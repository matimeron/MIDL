Function Gaussfit_mm, x, y, ser, $
	center= cen, sigma= sig, amplitude= amp, background= bck, show_fit= shf, $
	fit= fit, chi = chi, covmat = cvm, error = err, status = sta, _extra = _e

;+
; NAME:
;		GAUSSFIT_MM
; VERSION:
;		4.9
; PURPOSE:
;		Fits the input to a sum of Gaussians, with an optional background.
; CATEGORY:
;		Mathematical, general.
; CALLING SEQUENCE:
;		Result = GAUSSFIT_MM( X, Y, [, SER] [, keywords])
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
;	CENTER
;		A scalar or vector, containing the initial guess for the center(s) of
;		the Gaussian(s).  Optional when a single Gaussian approximation is
;		sought, mandatory otherwise.
;	SIGMA
;		A scalar or vector, containing the initial guess for the sigma value(s)
;		of the Gaussian(s).  Optional.
;	AMPLITUDE
;		A scalar or vector, containing the initial guess for the amplitude(s)
;		of the Gaussian(s).  Optional.
;	BACKGROUND
;		Integer scalar, specifies the type of the background.  Possible values:
;			1) 	-	Scalar background.
;			2)	-	Linear (in X) background.
;			3)	-	Quadratic (in X) background.
;	/SHOW_FIT
;		Switch.  If set, the fit parameters and their errors are printed to the
;		screen and both input values and the fit are plotted.
;	FIT
;		Optional output, see below.
;	CHI
;		Optional output, see below.
;	COVMAT
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
;		Returns a numeric vector containing the parameters of the best fit
;		function, in the following order:
;
;		first 3 entries	: 	coefficients of the background polynomial in the
;							order of [zero power, first power, second power].
;		next 3 entries	:	parameters of the first Gaussian, in the
;							[Amplitude, Center, Sigma] order.
;		next 3 entries	:	If present, parameters of the second Gaussian, in
;							same order as the first.
;		etc.			:	...
; OPTIONAL OUTPUT PARAMETERS:
;	FIT
;		Returns the values of the fitted function for X using the calculated
;		parameters.
;	CHI
;		Returns the calculated Chi squared per degree of freedom.
;	COVMAT
;		Returns the full covariance matrix of the return parameters.
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
;		Estimates the initial value of the parameters (unless provided) and
;		performs Least Squares evaluation.  Calls MGAUSS_FUN.  Calls CALCTYPE,
;		CAST, DEFAULT, DTOF, FGH_EXT, HOW_MANY, ISNUM, SIGN, SPLIT_XY, TABULATE,
;		TOLER, TRUCOL and WHERINSTRUCT from MIDL.  Also calls PEAK_CENT,
;		PEAK_INT and PEAK_SIG from SPEC.
; MODIFICATION HISTORY:
;		Created 25-JAN-2004 by Mati Meron.
;		Modified 5-MAY-2004 by Mati Meron.  Internal changes only.
;-

	on_error, 1
	fnam = 'mgauss_fun'

	n = Split_xy(x,y,ser,x=wx,y=wy,z=wser,inpz=inz)
	typ = Calctype(wx,wy)
	feps = Toler(wx)
	btyp = 0 > Default(bck,0,/dtyp) < 3

	yoff = min(wy) < 0
	if yoff lt 0 then begin
		btyp = btyp > 1
		twy = wy - yoff
	endif else twy = wy
	if not inz then wser = sqrt(twy)

	dum = where(wser le 0,ndum,comp=cdum,ncomp=ncdum)
	if ndum gt 0 then begin
		if ncdum gt 0 then wmin = min(wser[cdum]) else wmin = 1
		wser[dum] = wmin
	endif
	wei = 1/wser^2

	wcen = Peak_cent(wx,twy,wser)
	wsig = Peak_sig(wx,twy,wser)
	wamp = Peak_int(wx,twy,wser)/(sqrt(2*!pi)*wsig)

	wha = How_many(fir=cen,sec=sig,thi=amp,whi=whi)
	if wha gt 0 then begin
		if whi and 1 then begin
			nc = n_elements(cen)
			wcen = Cast(cen,4)
			if (whi and 3)/(whi > 3) then begin
				ns = n_elements(sig)
				if ns eq nc then begin
					tsig = Cast(sig,4)
					fac = sqrt(total(tsig^2)/nc)/wsig
					wsig = tsig
				endif else message, 'Sigma length error!'
			endif else begin
				tem = wsig^2 - total(wcen^2)/nc + (total(wcen)/nc)^2
				if tem gt 0 then fac = sqrt(tem)/wsig else esig = 1./nc
				wsig = replicate(fac*wsig,nc)
			endelse
			if (whi and 5)/(whi > 5) then begin
				na = n_elements(amp)
				if na eq nc then wamp = Cast(amp,4) $
				else message, 'Amp. length error!'
			endif else wamp = replicate(wamp,nc)/(fac*nc)
		endif else message, 'Missing Center data!'
	endif else nc = 1

	ind = 3*(lindgen(nc) + 1)
	ipar = dblarr(3*(nc+1))
	ipar[ind] = wamp
	ipar[ind+1] = wcen
	ipar[ind+2] = wsig

	msk = lonarr(3*(nc+1)) + 1
	valc = dblarr(3*(nc+1))
	valc[0] = yoff
	if btyp lt 3 then msk[btyp:2] = 0

	if nc gt 1 then begin
		msk[ind+1] = 0
		valc[ind+1] = ipar[ind+1]
		rpar = FGH_ext(fnam,feps,x_ini=ipar,par=wx,/min,mask=msk,val=valc,$
		/sum,yvals=wy,wei=wei,stat=sta,_extra=_e)
		msk[ind+1] = 1
		rpar = FGH_ext(fnam,feps,x_ini=rpar,par=wx,/min,mask=msk,$
		/sum,yvals=wy,wei=wei,error=err,stat=sta,chi=chi,covmat=cvm,_extra=_e)
	endif else 	rpar = FGH_ext(fnam,feps,x_ini=ipar,par=wx,mask=msk,val=valc,$
	/min,/sum,yvals=wy,wei=wei,error=err,stat=sta,chi=chi,covmat=cvm,_extra=_e)

	rpar[ind+2] = abs(rpar[ind+2])
	dinp = Isnum(wx,/double)
	rpar = Cast(DtoF(rpar,ignore=dinp),typ,typ,/fix)
	err = Cast(DtoF(err,ignore=dinp),typ,typ,/fix)
	if Isnum(cvm) then cvm = Cast(DtoF(cvm,ignore=dinp),typ,typ,/fix)
	fit = call_function(fnam,rpar,wx)

	if not sta then message, 'Warning, no convergence, results unreliable!',/con

	if keyword_set(shf) then begin
		print
		print,'	Chi squared = ', chi, form = '(a,g13.6)'
		print
		tit = 'Gaussian parameters'
		head = ['Amplitude','Amp. err','Center','Cent. err','Sigma','Sig. err']
		form = ['g13.6','g11.5','g13.6','g11.5','g13.6','g11.5']
		tabulate, tit= tit, head= head, form=form, $
		rpar[ind],err[ind],rpar[ind+1],err[ind+1],rpar[ind+2],err[ind+2]
		print

		if btyp gt 0 then begin
			print, strjoin(replicate('_',80))
			print
			print, 'Background coeff(s)',rpar[0:btyp-1], $
			form = "(a,'	:',3(' ',g13.6))"
			print, 'Coefficient error(s)', err[0:btyp-1], $
			form = "(a,'	:',3('   ',g11.5))"
			print
		endif

		if Wherinstruct('ylo',_e) ge 0 then begin
			dify = abs(Dif(wy[sort(wy)],/clip))
			dum = where(dify gt 0, ndum)
			if ndum eq 0 then pmin = wy[0] else pmin = min(dify[dum])
			yran = [pmin < 1, max(wy)]
		endif else yran = [0,0]

		tru = Trucol([[4,0,0],[0,4,0]])
		plot, wx, wy, yran = yran, /nodata, _extra = _e
		oplot, wx, wy, col = tru[0]
		oplot, wx, fit, col = tru[1]
	endif

	return, rpar
end