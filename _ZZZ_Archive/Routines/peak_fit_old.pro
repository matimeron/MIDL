Function Peak_fit_old, x, y, ser, fixed_err = fer, funct = fnc, edge = edg, $
	region = reg, center = cen, sigma = sig, amplitude = amp, background = bck,$
	show_fit= shf, net = net, psym = psm, dense = den, quiet = qui, $
	fit = fit, chi = chi, covmat = cvm, error = err, status = sta, _extra = _e

;+
; NAME:
;		PEAK_FIT
; VERSION:
;		8.40
; PURPOSE:
;		Fits the input to a sum of Gaussians, Lorentzians or Voigtians, with an 
;		optional background.
;		Optionally fits "edges" to integrals of Gaussians or Lerentzians (i.e.
;		error_functions and arctans).
; CATEGORY:
;		Mathematical, general.
; CALLING SEQUENCE:
;		Result = PEAK_FIT( X, Y, [, SER] [, keywords])
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
; 	FIXED_ERR
; 		Provides a fixed value for the data errors, in case the vector SER is
; 		not given (either explicitly, or implicitly as the third input column).
; 		If SER is present, FIXED_ERR is ignored.
; 		
; 		Note:	Setting FIXED_ERR, as .../FIXED_ERR,... sets the constant
; 				error to 1.
;	FUNCT
;		The function to use for fitting.  Currently accepted functional forms
;		are Gaussian, Lorentzian and Voigtian.  Default is Gaussian.
;		Note:	Voigtians are not available for edge fitting.
;	/EDGE
;		Switch.  If set, the fitting is done for "edges", i.e. integrals of
;		peaks, instead of peaks themselves.
;
;		Note:	Voigtian integral is not available in the EDGE mode.
;	REGION
;		Two-element numeric vactor specifying fitting region.  The default 
;		region is the full range of X.
;	CENTER
;		A scalar or vector, containing the initial guess for the center(s) of
;		the peak(s).  Optional when a single peak approximation is sought,
;		mandatory otherwise.
;	SIGMA
;		A scalar or vector, containing the initial guess for the sigma value(s)
;		of the peak(s).  Optional.  Note that in case of a Lorentzian(s),
;		"sigma" stands for the half-width.
;	AMPLITUDE
;		A scalar or vector, containing the initial guess for the amplitude(s)
;		of the peak(s).  Optional.
;	BACKGROUND
;		Integer scalar, specifies the type of the background.  Possible values:
;			1) 	-	Scalar background.
;			2)	-	Linear (in X) background.
;			3)	-	Quadratic (in X) background.
;	/SHOW_FIT
;		Switch.  If set, the fit parameters and their errors are printed to the
;		screen and both input values and the fit are plotted.  Also, the 
;		background is plotted separately, using dashed line.
;	/NET
;		Switch.  If set, and SHOW_FIT is set, the background isn't plotted, 
;		instead the input data and fit are plotted with the background 
;		subtracted.  If SHOW_FIT is not set, /NET has no effect.
;		
;		Note:  	If NET is provided with a name of a variable (which doesn't need
;				to be defined prior to the call) it'll serve as an optional
;				output.  See below for details.    
;	PSYM
;		Accepts an integer value specifiying the plot symbol (see the IDL PLOT
;		routine) to be used for plotting input data only (not for the fit).
;	DENSE
;		Accepts integer values which are cast into the range [1,6].  Specifies 
;		that the fit (see SHOW_FIT) is to be plotted with higher point density,
;		relative to the input data.  The density used is original density 
;		multiplied by 2^(DENSE).  Default value is 0.
;	/QUIET
;		Switch.  If set, and SHOW_FIT is set, suppresses printed output to the
;		screen (but not the plot).
;		
;		Note:	If SHOW_FIT is not set, PSYM, DENSE and QUIET have no effect.
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
;		/PROGRESS	:	Switch.  If set, the iteration progress is printed to
;						the screen.
; OUTPUTS:
;		Returns a numeric vector containing the parameters of the best fit
;		function, in the following order:
;
;		first 3 entries	: 	coefficients of the background polynomial in the
;							order of [zero power, first power, second power].
;		next 3 entries	:	parameters of the first peak, in the
;							[Amplitude, Center, Sigma] order.
;		next 3 entries	:	If present, parameters of the second peak, in same
;							order as the first.
;		etc.			:	...
;
;		In the case of a Voigt function fit there are four parameters for each
;		Voigtian, in the [Aplitude, Center, Sigma, Gamma] order.
;
;		IMPORTANT:		The convention used here is that the functions are 
;						normalized to an amplitude of 1 at maximum, not to an
;						integral of 1.
; OPTIONAL OUTPUT PARAMETERS:
; 	NET
; 		Normally serves as a switch, but when rpovided with a variable name, 
; 		returns the input data with the background subtracted.  If the input
; 		dat is in "packed" form (a [2,N] or [3,N] array, the returned data in
; 		NET will have same format.
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
;		performs Least Squares evaluation.  Calls MGAUSS_FUN, MLORENTZ_FUN or
;		MVOIGT_FUN in the standard mode, MIGAUSS_FUN or MILORENTZ_FUN in the
;		edge mode.  Calls CALCTYPE, CAST, DEFAULT, DIF, DTOF, FGH_EXT, HOW_MANY,
;		ISNUM, JOIN_XY, LINFIT_MM, POLEVAL, SIGN, SORPURGE, SPLIT_XY,
;		STRMATCH_MM, TABULATE, TOLER, TRUCOL and WHERINSTRUCT from MIDL.  Also
;		calls PEAK_CENT, PEAK_INT, PEAK_SMOOTH and PEAK_SWID from SPEC.
; MODIFICATION HISTORY:
;		Created 5-SEP-2008 by Mati Meron, based on GAUSSFIT_MM.
;		Modified 15-AUG-2010 by Mati Meron.  Added Voigt function  fitting.
;		Modified 15-JUN-2011 by Mati Meron.  Added edge fitting option.  Added 
;		keywords FIXED_ERR and EDGE.
;		Modified 25-NOV-2013 by Mati Meron.  Added keywords PSYM, DENSE and 
;		QUIET, and background plotting.
;		Modified 5-JUN-2014 by Mati Meron.  Added keyword NET.
;		Modified 25-DEC-2014 by Mati Meron.  Added keyword REGION.  Updated 
;		keyword NET to serve as optional output.
;		Modified 10-FEB-2015 by Mati Meron.  Internal change.
;-

	on_error, 1

	efl = keyword_set(edg)
	if efl then begin
		posib = ['gauss','lorentz']
		fnam = ['migauss_fun','milorentz_fun']
	endif else begin
		posib = ['gauss','lorentz','voigt']
		fnam = ['mgauss_fun','mlorentz_fun','mvoigt_fun']
	endelse
	tit = ['Gaussian','Lorentzian','Voigtian'] + ' parameters'
	sigcoef = 1/sqrt([!dpi,!dpi^2,!dpi])
	ampcoef = sqrt([2d,4d,2d])

	if n_elements(fnc) gt 0 then begin
		ftyp = Strmatch_mm(fnc,posib,3)
		if ftyp eq -1 then message, 'Unrecognized function!'
	endif else ftyp = 0
	vfl = ftyp eq 2
	fnam = fnam[ftyp]

	n = Split_xy(x,y,ser,x=wx,y=wy,z=wser,inpz=inz)
	typ = Calctype(wx,wy)
	feps = Toler(wx)
	btyp = 0 > Default(bck,0,/dtyp) < 3

	yoff = min(wy) < 0
	if yoff lt 0 then begin
		btyp = btyp > 1
		twy = wy - yoff
	endif else twy = wy
	if not inz then if keyword_set(fer) then wser=(0*wx+fer) else wser=sqrt(twy)

	if efl then s = Sorpurge(wx) else s = sort(wx)
	wx = wx[s]
	wy = wy[s]
	twy = twy[s]
	wser = wser[s]

	if keyword_set(reg) then begin
		if n_elements(reg) eq 2 then begin
			dum = where(wx ge min(reg) and wx le max(reg),ndum)
			if ndum gt 0 then begin
				wx = wx[dum]
				wy = wy[dum]
				twy = twy[dum]
				wser = wser[dum]
			endif else message, 'Unacceptable region!'
		endif else message, 'Region needs two values!'
	endif

	dum = where(wser le 0,ndum,comp=cdum,ncomp=ncdum)
	if ndum gt 0 then begin
		if ncdum gt 0 then wmin = min(wser[cdum]) else wmin = 1
		wser[dum] = wmin
	endif
	wei = 1/wser^2

	if efl then begin
		wsgn = Sign((Linfit_mm(wx,twy))[1],/noz)
		wder = Peak_smooth(wx,wsgn*Dif(twy,/lin)/Dif(wx,/lin),wid=n/10,/bin)
		ttwy = reform(wder[1,*])
	endif else begin
		wsgn = 1
		ttwy = twy
	endelse
	wcen = Peak_cent(wx,ttwy,wser)
	wwid = Peak_swid(wx,ttwy,wser)
	wsig = Cast(sigcoef[ftyp],typ,typ)*wwid/2
	wamp = wsgn*Cast(ampcoef[ftyp],typ,typ)*Peak_int(wx,ttwy,wser)/wwid

	wha = How_many(fir=cen,sec=sig,thi=amp,whi=whi)
	if wha gt 0 then begin
		if whi and 1 then begin
			nc = n_elements(cen)
			wcen = Cast(cen,4)
			if (whi and 3) eq 3 then begin
				ns = n_elements(sig)
				if ns eq nc then begin
					tsig = Cast(sig,4)
					fac = sqrt(total(tsig^2)/nc)/wsig
					wsig = tsig
				endif else message, 'Sigma length error!'
			endif else begin
				tem = wsig^2 - total(wcen^2)/nc + (total(wcen)/nc)^2
				if tem gt 0 then fac = sqrt(tem)/wsig else fac = 1./sqrt(nc)
				wsig = replicate(fac*wsig,nc)
			endelse
			if (whi and 5) eq 5 then begin
				na = n_elements(amp)
				if na eq nc then wamp = Cast(amp,4) $
				else message, 'Amp. length error!'
			endif else wamp = replicate(wamp,nc)/(fac*nc)
		endif else message, 'Missing Center data!'
	endif else nc = 1

	kb = 3
	kp = 3 + vfl
	ind = kb + kp*lindgen(nc)
	ipar = dblarr(kb + kp*nc)
	ipar[ind] = wamp
	ipar[ind+1] = wcen
	ipar[ind+2] = wsig

	msk = lonarr(kb + kp*nc) + 1
	valc = dblarr(kb + kp*nc)
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
	if vfl then rpar[ind+3] = abs(rpar[ind+3])
	dinp = Isnum(wx,/double)
	rpar = Cast(DtoF(rpar,ignore=dinp),typ,typ,/fix)
	err = Cast(DtoF(err,ignore=dinp),typ,typ,/fix)
	if Isnum(cvm) then cvm = Cast(DtoF(cvm,ignore=dinp),typ,typ,/fix)
	fit = call_function(fnam,rpar,wx)

	if not sta then message, 'Warning, no convergence, results unreliable!',/con

	if keyword_set(shf) then begin
		if not keyword_set(qui) then begin
			print
			print,'	Chi squared = ', chi, form = '(a,g13.6)'
			print
			tit = tit[ftyp]
			if vfl then begin
				head = ['Amplitude','Amp. err','Center','Cent. err',$
						'Sigma','Sig. err','Gamma','Gam. err']
				form = ['g11.6','g9.5','g11.6','g9.5',$
						'g11.6','g9.5','g11.6','g9.5']
				Tabulate, tit= tit, head= head, form=form, $
				rpar[ind],err[ind],rpar[ind+1],err[ind+1],$
				rpar[ind+2],err[ind+2],rpar[ind+3],err[ind+3]
			endif else begin
				head = ['Amplitude','Amp. err','Center','Cent. err',$
						'Sigma','Sig. err']
				form = ['g13.6','g11.5','g13.6','g11.5','g13.6','g11.5']
				Tabulate, tit= tit, head= head, form=form, $
				rpar[ind],err[ind],rpar[ind+1],err[ind+1],rpar[ind+2],err[ind+2]
			endelse
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
		endif

		wdx = wx
		if keyword_set(den) then begin
			wden = 1 > fix(den) < 6
			for i = 0, wden-1 do begin
				wdx = [wdx,(wdx[0:-2] + wdx[1:-1])/2]
				wdx = wdx[Sorpurge(wdx)]
			endfor
			pfit = call_function(fnam,rpar,wdx)
		endif else pfit = fit

		if keyword_set(net) or arg_present(net) then begin
			wy = wy - Poleval(wx,rpar[0:2])
			pfit = pfit - Poleval(wdx,rpar[0:2])
			nfl = 1
			if arg_present(net) then begin
				siz = size(reform(x))
				if siz[0] eq 2 then begin
					if siz[1] eq 2 then net = Join_xy(wx,wy) $
					else net = Join_xy(wx,wy,wser)
				endif else net = wy
			endif
		endif else nfl = 0

		if Wherinstruct('ylo',_e) ge 0 then begin
			dify = abs(Dif(wy[sort(wy)],/clip))
			dum = where(dify gt 0, ndum)
			if ndum eq 0 then pmin = wy[0] else pmin = min(dify[dum])
			yran = [pmin < 1, max(wy) > max(pfit)]
		endif else if Wherinstruct('yno',_e) lt 0 $
		then yran = [0 < min(wy) < min(pfit), max(wy) > max(pfit)]

		tru = Trucol([[4,0,0],[0,4,0],[0,0,4]])
		plot, wx, wy, yran = yran, /nodata, _extra = _e
		oplot, wx, wy, col = tru[0], thi = 2, psym = psm
		oplot, wdx, pfit, col = tru[1], thi = 2
		if not nfl then oplot, wdx, Poleval(wdx,rpar[0:2]), col = tru[2], line=2
	endif

	return, rpar
end