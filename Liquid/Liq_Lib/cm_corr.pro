Function CM_corr, x, y, lag_range = lag, xmask = xm, ymask = ym, $
	no_zero_mean = nzm, running_mean = rum, $
	lvalues = wlag, lcount = lcon, variance = var

;+
; NAME:
;		CM_CORR.
; VERSION:
;		8.15
; PURPOSE:
;		Calculate statistical correlations, similar to IDL C_CORRELATE.
; CATEGORY:
;		Statistical function.
; CALLING SEQUENCE:
;		Result = CM_CORR( X [, Y] [keywords])
; INPUTS:
;	X
;		Numeric vector.
; OPTIONAL INPUT PARAMETERS:
;	Y
;		Numeric vector.  If given, must be same length as X.  If not given, is
;		taken as equal to X.
; KEYWORD PARAMETERS:
;	LAG_RANGE
;		The range of lag values to be used in the calculation.  Can be given as
;		a scalar, 2-element vector or multi-element vector.  The lag values
;		used internally are, accordingly:
;		Scalar:					The (single) value of LAG_RANGE is used.
;		1-element vector:		All the integer values between 0 and LAG_RANGE
;								are used.
;		2-element vector:  		All the integer values between MIN(LAG_RANGE)
;								and MAX(LAG_RANGE) are used.
;		Multi-element vector:	All the values in LAG_RANGE are used.
;		
;		If LAG_RANGE is not given, it defaults to 0.
;	XMASK
;		The mask to be used in conjuction with the X-vector.  Provides the
;		ability to skip over "bad" entries.  If given, must be of same length
;		as X, with entries of 0 for bad alements and 1 for good ones.  Defaults
;		to all 1s.
;	YMASK
;		Same as XMASK, for the Y vector.
;	/NO_ZERO_MEAN
;		Switch.  If set, the data is assumed  to be already normalized to zero
;		mean.  Else, it'll be normalized during the calculation.
;	/RUNNING_MEAN
;		Switch.  If set, *and* if /NO_ZERO_MEAN is *not* set, the means are
;		recalculated for each lag value, according to the specific elements
;		used in the calculation.
;	LVALUES
;		Optional output, see below.
;	LCOUNT
;		Optional output, see below.
;	VARIANCE
;		Optional output, see below.
; OUTPUTS:
;		Returns the cross correlation of X and Y.  If more than one lag value
;		is used, the result is a vector with one entry for each lag value.
; OPTIONAL OUTPUT PARAMETERS:
;	LVALUES
;		Returns the set of lag values used in the calculation.  This does not
;		have to coincide with the values provided by LAG_RANGE as for some of
;		these values there may be no valid (see XMASK, YMASK) data points.
;	LCOUNT
;		Returns the number of lag values used.  This is the same as
;		N_ELEMENTS(Result) or N_ELEMENTS(LVALUES).
;	VARIANCE
;		Returns the variance of the result, given by
;		variance = (1 - Result^2)^2/LCOUNT
;
;		Note:	The format of LVALUES, LCOUNT and VARIANCE is same as the format
;				of the standard output. 
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Follows the mathematical definition while using the
;		masks to eliminate invalid data points.  Calls ARREQ, CALCTYPE, DEFAULT,
;		FPU_FIX, MAKE_RANGE, SORPURGE and TOLER from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2001 by Mati Meron.
;		Modified 10-FEB-2002 by Mati Meron.  Added dependence on MAKE_RANGE.
;		Modified 30-MAR-2012 by Mati Meron.  Slight internal changes to assure
;		that one-element outputs (main and optional) are scalars.
;-

	on_error, 1
	zmfl = not keyword_set(nzm)
	rmfl = keyword_set(rum)

	wx = reform(x)
	sizx = size(wx)
	if sizx[0] eq 1 then len = sizx[1] else message, 'X must be a vector!'
	if n_elements(y) ne 0 then begin
		wy = reform(y)
		if not Arreq([1,len],(size(wy))[0:1]) then message, 'Size mismatch!'
	endif else wy = wx
	eps = Toler(type=Calctype(wx,wy))

	if n_elements(xm) gt 0 then begin
		wxm = reform(xm)
		if not Arreq([1,len],(size(wxm))[0:1]) then message, 'Xmask mismatch!'
	endif else wxm = replicate(1l,len)
	if n_elements(ym) gt 0 then begin
		wym = reform(ym)
		if not Arreq([1,len],(size(wym))[0:1]) then message, 'Ymask mismatch!'
	endif else wym = wxm

	if zmfl and not rmfl then begin
		dum = where(wxm,ndum)
		if ndum gt 0 then wx[dum] = wx[dum] - total(wx[dum])/ndum
		dum = where(wym,ndum)
		if ndum gt 0 then wy[dum] = wy[dum] - total(wy[dum])/ndum
	endif

	wlag = Make_range(lag,def=0)
	dum = where(abs(wlag) le (len-1),ndum,comp=cdum,ncomp=ncdum)
	if ndum gt 0 then wlag = wlag(dum) else message, 'Impossible range!'
	if ncdum gt 0 then message, 'Range excessive, curtailed!' ,/continue
	lcon = make_array(size=size(wlag))
	res = make_array(size=size(wlag),type=Calctype(wx,wy))

	for i = 0l, n_elements(wlag) - 1 do begin
		if wlag[i] ge 0 then begin
			l = wlag[i]
			wxym = where(wxm and wym[l:*],npo)
			lx = 0
			ly = l
		endif else begin
			l = -wlag[i]
			wxym = where(wxm[l:*] and wym,npo)
			lx = l
			ly = 0
		endelse

		if npo gt 0 then begin
			tx = wx[wxym+lx]
			ty = wy[wxym+ly]
			if zmfl and rmfl then begin
				tx = tx - total(tx)/npo
				ty = ty - total(ty)/npo
			endif
			res[i] = total(tx*ty)/(sqrt(total(tx^2)*total(ty^2)) + eps)
			lcon[i] = npo
		endif
	endfor

	var = FPU_fix((1 - res^2)^2/lcon)
	if n_elements(res) eq 1 then begin
		res = res[0]
		wlag = wlag[0]
		lcon = lcon[0]
		var = var[0]
	endif

	return, FPU_fix(res)
end