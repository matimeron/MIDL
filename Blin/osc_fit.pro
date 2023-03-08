Function Osc_fit, dat, tim, omega = omg, area = are, order = ord, $
	status = sta, chi = chi, _extra = _e
	
;+
; NAME:
;		OSC_FIT
; VERSION:
;		8.42
; PURPOSE:
;		Fits an oscilatory input to a function consisting of a sum of constant,
;		declining exponent and an arbitrary number of sinusoids.
; CATEGORY:
;		Liquid compression specific.
; CALLING SEQUENCE:
;		Result = OSC_FUN(P, Q [, keywords])
; INPUTS:
;	DAT
;		Vector, set of values for the oscilatory function.
;	TIM
;		Vector, set of time coordinates for the values in DAT.  Must be same
;		length as DAT.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	OMEGA
;		Scalar, an initial base angular frequency value for fitting.  If not
;		given, an initial value is evaluated internally from the data.
;	/AREA
;		Switch.  Specifies that DAT represents Area, not Pressure, i.e. there
;		is no declining exponent present.
;	ORDER
;		Integer, specifies the highest order sine to be used (i.e. the last sine
;		is of the form sin(ORDER*OMEGA*X + phase).
;	STATUS
;		Optional output, see below.
;	CHI
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to transfer additional values to imbedded 
;		functions.  Not to be used directly.
; OUTPUTS:
;		Return a vector containing the parameters optimizing the function.  See
;		OSC_FUN for detailed listing.
; OPTIONAL OUTPUT PARAMETERS:
;	STATUS
;		Returns the optimization status code, as follows:
;			-1	:	Optimization successful.
;			0	:	Optimization failed.
;			1	:	Non oscillatory function, nothing to optimize.
;	CHI
;		Returns the final Chi-squared value for the optimization.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, fits the input through calls to OSC_FUN.  In addition
;		To OSC_FUN, also calls OSC_EXT.  Calls CALCTYPE, CAST, DEFAULT, DIF,
;		FGH_EXT, ISNUM and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2014 by Mati Meron.
;-

	on_error, 1
	fun = 'osc_fun'
	eps = 1d-12

	typ = Calctype(dat,tim)
	wdat = Cast(dat,5)
	wtim = Cast(tim,5)
	arfl = keyword_set(are)
	word = Default(ord,1,/dtyp) > 1
	msk = lonarr(4 + 2*word)
	res = dblarr(4 + 2*word)
	prfl = ((Wherinstruct('pro',_e))[0] ge 0)

	Osc_ext, dat, min = imin, max = imax, num = nos, /quiet
	if nos gt 0 then begin
		if not arfl and Isnum(omg) then womg = omg else $
		womg=4*!dpi/(mean((Dif(wtim[imax]))[1,*])+ mean((Dif(wtim[imin]))[1,*]))
		vdat = (wdat[imax] + wdat[imin])/2
		lev = mean(vdat)
		amp = mean((wdat[imax] - wdat[imin])/2)

		if arfl then begin
			res[[0,3,4,5]] = [lev,womg,amp,!dpi]
			msk[5] = 1
			res = FGH_ext(fun,eps,$
			x_ini=res,par=wtim,yval=wdat,mask=msk,/scale,/sum,/min)
			msk[[0,3,4,5]] = 1
			res = FGH_ext(fun,eps,stat=sta,chi=chi,$
			x_ini=res,par=wtim,yval=wdat,mask=msk,/scale,/sum,/min,_extra=_e)
		endif else begin
			vtim = (wtim[imax] + wtim[imin])/2
			res[0:2] = [min(vdat),max(vdat)-min(vdat),1./(max(vtim)-min(vtim))]
			msk[0:2] = 1
			res = FGH_ext(fun,eps,$
			x_ini=res,par=vtim,yval=vdat,mask=msk,/scale,/sum,/min)
			res = FGH_ext(fun,eps,$
			x_ini=res,par=wtim,yval=wdat,mask=msk,/scale,/sum,/min)
			res[3] = womg
		endelse
		for i = 0, word-1 do begin
			j = 2*i + 4
			tmsk = 0*msk
			tmsk[[j,j+1]] = 1
			res[[j,j+1]] = [amp,!dpi]
			res = FGH_ext(fun,eps,$
			x_ini=res,par=wtim,yval=wdat,mask=tmsk,/scale,/sum,/min)
			msk[[j,j+1]] = 1
			res = FGH_ext(fun,eps,stat=sta,chi=chi,$
			x_ini=res,par=wtim,yval=wdat,mask=msk,/scale,/sum,/min,_extra=_e)
			if res[j] lt 0 then begin
				res[j] = -res[j]
				res[j+1] = res[j+1] + !dpi
			endif
			res[j+1] = $
			(res[j+1] - 2*!dpi*(floor(res[j+1]/(2*!dpi) < 0))) mod (2*!dpi)
			if prfl then print
		endfor
		chi = Cast(chi,typ,typ,/fix)
	endif else begin
		res[0] = mean(wdat)
		sta = 1
		chi = 0.
	endelse
	if res[2] lt 0 then res[1:2] = 0

	return, Cast(res,typ,typ,/fix)
end