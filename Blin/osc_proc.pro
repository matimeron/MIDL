Pro Osc_proc, file, last = las, drop = drp, order = ord, show = sho, $
	a_param = apar, p1_param = p1par, p2_param = p2par, _extra = _e

;+
; NAME:
;		OSC_PROC
; VERSION:
;		8.42
; PURPOSE:
;		Reads (if needed) oscillation data from Liquid Compression file and
;		performs a series of fits.
; CATEGORY:
;		I/O, Liquid compression specific.
; CALLING SEQUENCE:
;		OSC_PROC [,FILE] [, keywords]
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;	FILE
;		The name of the file to be read.  If not given, will be querried for
;		interactively.
; KEYWORD PARAMETERS:
; 	/LAST
; 		Switch.  If set, last read data is reprocessed.
; 	DROP
; 		Integer scalar, the number of periods to drop from the beginning of the
; 		data.
; 	ORDER
; 		Integer scalar, specifies the highest order of sine to use.  See OSC_FUN
; 		for details.
; 	/SHOW
; 		Switch.  If set, plots of data and fits are displayed.  Also, the 
; 		Chi-Squared values for the fits are displayed to the screen.
; 	A_PARAM
; 		Optional output, see below
; 	P1_PARAM
; 		Optional output, see below.
; 	P2_PARAM
; 		Optional output, see below.
;	_EXTRA
;		A formal keyword used to transfer additional values to imbedded
;		functions.  Not to be used directly.
; OUTPUTS:
; 		Returned through the optional output parameters.
; OPTIONAL OUTPUT PARAMETERS:
; 	A_PARAM
; 		A vector containing the fiting parameters for the area.  See the 
; 		function OSC_FUN for a description of the parameter vector.
; 	P1_PARAM
; 		Same as A_PARAM, for the longitudinal pressure P1.
; 	P2_PARAM
; 		Same as A_PARAM, for the longitudinal pressure P1.
; COMMON BLOCKS:
;		OSC_DAT.  See OSC_READ for details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Reads the file (unless LAST is set) drops the beginning of the data, if
; 		needed, then performs fits on the area and both pressures.  Calls
; 		OSC_FIT, OSC_FUN and OSC_READ.  Calls DEFAULT, FNAMPARSE, PLVAR_KEEP, 
; 		and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2014 by Mati Meron.
;-

	common osc_dat, fnam, t, a, p1, p2
	on_error, 1
	bsiz = 32l

	if not (keyword_set(las) and Type(fnam) eq 7) then Osc_read,file,_extra= _e

	par = Osc_fit(a,t,/area)
	per = 2*!pi/par[3]
	ndrp = Default(drp,0,/dtyp)
	tfir = t[0] + ndrp*per
	nper = floor((t[-1] - tfir)/per)
	tlas = tfir + nper*per
	dum = where(t ge tfir and t le tlas)

	wt = t[dum]
	wa = a[dum]
	wp1 = p1[dum]
	wp2 = p2[dum]

	apar = Osc_fit(wa,wt,/area,order=ord,chi=achi,_extra=_e)
	omg = apar[3]
	p1par = Osc_fit(wp1,wt,omega=omg,order=ord,chi=p1chi,_extra=_e)
	p2par = Osc_fit(wp2,wt,omega=omg,order=ord,chi=p2chi,_extra=_e)

	if keyword_set(sho) then begin
		print
		print, '	Chisq values'
		print
		print, '	A	- ', achi
		print, '	P1	- ', p1chi
		print, '	P2	- ', p2chi
		plvar_keep, act = 'sav'
		window, 8, xsiz= 15*bsiz, ysiz= 30*bsiz, tit= 'IDL 8:  '+Fnamparse(fnam)
		!p.multi = [0,1,3]
		!p.charsize = 1.8
		!x.margin = [8,3]
		!y.margin = [4,3]
		plot, wt, wa, /ynoz, xtit = 'time (sec)', ytit = 'A (cm!e2!n)', $
		tit = 'Area', /nodata,  _extra = _e
		oplot, wt, wa, col = !pcol.red, thi = 2
		oplot, wt, Osc_fun(apar,wt), col = !pcol.green, thi = 2
		plot, wt, wp1, /ynoz, xtit = 'time (sec)', ytit = 'P (mN/m)', $
		tit = 'P1', /nodata, _extra = _e
		oplot, wt, wp1, col = !pcol.red, thi = 2
		oplot, wt, Osc_fun(p1par,wt), col = !pcol.green, thi = 2
		plot, wt, wp2, /ynoz, xtit = 'time (sec)', ytit = 'P (mN/m)', $
		tit = 'P2', /nodata, _extra = _e
		oplot, wt, wp2, col = !pcol.red, thi = 2
		oplot, wt, Osc_fun(p2par,wt), col = !pcol.green, thi = 2
		plvar_keep, act = 'res'
	endif

	return
end