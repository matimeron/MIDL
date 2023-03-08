Pro Edge_fit, snum, columns= col, func= fun, back= bac, range= ran, _extra= _e

;+
; NAME:
;		EDGE_FIT
; VERSION:
;		8.72
; PURPOSE:
;		Fits an absorption edge.
; CATEGORY:
;		Data Analysis.
; CALLING SEQUENCE:
;		EDGE_FIT, SNUM [, keywords]
; INPUTS:
;	SNUM
;		Scan number, in a currently defined SPEC file.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COLUMNS
;		3-element character vector, specifies the scan columns to be used.
;		Default is ['en','monp','monb'].
;	FUNC
;		Character scalar, specifies the fitting function.  Default is 'gauss',
;		alternatively 'lorentz' can be used.
;	BACK
;		Ingteger scalar, value in the range of 0-3, specifies the order of the
;		background to be subtracted.  See the routine PEAK_FIT for details.
;		Default value is 3 (meaning quadratic background).
;	RANGE
;		2-element vector, spcifies the X-range of data to be used.  Default is
;		all available data.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
; 		Screen output only, fit plots and printout of fit results.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Reads the scan data and fits both the data itself (to integral of
;		Gaussian or Lorentzian) and a numeric derivative of the data (to
;		Gaussian or Lorentzian), using PEAK_FIT from BLIN. Calls SCAN_DER,
;		SCAN_READ, SCAN_SCALE and SCAN_VER, for SPEC_LIB.  Also calls DEFAULT,
;		PLVAR_KEEP and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-OCT-2012 by Mati Meron.
;		Documented 25-DEC-2020 by Mati Meron.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	if Type(fun) eq 7 then wfun = [fun,fun] else wfun = ['gau','gau']
	wbac = Default(bac,3,/dtyp)
	raw = 0
	if Scan_ver(snum) then begin
		dat = snum
		raw = 1
	endif else $
	dat = (Scan_read(snum,col=Default(col,['en','monp','monb']),/noerr))[0:1,*]

	if n_elements(ran) eq 2 then begin
		dum = where(dat[0,*] ge min(ran,max=maxr) and dat[0,*] le maxr, ndum)
		if ndum gt 0 then dat = dat[*,dum] else message, 'Bad Range!'
	endif
	if raw then tit = '' else $
	tit = fildat.name + ' ; Scan # ' + string(snum,form='(i0)')

	Plvar_keep, act = 'sav'
	window, 4, xsi = 32*16, ysi = 32*24
	!p.multi = [0,1,2]
	!y.margin = [4,4]
	print
	print, '				EDGE FIT'
	print
	fir = Peak_fit(dat,fun=wfun[0],/edge,bac=wbac,/fixed,$
	/show_fit,den=2,tit='Edge fit!c' + tit,xtit='E (keV)',_extra=_e)
	print
	print, '			EDGE DERIVATIVE FIT'
	print
	sec = Peak_fit(Scan_scale(Scan_der(dat,/part),-1,/part),fun=wfun[1],/fixed,$
	bac=wbac,/show_fit,den=3,tit='Edge derivative fit!c' + tit,xtit='E (keV)',$
	_extra=_e)
	!p.multi = 0
	Plvar_keep, act = 'res'

	return
end