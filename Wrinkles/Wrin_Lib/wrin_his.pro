Function Wrin_his, file, slice= sli, pixel= pix, smooth= smo, normalize= nrm, $
	show = sho, window = win, restore = rst, _extra = _e

;+
; NAME:
;		WRIN_HIS
; VERSION:
;		7.0
; PURPOSE:
;		Generates a "Period histogram" for an image file.
; CATEGORY:
;		Wrinkles calculations
; CALLING SEQUENCE:
;		Result = WRIN_HIS( FILE [,keywords])
; INPUTS:
;	FILE
;		A valid (full, unless the keyword /CURRENT is used) filename.  If not
;		provided, will be querried for interactively.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SLICE
;		Integer scalar, the number of slices to divide the image into.  Default
;		value is 1.
;	PIXEL
;		Pixel size (in whatever units).  Default value is 1.
;	SMOOTH
;		Integer scalar, the width to be provided to SMOOTH_MM for the purpose of
;		smoothing of the input data.  Default is no smoothing.  Note that:
;			1)	SMOOTH_MM always uses odd width.  If even number is provided it
;				is rounded upwards to the nearest odd value.
;			2)	The width required for a given degree of smoothing is roughly
;				twice as large as this with boxcar smoothing.  This is not a
;				problem since non-broadening Savitzky-Golay smoothing is used.
;	/NORMALIZE
;		Switch.  If set, the resulting histogram is normalized to yield an
;		integral of 1.
;	/SHO
;		Switch.  If set, the output is being plotted to the screen.
;	WINDOW
;		Plot window number.  Active only when /SHO is set.  Default is current
;		plot window number.
;	/RESTORE
;		Switch.  If set, and /SHO is set, the original window number (from
;		before the call) is restored following the plotting.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns a [2,*] array with the period values in the first column and the
;		corresponding histogram values in the second.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		Block JPEG_LOG.  Containes the name of the last accessed file.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Given an output of SLICE_JPEG (see there), WRIN_HIS finds the spacing 
;		between consecutive maxima (and same for minima), combines the results 
;		for all the slices and generates a histogram of the spacings.  Calls 
;		SLICE_JPEG.  Calls ADC, DEFAULT, DIF, EXTREMA, FNAMPARSE and ISNUM 
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JAN-2008 by Mati Meron.
;-

	common jpeg_log, lfile
	on_error, 1

	if Isnum(file) then begin
		if (size(file))[0] eq 2 then dat = file else message, 'Bad input!'
	endif else dat = Slice_jpeg(file,slice=sli,smooth=smo,_extra=_e)
	siz = size(dat)
	if siz[0] eq 1 then begin
		dat = reform(dat,siz[1],1)
		siz = size(dat)
	endif
	wpix = Default(pix,1.,/dtyp)

	for i = 0l, siz[2]-1 do begin
		ext = Extrema(reform(dat[*,i]),sig=esig)
		tmin = ext(where(esig lt 0))
		tmax = ext(where(esig gt 0))
		tdif = [(Dif(tmin))[1:*],(Dif(tmax))[1:*]]
		if i eq 0 then difs = tdif else difs = [difs,tdif]
	endfor
	hist = ADC(difs,bin=1,/left,val=per)
	per = per*wpix
	if keyword_set(nrm) then hist = hist/(wpix*total(hist))

	if keyword_set(sho) then begin
		owin = !d.window > 0
		wwin = Default(win,owin,/dtyp)
		window, wwin
		tit = Fnamparse(lfile) + ' :	Period histogram'
		plot, per, hist, psym = 10, tit = tit, xtit = 'period', _extra = _e
		wshow, wwin
		if keyword_set(rst) then wset, owin
	endif

	return, transpose([[per],[hist]])
end