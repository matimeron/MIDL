Function Wrin_cor, file, slice = sli, pixel = pix, smooth = smo, $
	lag = lag, show = sho, offset = off, window= win, restore= rst, _extra = _e

;+
; NAME:
;		WRIN_COR
; VERSION:
;		7.0
; PURPOSE:
;		Evaluates autocorrelations for slices of a JPEG file.
; CATEGORY:
;		Wrinkles calculations
; CALLING SEQUENCE:
;		Result = WRIN_COR( FILE [,keywords])
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
;	LAG
;		2 element vector specifying the range of lag values to be used in the 
;		calculation.  Default is all the available values to the extent of row
;		length.
;	/SHO
;		Switch.  If set, the output is being plotted to the screen.
;	OFFSET
;		The value to use as vertical offset between the plotted curves, when 
;		/SHO is set.  If not provided, an internal value is generated.  If /SHO
;		is not set, OFFSET does nothing.
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
;		Returns a [1+SLICE,*] array, with the lag values in the first column and 
;		the corresponding autocorrelations for each slice in the remaining 
;		columns.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		Block JPEG_LOG.  Containes the name of the last accessed file.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Given an output of SLICE_JPEG (see there), WRIN_COR calculates the
;		autocorrelations for each slice.  Calls SLICE_JPEG.  Calls CM_CORR from
;		LIQUID.  Calls DEFAULT, FNAMPARSE and ISNUM from MIDL. 
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
	wlag = Default(lag,[0l,siz[1]-1],/dtyp)
	if n_elements(wlag) ne 2 then message, 'Lag needs two values!'
	nl = wlag[1] - wlag[0] + 1
	ns = siz[2]

	corr = findgen(nl,ns)
	for i = 0l, ns-1 do corr[*,i] = CM_corr(dat[*,i],lag=wlag,lval=xl)
	xl = Default(pix,1.,/dtyp)*xl

	if keyword_set(sho) then begin
		owin = !d.window > 0
		wwin = Default(win,owin,/dtyp)
		window, wwin
		tit = Fnamparse(lfile) + ' :	Auto correlation'
		cols = [!pcol.red,!pcol.green,!pcol.blue,!pcol.purple]
		max = max(corr,min=min)
		woff = Default(off,max-min)
		yran = [min,max + woff*(ns-1)]
		plot, xl, corr[*,0], /nodata, yran= yran, tit= tit, xtit= 'x'
		for i = 0l, ns-1 do oplot, xl, corr[*,i] + i*woff, color = cols[i mod 4]
		wshow, wwin
		if keyword_set(rst) then wset, owin
	endif

	res = findgen(ns+1,nl)
	res[0,*] = xl
	res[1:*,*] = transpose(corr)

	return, res
end