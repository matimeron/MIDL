Function Slice_jpeg, file, slices= sli, smooth = smo, zero_mean = zrm, $
	show = sho, offset = off, last = las, _extra = _e

;+
; NAME:
;		SLICE_JPEG
; VERSION:
;		7.0
; PURPOSE:
;		Reading and processing JPEG files.
; CATEGORY:
;		Input/output.
; CALLING SEQUENCE:
;		Result = SLICE_JPEG( FILE [,keywords])
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
;	SMOOTH
;		Integer scalar, the width to be provided to SMOOTH_MM for the purpose of
;		smoothing.  Default is no smoothing.  Note that:
;			1)	SMOOTH_MM always uses odd width.  If even number is provided it
;				is rounded upwards to the nearest odd value.
;			2)	The width required for a given degree of smoothing is roughly
;				twice as large as thie with boxcar smoothing.  This is not a
;				problem since non-broadening Savitzky-Golay smoothing is used.
;	/ZERO_MEAN
;		Switch.  If set, the output data is shifted up/down so as to have zero
;		mean.
;	/SHO
;		Switch.  If set, the output is being plotted to the screen, with one 
;		curve for each slice.
;	OFFSET
;		The value to use as vertical offset between the plotted curves, when 
;		/SHO is set.  If not provided, an internal value is generated.  If /SHO
;		is not set, OFFSET does nothing.
;	/LAST
;		Switch.  If set, the last accessed file is reused.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Given a JPEG image of dimensions [M,N], SLICE_JPEG returns an array of
;		dimensions [M,SLI] whereSLI is the number of slices.  Each row of the 
;		array contains the average of the appropriate N/SLI rows of the original
;		image.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		Block JPEG_LOG.  Containes the name of the last accessed file.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, uses the IDL routine REBIN for the averaging.  Calls
;		CAST, DEFAULT, FILE_GET and SMOOTH_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JAN-2008 by Mati Meron.
;-

	common jpeg_log, lfile
	on_error, 1

	if not (keyword_set(las) and n_elements(lfile) ne 0) then begin
		wfile = File_get(file,filt='jpg',stat=stat,_extra=_e)
		if stat then lfile = wfile
	endif else wfile = lfile
	read_jpeg, wfile ,tem,/gray

	siz = size(tem)
	wsli = Default(sli,1l,lo=3,hi=3) + 1
	repeat begin
		wsli = wsli - 1
		check = siz[2] mod wsli
	endrep until check eq 0

	res = rebin(Cast(tem,4),siz[1],wsli)
	if keyword_set(zrm) then begin
		mean = total(res,1)/siz[1]
		for i = 0l, wsli-1 do res[*,i] = res[*,i] - mean[i]
	endif
	if keyword_set(smo) then $
	for i = 0l, wsli-1 do res[*,i] = Smooth_mm(res[*,i],smo)

	if keyword_set(sho) then begin
		cols = [!pcol.red,!pcol.green,!pcol.blue,!pcol.purple]
		max = max(res,min=min)
		woff = Default(off,max-min)
		yran = [min,max + woff*(wsli-1)]
		plot, res[*,0], /nodata, yran = yran, _extra = _e
		for i = 0l, wsli-1 do oplot, res[*,i] + i*woff, color = cols[i mod 4]
	endif

	return, reform(res,siz[1],wsli)
end

