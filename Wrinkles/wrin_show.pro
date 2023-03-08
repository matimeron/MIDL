Pro Wrin_show, file, slice = sli, pixel = pix, smooth = smo, normalize = nrm, $
	histogram = his, fft = fft, corr = cor, save = sav, _extra = _e

;+
; NAME:
;		WRIN_SHOW
; VERSION:
;		7.0
; PURPOSE:
;		Displays (and optionally saves) info from a wrinkles image file.
; CATEGORY:
;		Wrinkles calculations
; CALLING SEQUENCE:
;		WRIN_SHOW, FILE [,keywords]
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
;		Switch.  If set, the resulting histogram and/or FFT data is normalized 
;		to yield an integral of 1.  Has no effect on correlation data.
;	/HISTOGRAM										|
;		Switch.  If set, histogram data is shown.	|	If non of these three
;	/FFT											|	is set, HISTOGRAM is
;		Switch.  If set, FFT data is shown.			|	used as default.
;	/CORR											|
;		Switch.  If set, correlation data is shown.	|
;	SAVE
;		Accepts a file name (or just directory name) for data saving purposes.
;		The rules are:
;			1)  If full file name is provided, it'll be used as is.
;			2)  If only directory name is provided, it will be used in 
;				combination with the name of the input data file.
;			3)	If the SAVE input is doesn't translate to a valid path, 
;				WRIN_SHOW will query for a directory interactively and use it
;				with the name of the input data file.
;
;		If there is no input, data will not be saved, only displayed.
;
;		The graphic data is saved as PNG files and the numeric data as TXT 
;		files.  Results from histogram, FFT and correlation have "_HIS", "_FFT"
;		and "_COR" appended to the end of the file name, respectively.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Screen output and possible saved files.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		Block JPEG_LOG.  Containes the name of the last accessed file.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		WRIN_SHOW serves as a front end to WRIN_HIS, WRIN_FFT and WRIN_COR.  
;		Calls SLICE_JPEG.  Calls FILE_GET, FNAMPARSE, HOW_MANY, TYPE, WASCII
;		and WIMG_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JAN-2008 by Mati Meron.
;-

	common jpeg_log, lfile
	on_error, 1

	dat = Slice_jpeg(file,sli=sli,smo=smo,_extra=_e)
	wsli = (size(dat))[2]

	if Type(sav) gt 0 then begin
		 if Type(sav) eq 7 then snam = Fnamparse(sav,pat=spat,pres=pres) $
		 else snam = (spat = '')
		 if snam eq '' then snam = Fnamparse(lfile)
		 if spat eq '' then spat = $
		 File_get(/dir,/write,title='Select output folder')
		 snam = spat + snam
		 savfl = 1
	endif else savfl = 0

	dum = How_many(fir=his,sec=fft,thi=cor,whi=whi)
	whi = whi > 1
	win = 0
	owin = !d.window
	if whi and 1 then begin
		dum = Wrin_his(dat,pix=pix,norm=nrm,/sho,win=win,_extra=_e)
		if savfl then begin
			Wimg_mm, snam + '_his.png', /auto, call = 2
			Wascii, dum, snam + '_his.txt', /auto, call = 2, $
			tit = 'Period histogram', head = ['Period','Amplitude']
		endif
		win = win + 1
	endif
	if whi/2 and 1 then begin
		dum = Wrin_fft(dat,pix=pix,norm=nrm,/sho,win=win,_extra=_e)
		if savfl then begin
			Wimg_mm, snam + '_fft.png', /auto, call = 2
			Wascii, dum, snam + '_fft.txt', /auto, call = 2, $
			tit= 'Period distribution (from FFT)', head= ['Period','Amplitude']
		endif
		win = win + 1
	endif
	if whi/4 and 1 then begin
		csli = wsli < 6
		if wsli gt 6 then dat= Slice_jpeg(sli=csli,smo=smo,/last,_extra=_e)
		dum = Wrin_cor(dat,pix=pix,/sho,win=win,_extra=_e)
		if savfl then begin
			head = ['X',['Slice # ' + string((1+ indgen(csli)), form= '(i0)')]]
			Wimg_mm, snam + '_cor.png', /auto, call = 2
			Wascii, dum, snam + '_cor.txt', /auto, call = 2, $
			tit = 'Autocorrelation', head = head[0:csli]
		endif
	endif
	wset, owin

	return
end