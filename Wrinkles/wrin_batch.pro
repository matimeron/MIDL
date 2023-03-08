Pro Wrin_batch, file, slice = sli, pixel = pix, smooth = smo, normalize = nrm, $
	histogram= his, fft= fft, corr= cor, input= inp, output= oup, verbose= vrb,$
	_extra = _e

;+
; NAME:
;		WRIN_BATCH
; VERSION:
;		7.0
; PURPOSE:
;		Processes multiple wrinkle files, saving the outputs.
; CATEGORY:
;		Wrinkles calculations
; CALLING SEQUENCE:
;		WRIN_BATCH, FILE [,keywords]
; INPUTS:
;	FILE
;		Full or partial file name (file only, no path info) may include wild 
;		cards.  All files in the input directory (see below) fitting FILE will
;		be processed.  If not given, FILE defaults to '*'.
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
;	/VERBOSE
;		Switch.  If set, the names of the processed files are displayed to the 
;		screen.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Screen output and possible saved files.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		WRIN_BATCH queries interactively for input and output directory names
;		and processes all files from the input directory fitting the template
;		FILE, using WRIN_SHOW (see there for details).  Then it saves the 
;		generated output files into 6 subdirectories (*under* the output 
;		directory) named HIS_PNG, HIS_TXT, FFT_PNG, FFT_TXT, COR_PNG, COR_TXT.
;		if those directories don't exist, they're created on the fly.  In 
;		addition, text files containing the lists of files saved into the 
;		subdirectories are written into the output directory.
;
;		WRIN_BATCH calls WRIN_SHOW.  It also calls SDEP from IMPORTS and 
;		DEFAULT and FILE_GET from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JAN-2008 by Mati Meron.
;-

	on_error, 1

	ext = '.jpg'
	lext = strlen(ext)
	wfil = Default(file,'*',/dtyp)
	pos = strpos(wfil,'.',/reverse_search)
	if pos gt 0 then wfil = strmid(wfil,0,pos)
	rdir = File_get(inp,/dir,title='Select input folder')
	wdir = File_get(oup,/dir,title='Select output folder')
	lrdir = strlen(rdir)
	spat = rdir + sdep(/ds) + wfil + ext
	flist = file_search(spat,count=nfil)

	if keyword_set(vrb) then begin
		nam = strmid(flist,lrdir,transpose(strlen(flist)) - lrdir - lext)
		inf = string((indgen(nfil)+1),form='("	",i0,":	")') + nam
		vrbfl = 1
		print
	endif else vrbfl = 0

	for i = 0l, nfil-1 do begin
		Wrin_show, flist[i],sli= sli,pix= pix,smo= smo, norm= nrm,$
		hist= his,fft= fft,corr= cor,save= wdir, _extra = _e
		if vrbfl then print, inf[i]
	endfor

	cd, cur = cur
	cd, wdir
	wlist = ['his_png', 'his_txt', 'fft_png', 'fft_txt', 'cor_png', 'cor_txt']
	clist = ['his.png', 'his.txt', 'fft.png', 'fft.txt', 'cor.png', 'cor.txt']
	file_mkdir, wlist
	flist = file_search('*',/test_regular)
	for i = 0, n_elements(clist) - 1 do begin
		dum = where(strpos(flist,clist[i]) gt 0,ndum)
		if ndum gt 0 then begin
			file_move, flist[dum], wlist[i], /over
			openw, unit, wlist[i] + '.lst', /get_lun
			printf, unit, flist[dum], format = '(a)'
			free_lun, unit
		endif
	endfor
	cd, cur

	return
end