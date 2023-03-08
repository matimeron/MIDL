Function PD_peak_pars, snum, fnum, filename= fnam, last= las, rotate= rot, $
	auto= aut, range= ran, mean= mea, funct= fun, show= sho, $
	hdata = hdat, vdata = vdat, _extra= _e

;+
; NAME:
;		PD_PEAK_PARS
; VERSION:
;		8.46
; PURPOSE:
;		Evaluates parameters of a 2D peak.
; CATEGORY:
;		2D data anlysis.
; CALLING SEQUENCE:
;		Result = PD_PEAK_PARS([SNUM, FNUM], [keywords])
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;	SNUM
;		Single scan number.
;	FNUM
;		Frame number within the scan.  Only single frame allowed.
;
;		Note:	Both SNUM and FNUM are optional but, if given, they must be
;				both given and the data will be read from the image file pointed
;				to by SNUM and FNUM (in the context of the current SPEC file).
;				If SNUM and FNUM are not given, see FILENAME below.
; KEYWORD PARAMETERS:
; 	FILENAME														|
; 		Name of the image data file to be read, if SNUM and FNUM	|
; 		are not provided.  If they're provided, FILENAME (if given)	| These two
; 		will be ignored.											| keywords
;																	| are
; 		If neither SNUM and FNUM, nor FILENAME are provided, the 	| mutually
; 		user will be asked to locate the file, interactively.		| exclusive
; 	/LAST															|
; 		Switch.  If set, the last read filename is reread.  Also	|
; 		the last used value or ROTATE is reused.
;	ROTATE
;		Integer scalar, specifies rotation as in the IDL ROTATE function.
;		Default is 0, i.e. no rotation.
; 	/AUTO
; 		Switch.  If set, the peak is located automatically (maximum amplitude
; 		selection).  The default is having the user point it.
; 		AUTO is useful when the data includes one prominent peak,
; 		counterindicated otherwise.
; 	RANGE
; 		Integer scalar or 2-element vector specifying the range (in pixels) for
; 		peak search (from an initial manual selection) and peak fitting.  The
; 		full range used in each dimension is +- RANGE relative to the starting
; 		pixel, for a total span of 2*RANGE + 1.  When given as a vector, the
; 		first value applies to the horizontal dimension and the second to the
; 		vertical one.  Default value is the greater of height and width of the
; 		image (in pixels) divided by 64.
; 	/MEAN
; 		Switch.  If set, the peak centroid (insted of the highest amplitude
; 		pixel) is used as the center for the fitting region.
; 	 FUNCT
; 	 	Character value, the name of the fitting function.  Currently Gaussian,
; 	 	Lorentzian and Voigtian are accepted (first three letters suffice).
; 	 	Default is Gaussian.
; 	 /SHOW
; 	 	Switch.  If set, the selected image region and the horzontally and
; 	 	vertically integrated data and fits are displayed to the screen.  Also,
; 	 	the fit parameters are printed to the screen.
; 	 HDATA
; 	 	Optional output, see below.
; 	 VDATA
; 	 	Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  Not to be used directly.
; OUTPUTS:
;		Returns a structure of type PEAK_PARS.  See PEAK_PARS__DEFINE for
;		details.
; OPTIONAL OUTPUT PARAMETERS:
; 	HDATA
; 		Returns the horizontal (vertically integrated) data in the [3,N] format.
; 	VDATA
; 		Returns the vertical (horizontally integrated) data in the [3,N] format.
; COMMON BLOCKS:
;		PEAK_PARS.  Includes:
;			LFIL	-	The full name of the last file read.
;			LROT	-	Last used ROTATE value.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Following the location of the peak (either manually or, when AUTO is
; 		set, automatically) PD_PEAK_PARS integrates the region around the peak
; 		(as specified by RANGE) both horizontally and vertically and fits both
; 		integrals tot he fit function in order to find the peak centroid and
; 		sigma values (in both directions).
; 		Calls IMG_INT, SCAN_PD_PEAK, SCAN_PD_READ, and SPEC_FILE_CHECK from SPEC
; 		Calls PEAK_FIT from BLIN.  Also calls DEFAULT, DISPLAY_MM, FNAMPARSE,
; 		ONE_OF and PLVAR_KEEP, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-APR-2016 by Mati Meron.
;-

	common peak_pars, lfil, lrot

	on_error, 1
	shofl = keyword_set(sho)
	bwin = 8

	posib = ['Gauss','Lorentz','Voigt']
	if n_elements(fun) ne 0 then begin
		ftyp = Strmatch_mm(fun,posib,3)
		if ftyp eq -1 then message, 'Unrecognized function!'
	endif else ftyp = 0
	fityp = posib[ftyp]

	dwin = !d.window
	res = {peak_pars}
	res.scan = (res.frame = -1)
	if keyword_set(las) and n_elements(rot) eq 0 then rot=Default(lrot,0,/dtyp)
	case n_params() of
		0	:	begin
					if (One_of(fnam,las) > 0) $
					then wfnam = Default(lfil,'',/dtyp) $
					else wfnam = Default(fnam,'',/dtyp)
					dat= Scan_PD_read( $
					file=wfnam, rot=rot, /raw, tit=tit, ext=ext, _extra=_e)
				end
		1	:	message, 'Either both Scan and Frame numbers, or none!'
		2	:	begin
					Spec_file_check, snum, /sing, /pildet
					if n_elements(fnum) eq 1 then begin
						dat = Scan_PD_read( $
						snum,fnum, rot=rot, /raw, tit=tit, ext=ext, _extra=_e)
						res.scan = snum
						res.frame = fnum
					endif else message, 'Only single frame allowed!'
				end
		else:	message, 'Bad input!'
	endcase
	dim = (size(dat,/dim))[1:2]
	lrot = Default(rot,0,/dtyp)
	res.file = (lfil = tit + ext)
	res.fit = Default(fun,'gau',/dtyp)

	wran = Default(ran,max(dim)/64,/dtyp) > 3
	if n_elements(wran) eq 1 then wran = [wran,wran]
	res.range = wran
	if not keyword_set(aut) then begin
		Display_mm, dat, ppos= pps, tit= tit, _extra = _e
		done = 0
		repeat begin
			print
			print, '		*** Click to select location ***'
			print
			cursor, h, v, /dev, /down
			cloc = [h,v] - pps[0:1]
			if cloc[0] ge 0 and cloc[0] lt dim[0] $
			and cloc[1] ge 0 and cloc[1] lt dim[1] then done = 1
		endrep until done
		lo = 0 > (cloc-wran) < dim
		hi = 0 > (cloc+wran) < dim
		reg = [lo[0],lo[1],hi[0],hi[1]]
		ploc = Scan_PD_peak(dat,region=reg)
	endif else ploc = Scan_PD_peak(dat)
	res.peak_loc = ploc
	res.peak_val = dat[2,ploc[0],ploc[1]]
	if keyword_set(mea) then begin
		res.mean_set = 1
		ploc = round(Scan_PD_peak(dat,region=reg,/mean,range=wran[0]))
		res.mean_loc = ploc
	endif else res.mean_loc = [-1,-1]

	lo = 0 > (ploc-wran) < dim
	hi = 0 > (ploc+wran) < dim
	wdat = dat[*,lo[0]:hi[0],lo[1]:hi[1]]
	wdat[2,*,*] = wdat[2,*,*] - min(wdat[2,*,*])

	if dwin lt 0 then dwin = !d.window
	if shofl then begin
		scal = 320l
		Display_mm, wdat, auz=scal[0], /shave, /nodata, wsiz=wsi, _extra = _e
		window, bwin, xsi= wsi[0], ysi= wsi[1] + 2*scal, tit= 'PD_PEAK_PARS'
		Display_mm, wdat, win=bwin, auz=scal[0], /shave, tit=Fnamparse(tit), $
		poff=[0,2*scal]
	endif

	Plvar_keep, act='sav'
	xof = 64
	yof = 32
	if shofl then !p.region=[xof,scal+yof,wsi[0]-1-xof,2*scal-1-yof]
	hdat = Img_int(wdat,/z_int)
	hdat[1,*] = hdat[1,*] - min(hdat[1,*])
	hres = Peak_fit(hdat,fun=fun,bac=2,cen=ploc[0],sho=shofl,den=3,err=herr,$
	psym=-8,xstyle=1,/dev,/noerase,tit='Horizontal '+fityp+' Fit',_extra=_e)
	if shofl then !p.region=[xof,yof,wsi[0]-1-xof,scal-1-yof]
	vdat = Img_int(wdat,/xy_int)
	vdat[1,*] = vdat[1,*] - min(vdat[1,*])
	vres = Peak_fit(vdat,fun=fun,bac=2,cen=ploc[1],sho=shofl,den=3,err=verr,$
	psym=-8,xstyle=1,/dev,/noerase,tit='Vertical '+fityp+' Fit',_extra=_e)
	Plvar_keep, act='res'
	if shofl and dwin ge 0 then wset, dwin

	res.center = [hres[4],vres[4]]
	res.centerr = [herr[4],verr[4]]
	res.sigma = [hres[5],vres[5]]
	res.sigerr = [herr[5],verr[5]]

	return, res
end