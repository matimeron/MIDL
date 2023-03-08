Pro PD_walk_old, snum, fnum, width = wid, filter = fil, $
	slit = sli, back_ang = bag, smooth = smo, refine = ref, $
	show_fit = shf, intensity = int, result = peak, _extra = _e

;+
; NAME:
;		PD_WALK
; VERSION:
;		8.31
; PURPOSE:
;		Displays the result of a "walking scan" for curved surfaces.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		PD_WALK, SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
; KEYWORD PARAMETERS:
;	WIDTH
;		Scalar value for width, in pixels, for the purpose of centroid 
;		calculation.  Defaults to 12 for Pilatus, 36 for APEX.
;	FILTER
;		Obsolete keyword, maintained for backward compatibility.
;	SLIT
;		Numerical scalar, horizontal size of electronic slit to be applied to 
;		the data.  Default is no slit.
;	BACK_ANG
;		Numerical scalar, the "sideways" offset angle used to evaluate
;		background, in degrees.  Default is no background.  If SLIT is not 
;		defined, BACK_ANG is ignored.
;	SMOOTH
;		Numeric scalar, if present, the data (1D, after integration) is smoothed
;		(using a BC kernel) before further processing.  If the value of SMOOTH 
;		is > 1, it is used as the smoothing width, else half of WIDTH (see above
;		is used.
;	/REFINE
;		Switch.  Specifies refinement of the calculation of sample center (point
;		of minimal curvature).
;	/SHOW_FIT
;		Switch, when set, cubic fit to data is displayed alongside the data.
;	/INTENSITY
;		Switch.  If set, an additional plot, of integrated intensity, is shown.
;	RESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Screen output only.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the locations (in pixels) of specular reflections as a function
;		of sample_height, in the standard 3 column format, [S_H, reflection, 
;		err]
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Locates the specular peak in all the frames and finds the vertical
;		centroid locations and the vertical and horizontal widths.  Displays
;		these results and fits a cubic polynomial to the centroid data in order
;		to find sample center (point of minimal curvature) and the radius of 
;		curvature at the center.
;		Calls IMG_INT, PEAK_CENT, PEAK_FWHM, PEAK_SMOOTH, SCAN_COLUMN, 
;		SCAN_FIND, SCAN_LC, SCAN_PD_CENTER, SCAN_PD_FPREAD, SCAN_SCALE and 
;		SCAN_SHOW.  Calls ERREST from SURF_LIB.  Calls ARRO, CAST, DEFAULT, 
;		ISNUM, LEGEND_MM, LINFIT_MM, POLEVAL, PSYMS and SPLIT_XY from MIDL.
; MODIFICATION HISTORY:
;		Created 30-JUN-2008 by Mati Meron.
;		Rewritten 15-MAR-2009 by Mati Meron.  Added fitting capability.
;		Modified 20-OCT-2009 by Mati Meron.  Added keyword INTENSITY.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 25-AUG-2010 by Mati Meron.  Replaced LEGEND with LEGEND_MM.
;		Modified 25-MAR-2011 by Mati Meron.  Updated error calculations for APEX
;		Modified 15-JUN-2011 by Mati Meron.  Added keywords SLIT and CENTER.
;		Modified 5-AUG-2011 by Mati Meron.  Internal changes.
;		Modified 10-AUG-2011 by Mati Meron.  More internal changes.  Added 
;		keyword SMOOTH.
;		Modified 30-MAY-2014 by Mati Meron.  Internal changes.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	if Scan_find(snum,typ='sh',/val,/vpd) eq snum then cur= fildat.scan[snum] $
	else message, 'Not an SH scan!'
	pfl = (cur.pdstat - 1)/2

	if keyword_set(sli) then begin
		scen = (Scan_PD_center(snum))[0]
		hs = round(sli[0])/2
		xyr = scen + [-hs,hs]
		if Isnum(bag) then begin
			hoff = round(bag*!dtor*cur.pdist/cur.pdpix)
			if hoff le 2*hs then begin
				bagm = cur.pdpix*(2*hs+1)/(!dtor*cur.pdist)
				message, 'Minimal offset for current slit is ' + $
				string(bagm,form='(f6.3)') + ' degrees!'
			endif
			bfl = 1
			coef = [1.,-0.5,-0.5]
		endif else bfl = 0
	endif else bfl = 0

	hwid = Default(wid,12*(cur.pdstat-2),/dtyp)/2
	if Isnum(smo) then begin
		if smo le 1 then swid = hwid else swid = round(smo)
		smofl = 1
	endif else smofl = 0

	dat = Scan_PD_fpread(snum,fnum,$
		flist=wfnum,nframes=nfnum,title=tit,/norm,nfac=nfc,_extra=_e)
	lim = (size(dat))[2:3] - 1
	peak = (hfwhm = (vfwhm = fltarr(3,nfnum)))
	psta = (hsta = (vsta = intarr(nfnum)))
	sh = (Scan_column(snum,0))[wfnum]
	peak[0,*] = (hfwhm[0,*] = (vfwhm[0,*] = sh))
	infl = keyword_set(int)
	if infl then inint = peak

	for i = 0l, nfnum-1 do begin
		ddat = reform(dat[i,*,*])
		hdat = Img_int(ddat,/z_int,_extra=_e)
		vdat = Img_int(ddat,/xy_int,xy_reg=xyr,_extra=_e)
		if bfl then begin
			ldat = Img_int(ddat,/xy_int,xy_reg=xyr-hoff,_extra=_e)
			rdat = Img_int(ddat,/xy_int,xy_reg=xyr+hoff,_extra=_e)
			vdat = Scan_lc(vdat,ldat,rdat,coef=coef)
		endif
		if smofl then begin
			hdat = Peak_smooth(hdat,wid=swid)
			vdat = Peak_smooth(vdat,wid=swid)
		endif
		tem = max(vdat[1,*],loc)
		if loc ge hwid and loc le (lim[1] -hwid) then begin
			psta[i] = 1
			peak[1,i] = Peak_cent(vdat,range=[loc-hwid,loc+hwid],err=err)
			peak[2,i] = err
		endif else peak[1,i] = loc
		htem = Peak_fwhm(hdat,err=err,/rel,stat=sta,/quiet,_extra=_e)
		if sta then begin
			hsta[i] = 1
			hfwhm[1,i] = htem
			hfwhm[2,i] = err
		endif
		vtem = Peak_fwhm(vdat,err=err,/rel,stat=sta,/quiet,_extra=_e)
		if sta then begin
			vsta[i] = 1
			vfwhm[1,i] = vtem
			vfwhm[2,i] = err
		endif
		if infl then begin
			tem = max(hdat[1,*],lloc)
			wdat = ddat[(lloc- 2*hfwhm[1,i]) > 0:(lloc+ 2*hfwhm[1,i]) < lim[0],$
						(loc - 2*vfwhm[1,i]) > 0:(loc + 2*vfwhm[1,i]) < lim[1]]
			inint[1,i] = total(wdat)
			if pfl then inint[2,i] = sqrt(nfc[i]*inint[1,i]) $
			else inint[2,i] = nfc[i]*Errest(wdat/nfc[i],/emod,/tot)
		endif
	endfor
	peak = Scan_scale(peak,cur.pdpix)
	hfwhm = Scan_scale(hfwhm,cur.pdpix)
	vfwhm = Scan_scale(vfwhm,cur.pdpix)

	good = where(psta and vsta and hsta,ngood)
	if ngood gt 0 then begin
		gfl = 1
		dum = Split_xy(Cast(peak[*,good],5),x=x,y=y,z=err)
		yspan = max(y,min=min)-min
		coef = Linfit_mm(x,y,1/err,ord=3,err=ferr)
		xcen = -coef[2]/(3*coef[3])
		mslop = coef[1] + xcen*coef[2]
		rx = x
 		if keyword_set(ref) then begin
			span = sqrt(abs(2*mslop/coef[3]))
			ran = where((x ge xcen-span) and (x le xcen + span), ndum)
			if ndum gt 3 then begin
				rx = x[ran]
				ry = y[ran]
				rerr = err[ran]
				coef = Linfit_mm(rx,ry,1/rerr,ord=3,err=ferr)
				xcen = -coef[2]/(3*coef[3])
				mslop = coef[1] + xcen*coef[2]
			endif else message, "Can't refine!", /con
		endif
		serr = ferr/coef
		dcen = abs(xcen)*sqrt(serr[2]^2 + serr[3]^2)
		dslop = sqrt(ferr[1]^2 + xcen^2*coef[2]^2*(4*serr[2]^2 + serr[3]^2))
		walp = !dtor*(cur.angs)[0]
		if pfl then smult = 1. else smult = cos(walp)
		irad = sin(walp)/cur.pdist*(smult*mslop/2 - cos(walp))
		dirad = abs(sin(walp)/cur.pdist*dslop/2)
		if irad eq 0 then begin
			rad = (machar()).xmax
			drad = 1/dirad
		endif else begin
			rad = 1/irad
			drad = rad^2*dirad
		endelse
		print
		print, xcen, dcen, $
		form = '("	Center at sh 	= ",g12.4," (",g10.3,") mm")'
		print, 1e-3*rad, 1e-3*drad, $
		form = '("	Center Radius	= ",g12.4," (",g10.3,") m")'
		j = value_locate(x,xcen)
		if j gt 0 and j lt n_elements(x)-1 then begin
			ycen = (y[j]*(x[j+1]-xcen) + y[j+1]*(xcen-x[j]))/(x[j+1]-x[j])
			to = [xcen,ycen]
			from = to - [0,0.1*yspan]
			afl = 1
		endif else afl = 0
	endif else gfl = 0

	keep = !d.window
	window, 0
	if gfl then begin
		Scan_show, peak, peak[*,good],tit=tit,xtit='sh (mm)',ytit='Peak (mm)',$
		line = [2,0], lcol=!pcol.red, /ynozero, _extra = _e
		if afl then Arro, from = from, to = to
		Psyms
		if keyword_set(shf) then oplot, rx, Poleval(rx,coef), col = !pcol.cyan
	endif else Scan_show, peak, tit = tit, xtit= 'sh (mm)', ytit= 'Peak (mm)',$
		line = 2, lcol=!pcol.red, _extra = _e 
	window, 1
	cols = [!pcol.green,!pcol.blue]
	if gfl then Scan_show, hfwhm, vfwhm, hfwhm[*,good], vfwhm[*,good],tit= tit,$
	xtit='sh (mm)',ytit='FWHM (mm)',line=[2,2,0,0],lcol=[cols,cols],_extra= _e $
	else Scan_show, hfwhm, vfwhm, title = tit, $
	xtit = 'sh (mm)', ytit = 'FWHM (mm)', lcol = [cols,cols], _extra= _e
	Legend_mm, text = ['Horizontal','Vertical'],line=[0,0],col=cols

	if infl then begin
		window, 2
		Scan_show, inint, tit = tit,xtit='sh (mm)',ytit='Integrated Intensity',$
		_extra = _e
	endif 
	wset, keep > 0

	return
end