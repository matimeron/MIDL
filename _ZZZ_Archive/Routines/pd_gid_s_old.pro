Pro PD_GID_S_old, snum, frame = fnum, verify = ver, angles = ang, $
	slit = sli, ystep = yst, ylim = yli, nspec = nsp, auto_lim = aul, $
	tiny = tin, wnew = wnw, wait = wai, result = res, oresult = ores, $
	title = tit, scale_fac = scl, _extra = _e

;+
; NAME:
;		PD_GID_S
; VERSION:
;		8.31
; PURPOSE:
;		Generates "composite" GID scan data, by combining multiple slices from 
;		each area detector frame.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_GID_S, SNUM [,keywords])
; INPUTS:
;	SNUM
;		A list of scan numbers, in any form recognizable by RANGE_PROC.  If more
;		than one scan is provided, the data will be patched together in the
;		horizontal direction.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FRAME
;		Accepts a frame number or list of frame numbers, in any form
;		recognizable by RANGE_PROC.  Valid only when a single scan is used,
;		automatically disabled for multiple scans.
;		Note that the default operation is always to use all frames.
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	/ANGLES
;		Switch.  If set the independent coordinate of the result is the angle
;		DTHETA.  Default is Q_xy.
;	SLIT
;		Numeric scalar, the value of the "electronic slit" (around the center
;		pixel) in *pixels*.  See PD_GID_READ for details.
;	YSTEP
;		Scalar, step in the Y direction (along the beam footprint).  PD_GID_COMB
;		takes a series of data "slices" with different values of Y_OFF (see
;		PD_GID_READ), separated by YSTEP, within the range [-YLIM, YLIM] (see
;		below. 
;		Note:	If not given, a value of YSTEP corresponding to the angular
;				spacing of the data is selected, to minimize interpolation
;				errors.
;	YLIM																| one
;		Specifies the limits of the YOFF range.  If given as a scalar,	| and
;		the limits are [-ABS(YLIM),ABS(ylim)].  If given as a vector, 	| only
;		the limits are [-ABS(YLIM[0]),ABS(YLIM[1])].					| one of
;	NSPEC																| these
;		Number of spectra to be generated, on each side of the center	| two
;		(YOFF = 0) slice.  The yoffset of the spectra will be separated	| may be
;		by YSTEP.  This is an alternative way to set YOFF limits.		| used.
;
;		Note:	In any case, the limits may be further restricted by the
;				observable range and the setting of AUTO_LIM (see below).
;	AUTO_LIM
;		Specifies the fraction of the data within the observable range of each
;		frame that can be used.  See PD_PIN for more details.
;	/TINY
;		Switch.  If set, the display window is reduced in size, making it
;		appropriate for small laptops.
;	/WNEW
;		Switch.  If set, a new graphics window is created instead of the old
;		one being overwritten.  The windows at the disposal of PD_GID_S are 
;		20-23, when the last one is reached the sequence reverts to the origin.
;	WAIT
;		Optional time, in seconds, for the routine to stop and wait after 
;		displaying each intermediate result.  If PROGRESS is not set, WAIT has
;		no effect.
;	RESULT
;		Optional output (and input), see below.
;	ORESULT
;		Optional output (and input), see below.
;	TITLE
;		Optional output (and input), see below.
;	SCALE_FAC
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  This includes (among others) the following keywords,
;		affecting data readout:
;
;			/BAD
;				Switch.  If set, faulty (very high count rate) pixels are
;				removed from the data.
;			/NORM
;				Switch.  If set, the data is normalized to monitor counts.
;
;		See SCAN_PD_READ for more details.
;
;		Note:	/NORM is set by default in PD_GID_READ, it can be disabled by
;				using NORM = 0 in the function call.
; OUTPUTS:
; 		Direct output to screen only, other outputs through the optional output
; 		parameters, see below.
; OPTIONAL OUTPUT PARAMETERS:
; 	RESULT
;		Returns a 1D data set (i.e. a [3,N] array containing the horizontal
;		GID spectrum, genarating by patching together the specified slices.
;		The data columns are:
;			Result[0,*] = Q_xy coordinates of the image.
;			Result[1,*] = Diffracted intensity.
;			Result[2,*] = The statistical errors of the diffracted intensity.
;
;		If /ANGLES is set, the first column is replaced by the DTHETA angles.
;	ORESULT
;		Returns the horizontal GID spectrum generated from the center slice 
;		alone, in same format as RESULT.
;	TITLE
;		Returns a character scalar, made of the file name and the scan numbers,
;		to be used as title for plots.
;	SCALE_FAC
;		Returns a [2,M] array where M is the numbers of Y_OFF values used.  The
;		first column of the array contains the Y_OFF values, the second contains
;		the patching factors required to bring the offset data in line with the
;		no-offset one.  Can be used as a crude intensity correction factor for
;		PD_PIN images.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
;		PD_GID_S_KEEP.  Contains the index of the last graphics window used.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None other than the restrictions of PD_GID_READ.
; PROCEDURE:
; 		Repeatedly reads the data, through calls to PD_GID_READ with different
; 		Y_OFF values, and patches the results together to increase range and
; 		improve statistics.
;		Reads the data, evaluates Q_z and Q_xy using calibration values, then
;		translates the Q values to rectalinear coordinates and interpolates
;		the data using triangulation.
;		Calls PD_GID_READ and PD_PIN_RANGE.  Calls IMG_COO_CONV, SCAN_DER,
;		SCAN_GET_ANGS, SCAN_JOIN, SCAN_ORDER, SCAN_PD_CENTER, SCAN_PD_FRAMES,
;		SCAN_PD_FTOD, SCAN_PRUNE, SCAN_SCALE, SCAN_SHOW and SPEC_FILE_CHECK,
;		from SPEC.  Also calls ARREQ, DEFAULT, DIF, FLTROUND, ISNUM, JOIN_XY,
;		LEGEND_MM, ONE_OF and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-NOV-2013 by Mati Meron.
;		Modified 20-FEB-2014 by Mati Meron.  Internal streamlining.  Added
;		keywords PROGRESS and WAIT.
;		Modified 15-MAR-2014 by Mati Meron.  Converted from function to 
;		procedure.  Added keywords RESULT and ORESULT.
;		Modified 25-MAR_2014 by Mati Meron.  Internal changes.
;		Modified 20-MAY-2014 by Mati Meron.  Added keywords TINY and WNEW.  
;		Changed display pattern.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	common pd_gid_s_keep, lwin
	on_error, 1

	sp =  strcompress('s_' + sindgen(16),/remove)
	tsp = strarr(16)
	gform = '(f6.2," mm")'

	Spec_file_check, snum,/pil,nsc=nsc,lis=lis,_extra=_e
	if nsc gt 1 then wfnum = (cfl = -1) $
	else wfnum = Scan_PD_frames(lis[0],fnum,verify=ver,/uni,comp=cfl)

	res = (ores = (sp_0 = $
	PD_GID_read(snum,frame=fnum,slit=sli,ang=ang,/z_int,tit=tit,_extra=_e)))
	tsp[0] = string(0,form=gform)

	sord = Scan_order(lis,col=[0,-1,-2])
	slis = lis[sord]
	ystmin = Scan_PD_ftod(slis[0],0,xdet=sli)

	whi = One_of(yli,nsp,/nozer)
	if whi ge 0 then begin
		if Isnum(yst) then begin
			wyst = abs(float(yst))
			if wyst gt 0 and wyst lt ystmin then message, $
			'Y-step too small, minimal value to avoid data mixing is ' $
			+ string(ystmin,form = gform), /con
		endif else begin
			cur = fildat.scan[slis[0]]
			phi = reform(res[0,*])
			ddres = Scan_der(Scan_der(res))
			wei = reform(ddres[1,*])^2
			if total(wei) eq 0 then wei = total(res[1,*])^2
			if keyword_set(ang) then phi = !dtor*phi $
			else phi = 2*asin(cur.lambda*phi/(4*!pi))
			aphi = total(wei*phi)/total(wei)
			psi = max(Dif(phi,/lin))
			l1 = cur.pdist - cur.pdsdist
			wyst = l1*psi/sin(aphi)
			wyst = wyst*ceil(ystmin/wyst)
		endelse
	endif else wyst = 0.

	bsi = 32l
	xsi = [18,15]*bsi
	ysi = [27,18]*bsi
	tifl = keyword_set(tin)
	xsi = xsi[tifl]
	ysi = ysi[tifl]
	topos = [2*bsi,ysi/2+bsi,xsi-bsi,ysi-1.5*bsi]
	bopos = [2*bsi,1.5*bsi,xsi-bsi,ysi/2-bsi]

	cwin = !d.window
	bwnum = 16l
	if keyword_set(wnw) then begin
		lwin = Default(lwin,bwnum-1)
		wnum = bwnum + ((lwin - bwnum + 1) mod 4)
		lwin = wnum
	endif else wnum = Default(lwin,bwnum)
	window, wnum, xsize = xsi, ysize = ysi, $
	tit = strcompress('IDL ' + string(wnum) + ' : PD_GID_S Results')

	wcol = [!pcol.blue,!pcol.red]
	wchars = 1. - 0.2*tifl
	if keyword_set(ang) then xtit = 'Dth' else xtit = 'Q!dxy!n'
	wai = Default(wai,1e-3,/dtyp)

	Scan_show, res, thi= 2, lcol= wcol, pos= topos, /dev, /nofile, $
	tit = 'Initial, Yoff = ' + tsp[0], xtit= xtit, charsize= wchars,_extra = _e
	wait, wai

	if wyst ne 0 then begin
		if (Wherinstruct('mer',_e))[0] ge 0 then begin
			merfl = 1
			tcur = fildat.scan[slis[0]]
			litl = tcur.pdpix
			if not keyword_set(ang) then begin
				angs = (Scan_get_angs(slis[0]))
				coord = Img_coo_conv([0,litl],[0,0], qvals=1-keyword_set(ang),$
				dist=tcur.pdist,pdist=tcur.pdsdist,lam=tcur.lambda,/xrev,/sign,$
				alp=angs[0],bet=angs[1],dth=angs[2])
				tem = reform(coord[0,*,0])
				mdel = abs(tem[1]-tem[0])
			endif else mdel = !radeg*atan(litl/tcur.pdsdist)
			mdel = Fltround(mdel,dig=1)
		endif else merfl = 0
	
		asord = Scan_order(slis,col=[0,-1,-2],ord='max')
		chsnum = slis[asord[-1]]
		chfnum = fildat.scan[chsnum].ncr[1]-1
		waul = Default(aul,1.,/dtyp)
		gran = PD_pin_range(chsnum,chfnum,/raw) - (Scan_PD_center(chsnum))[0]
		wran = round(waul*gran)
		yran = [Scan_PD_ftod(chsnum,chfnum,xdet=wran[0]),$
				Scan_PD_ftod(chsnum,chfnum,xdet=wran[1])] + ystmin*[1,-1]

		if whi then begin
			if n_elements(nsp) eq 1 then wnsp = [nsp,nsp] else wnsp = nsp[0:1]
			wnsp = long(abs(wnsp))
		endif else begin
			if n_elements(yli) eq 1 then wyli = [yli,yli] else wyli = yli[0:1]
			wnsp = long(abs(yli)/wyst)
		endelse
		ensp = long(abs(yran)/wyst)
		ownsp = wnsp
		wnsp = wnsp < ensp
		dnsp = ownsp - wnsp
		if dnsp[0] gt 0 then begin
			print
			print, dnsp[0],form='("	Dropped ",i0," spectra on the (-) side.")'
		endif
		if dnsp[1] gt 0 then begin
			print
			print, dnsp[1],form='("	Dropped ",i0," spectra on the (+) side.")'
		endif
		if not Arreq(dnsp,[0,0]) then print
		if wnsp[0] eq 0 then yneg = [] else yneg = -wyst*(1 + findgen(wnsp[0]))
		if wnsp[1] eq 0 then ypos = [] else ypos = wyst*(1 + findgen(wnsp[1]))
	
		mfac = [1.]
		j = 0
		sform = '("sp_",i0," = next")'
		pform = '("tsp[",i0,"] = string(ypos[",i0,"],form=gform)")'
		nform = '("tsp[",i0,"] = string(yneg[",i0,"],form=gform)")'

		for i = 0, max(wnsp) - 1 do begin
			if i lt wnsp[1] then begin
				j = j + 1
				next = PD_GID_read($
				snum,fra=fnum,sli=sli,yof=ypos[i],ang=ang,/z_int,_extra=_e)
				dum = execute(string(j,form=sform))
				dum = execute(string(j,i,form=pform))
				res = Scan_join(res,next,fact=fac,_extra=_e)
				mfac = [mfac,fac[1]]

				Scan_show, sp_0, next, thi=2,lcol=wcol,pos=topos,/dev,/nofile, $
				tit= string(ypos[i],form='("Yoff = ",f6.2," mm")'),xtit= xtit, $
				charsize= wchars, _extra=_e
				Legend_mm, loc = 'UL', col = wcol, line = 0, /nowrap, $
				text = ['initial.','current'], charsize= wchars
				wait, wai
			endif
			if i lt wnsp[0] then begin
				j = j + 1
				next = PD_GID_read($
				snum,fra=fnum,sli=sli,yof=yneg[i],ang=ang,/z_int,_extra=_e)
				dum = execute(string(j,form=sform))
				dum = execute(string(j,i,form=nform))
				rres = Scan_scale(reverse(res,2),-1,/xscal)
				rnext = Scan_scale(reverse(next,2),-1,/xscal)
				res = Scan_join(rres,rnext,/rev,fact=fac,_extra=_e)
				res = Scan_scale(reverse(res,2),-1,/xscal)
				mfac = [fac[1],mfac]

				Scan_show, sp_0, next, thi=2,lcol=wcol,pos=topos,/dev,/nofile, $
				tit= string(yneg[i],form='("Yoff = ",f6.2," mm")'),xtit= xtit, $
				charsize= wchars, _extra=_e
				Legend_mm, loc = 'UL', col = wcol, line = 0, /nowrap, $
				text = ['initial.','current'], charsize= wchars
				wait, wai
			endif
		endfor

		if merfl then res = Scan_prune(res,mdel)	
		if arg_present(scl) then begin
			yval = [reverse(yneg),0.,ypos]
			scl = Join_xy(yval,mfac)
		endif
	endif else wnsp = 0

	Scan_show, sp_0, res, thi = [2,2], lcol = wcol, pos = topos, /dev, /nofile,$
	tit = tit+'!cInitial and Combined', xtit= xtit, charsize= wchars,_extra = _e
	Legend_mm, loc = 'UL', col = wcol, line = 0, /nowrap, $
	text = ['initial','combined'], charsize= wchars, extend = 1
	ntot = total(wnsp,/pres)
	lrainbow = [!rainbow,!rainbow]
	Scan_show, sp_0, sp_1, sp_2, sp_3, sp_4, sp_5, sp_6, sp_7, sp_8, $
	sp_9, sp_10, sp_11, sp_12, sp_13, sp_14, sp_15, /noerase, lcol = !rainbow, $
	pos = bopos, /dev, /nofile, tit = 'All Spectra', xtit = xtit, $
	charsize= wchars, _extra = _e
	Legend_mm, loc = 'UL', col = lrainbow[0:ntot], line = 0, /nowrap, $
	text = tsp[0:ntot], charsize= wchars, extend = 2

	Wimg_mm, Clean_name(tit), call = 2, /nodef, /verb, _extra = _e
	if cwin ge 0 then wset, cwin

	return
end