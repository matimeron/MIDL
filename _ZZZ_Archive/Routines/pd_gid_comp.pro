Function PD_GID_COMP, snum, frame = fnum, verify = ver, $
	slit = sli, ystep = yst, ylim = yli, angles = ang, auto_lim = aul, $
	progress = prg, wait = wai, title = tit, scale_fac = scl, _extra = _e

;+
; NAME:
;		PD_GID_COMP
; VERSION:
;		8.23
; PURPOSE:
;		Generates "composite" GID scan data, by combining multiple slices from 
;		each area detector frame.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = PD_GID_COMP( SNUM [,keywords])
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
;	SLIT
;		Numeric scalar, the value of the "electronic slit" (around the center
;		pixel) in *pixels*.  See PD_GID_READ for details.
;	YSTEP
;		Scalar, step in the Y direction (along the beam footprint).  PD_GID_COMB
;		takes a series of data "slices" with different values of Y_OFF (see
;		PD_GID_READ), separated by YSTEP, within the range [-YLIM, YLIM] (see
;		below. 
;		Note:	YSTEP may be rounded up or down, as needed, in application.
;	YLIM
;		Specifies the limits of the YOFF range.  If given as a scalar, the 
;		limits are [-ABS(YLIM),ABS(ylim)].  If given as a vector, the limits
;		are [YLIM[0],YLIM[1]].
;		
;		Note:	In any case, the limits may be further restricted by the 
;				observable range and the setting of AUTO_LIM (see below). 
;	/ANGLES
;		Switch.  If set the independent coordinate of the result is the angle
;		DTHETA.  Default is Q_xy.
;	AUTO_LIM
;		Specifies the fraction of the data within the observable range of each
;		frame that can be used.  See PD_PIN for more details.
;	/PROGRESS
;		Switch.  If set, interim spectra, immediate and cumulative, are 
;		displayed to the screen during processing.
;	WAIT
;		Optional time, in seconds, for the routine to stop and wait after 
;		displaying each intermediate result.  If PROGRESS is not set, WAIT has
;		no effect.
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
;		Returns a 1D data set (i.e. a [3,N] array containing the horizontal
;		GID spectrum.  The data columns are:
;			Result[0,*] = Q_xy coordinates of the image.
;			Result[1,*] = Diffracted intensity.
;			Result[2,*] = The statistical errors of the diffracted intensity.
;
;		If /ANGLES is set, the first column is replaced by the DTHETA angles.
; OPTIONAL OUTPUT PARAMETERS:
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
;		Calls PD_GID_READ and PD_PIN_RANGE.  Calls IMG_COO_CONV, SCAN_GET_ANGS,
;		SCAN_JOIN, SCAN_ORDER, SCAN_PD_CENTER, SCAN_PD_FRAMES, SCAN_PD_FTOD,
;		SCAN_PRUNE, SCAN_SCALE and SPEC_FILE_CHECK, from SPEC.  Also calls
;		DEFAULT, FLTROUND, ISNUM, JOIN_XY, LEGEND_MM, RANGE_COMP and 
;		WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-NOV-2013 by Mati Meron.
;		Modified 20-FEB-2014 by Mati Meron.  Internal streamlining.  Added
;		keywords PROGRESS and WAIT.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum,/pil,nsc=nsc,lis=lis,_extra=_e
	if nsc gt 1 then wfnum = (cfl = -1) $
	else wfnum = Scan_PD_frames(lis[0],fnum,verify=ver,/uni,comp=cfl)

	sord = Scan_order(lis,col=[0,-1,-2])
	slis = lis[sord]
	ystmin = Scan_PD_ftod(slis[0],0,xdet=sli)

	if Isnum(yst) then wyst = abs(yst) else message, 'Missing step value!'
	if wyst gt 0 and wyst lt ystmin then $
	message, 'Y-step small, minimal value to avoid ' + 'data mixing is ' $
	+ string(ystmin,form = '(f6.3)'), /con
	
	if keyword_set(prg) then begin
		prfl = 1
		if !d.window eq -1 then window, 0
		wcol = [!pcol.blue,!pcol.red,!pcol.green]
		if keyword_set(ang) then xtit = 'Dth' else xtit = 'Q!dxy!n'
		wai = Default(wai,1e-3,/dtyp)
	endif else prfl = 0

	res = (ores = $
	PD_GID_read(snum,frame=fnum,slit=sli,ang=ang,/z_int,tit=tit,_extra=_e))
	if prfl then Scan_show, res, lcol = wcol[2], xtit = xtit, $
	tit = string(0,form='("Initial, Yoff = ",f6.2," mm")'), _extra = _e

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
		case n_elements(yli) of
			0	:
			1	:	yran = [yran[0] > (-abs(yli)),yran[1] < abs(yli)]
			else:	yran = [yran[0] > yli[0],yran[1] < yli[1]]
		endcase
	
		nneg = floor(-yran[0]/wyst)
		if nneg gt 0 then yneg = -wyst*(1+findgen(nneg)) else yneg = []
		npos = floor(yran[1]/wyst)
		if npos gt 0 then ypos = wyst*(1+findgen(npos)) else ypos = []
	
		mfac = [1.]
		for i = 0, (npos > nneg) - 1 do begin
			if i lt npos then begin
				pres = res
				next = PD_GID_read($
				snum,fra=fnum,sli=sli,yof=ypos[i],ang=ang,/z_int,_extra=_e)
				res = Scan_join(res,next,fact=fac,_extra=_e)
				mfac = [mfac,fac[1]]
				if prfl then Scan_show, pres, Scan_scale(next,fac[1]), res, $
				tit= string(ypos[i],form='("Yoff = ",f6.2," mm")'),xtit= xtit, $
				thi = [2,1,1], lcol = wcol, _extra = _e
				Legend_mm, loc = 'UL', col = wcol, line = 0, /nowrap, $
				text = ['previous combined','currently read','new combined']
				wait, wai
			endif
			if i lt nneg then begin
				pres = res
				res = Scan_scale(reverse(res,2),-1,/xscal)
				pnext = PD_GID_read($
				snum,fra=fnum,sli=sli,yof=yneg[i],ang=ang,/z_int,_extra=_e)
				next = Scan_scale(reverse(pnext,2),-1,/xscal)
				res = Scan_join(res,next,/rev,fact=fac,_extra=_e)
				mfac = [fac[1],mfac]
				res = Scan_scale(reverse(res,2),-1,/xscal)
				if prfl then Scan_show, pres, Scan_scale(pnext,fac[1]), res, $
				tit= string(yneg[i],form='("Yoff = ",f6.2," mm")'),xtit= xtit, $
				thi = [2,1,1], lcol = wcol, _extra = _e
				Legend_mm, loc = 'UL', col = wcol, line = 0, /nowrap, $
				text = ['previous combined','currently read','new combined']
				wait, wai
			endif
		endfor
		if merfl then res = Scan_prune(res,mdel)
		if prfl then begin
			Scan_show, ores, res, lcol = [!pcol.blue,!pcol.green], $
			xtit = xtit, tit = 'Initial and combined', _extra = _e
			Legend_mm, loc = 'UL', col = wcol[[0,2]], line=0, /nowrap, $
			text = ['initial','combined']
		endif
	
		if arg_present(scl) then begin
			yval = [reverse(yneg),0.,ypos]
			scl = Join_xy(yval,mfac)
		endif
	endif
	
	tit = strmid(tit,0,strpos(tit,'_')) + ' S# ' + Range_comp(lis)
	if nsc eq 1 and not cfl then tit = tit + ' (' + Range_comp(wfnum)+ ')'

	return, res
end