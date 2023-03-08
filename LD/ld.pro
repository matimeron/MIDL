Pro LD, sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, $
	norm=nrm, chan_range=chr, bin=bin, relaxed=rel, angles=ang, horizontal=hor,$
	xy_region = xyr, z_region = zr, smooth = smo, log = log, title = tit, $
	min = min, max = max, zbin = zbin, offset = off, tiny = tin, $
	radius = rad, rthick  = rth, rline = rli, rcolor = rco, $
	result = res, xy_result = xy_res, z_result = z_res, $
	output= out, full_siz= ful, half_siz= hlf, _extra = _e

;+
; NAME:
;		LD
; VERSION:
;		8.0
; PURPOSE:
;		Readout and display of linear detector data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		LD, SL_0,... [,keywords]
; INPUTS:
;	SL_0, SL_1, SL_2, SL_3, SL_4, SL_5, SL_6, SL_7
;		List of linear detector scans, provided in any form that is acceptable
;		by SCAN_LIST_VER.  If more than one input is provided, LD will attempt
;		patching in the horizontal direction.
;
;		Alternatively, SL_O may already contain linear detector data in the form
;		returned by LD_READ.  In such case it is used as is and any additional
;		inputs (if any) are ignored.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	NORM
;		Specifies data normalization.  If NORM is set (with nonzero numeric
;		value) the data is normalized to MONC.  This can be overriden by
;		giving NORM as character value in which case this value specifies
;		that column to be used for normalization.
;	CHAN_RANGE
;		The range of channels to use in the readout.  Default is full range
;		present in the data.
;	BIN
;		Specifies the bin size to be applied to each scan.  Default is 1, i.e.
;		no binning.  Any size provided is always rounded downward to the nearest
;		power of 2 (but the resulting binsize is never smaller than 1).
;		Note:	When both CHAN_RANGE and BIN are present, the CHAN_RANGE values
;				are taken as pre-binned values.
;	/RELAXED
;		Switch.  If set, the internal transformation from angles to Q-values is
;		unconstrained.  By default it is constrained to the internal data
;		region, to avoid edge inaccuracies.
;
;		Note:  In the previous version of LD, the default option was
;		unconstrained and the keyword /TIGHT had to be used to constrain it.
;		While /TIGHT is still recognized, it has no effect.
;	/ANGLES
;		Switch.  If set the coordinates of the image are the angles DTHETA (for
;		horizontal) and BETA for vertical, instead of Q_xy and Q_z.
;	/HORIZONTAL
;		Switch.  If set, the LD is assumed to be positioned horizontally
;		(default is vertically).
;
;		Note:	The six keywords above are relevant to data readout.  In the
;				case where SL_0 contains pre-read data, these keywords are
;				ignored, with the exception of /ANGLES which influences plot
;				annotation.
;	XY_REG
;		A two element vector specifying Qxy region of interest in [min,max]
;		order.  If given, only data within the range will be displayed.
;	Z_REG
;		Same as XY_REG, for Qz.
;
;		Note:  XY_REG and Z_REG can be both specified, to define a rectangular
;		region of interest.
;		Note:  If either XY_reg or Z_REG is provided as a single value, this
;		value is taken as the bottom of the range.
;	SMOOTH
;		Specifies Savitzky-Golay smoothing of the data.  Can be given as scalar
;		(which specifies smoothing width in both dimensions) or a 2-element
;		vector with each component providing smoothin width in the corresponding
;		dimension.
;	/LOG
;		Switch.  Specifies logarithmic display.
;	TITLE
;		Character input to be used as the title of the plot and (optionally)
;		saved image file.
;	MIN
;		A scalar entry in the range (0,1) specifying minimal relative value to
;		be displayed.  The minimum is set at MIN*maximum(IMG) and all image
;		values less than the minimum are set to the minimum.
;	MAX
;		Same as minimum, for the maximal value to be displayed.  All image
;		values greater than the maximum are set to the maximum.
;	ZBIN
;		Specifies bin size for the embedded routine LD_ZBIN (see there).  Any
;		value, not just power of 2, may be used.  Default value is 64 (in terms
;		of original, pre-binned channels.  Check out LD_ZBIN for more details.
;	OFFSET
;		Scalar value, specifies the OFFSET parameter for LD_ZBIN.  See there.
;	/TINY
;		Switch, specifies reduced size display.
;	RADIUS
;		Value, in Angstrem, of a scattering sphere's radius.  If provided and
;		non-zero, a set of curves  representing the minima of the scattering
;		spherical form factor are superimposed on the displayed image.
;	RTHICK
;		Integer value, specifies the thickness of the form factor curves.
;		Default is 1
;	RLINE
;		Integer value, specifies the linestyle of the form factor curves.
;		Default is 0 (solid).
;	RCOLOR
;		A True Color value, specifies the color or the form factor curves.  
;		Default is white.
;	RESULT
;		Optional output, see below.
;	XY_RESULT
;		Optional output, see below.
;	Z_RESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to
;		embedded routines.  Not to be used directly.
;
;		Note:	If any of the keywords /PNG, /JPG or /BMP is invoked in call
;				to LD, whether directly or through _EXTRA, the
;				appropriate image file will be generated by WIMG_MM.
; OUTPUTS:
;		None other than the graphic output and the opt. outputs.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the processed data, (smoothed and cut down to region of interest
;		only.
;	XY_RESULT
;		Returns the integrated (over Q_xy) data.
;	Z_result
;		Returns the integrated (over Q_z) data.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The restrictions of LD_READ.  Also, a single spectrum is not acceptable
;		as it doesn't translate into a 2D image.
; PROCEDURE:
;		Data readout, if needed, performed through LD_READ.  Region processing
;		is straightforward, rest is done in embedded routines.  Calls LD_READ,
;		LD_XYINT, LD_ZINT, LD_ZBIN and OP_SFF.  Also calls CLEAN_NAME, DEFAULT,
;		DISPLAY_MM, ISNUM, LABELS, PLVAR_KEEP, SMOOTH_MM, STRPARSE_MM and
;		WIMG_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 30-NOV-2005 by Mati Meron.
;		Modified 15-DEC-2005 by Mati Meron.  Added keyword /ANGLES.
;		Modified 15-JAN-2006 by Mati Meron.  Added keyword INT_RESULT.
;		Modified 15-FEB-2006 by Mati Meron.  Added single spectrum capacity.
;		Modified 20-JUN-2006 by Mati Meron.  Changed to display multiple plots
;		within a single panel.  Significant internal changes.
;		Modified 5-MAR-2007 by Mati Meron.  Added /HORIZONTAL option.
;		Modified 20-NOV-2007 by Mati Meron.  Enabled saving in JPG and BMP
;		formats (in addition to PNG).
;		Modified 10-OCT-2010 by Mati Meron.  Improved superposition of form 
;		factor curves.  Added keywords RTHICK, RLINE and RCOLOR.
;-

	on_error, 1

	wnum = 24l
	bsiz = 32l
	xsi = [31, 22]*bsiz
	ysi = [30, 21]*bsiz
	pof = [[0.0, 9.5],[0.0, 9.0]]*bsiz
	auz = [512l, 256l]
	ylb = [[1.00, 0.98],[1.01, 0.98]]
	reg = 	[[0.655,0.34,1.,0.935],[0.0,0.,0.585,0.33],[0.58,0.,0.965,0.35],$
			 [0.53,0.465,1.,0.935],[0.,0.,0.475,0.46],[0.475,0.,0.95,0.46]]

	tifl = keyword_set(tin)
	dbin = 1 + tifl
	wbin = Default(bin,dbin,/dtyp) > dbin

	siz = size(sl_0)
	if siz[0] eq 3 then res = sl_0 else $
	res = LD_read(sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, norm= nrm,$
	chan_range= chr, bin= wbin, relax= rel, angles= ang, title= dtit,_extra= _e)
	if (size(res))[0] ne 3 then message, 'Invalid Data!'
	tit = Default(tit,Default(dtit,''))

	if keyword_set(ang) then begin
		xtit = 'Dth'
		ytit = 'Beta'
	endif else begin
		xtit = 'Q!dxy!n'
		ytit = 'Q!dz!n'
	endelse
	if keyword_set(nrm) then utit = 'a.u' else utit = 'Counts'
	lfl = keyword_set(log)

	qxyran = [min(reform(res[0,*,0]),max=maxx),maxx]
	qzran = [min(reform(res[1,0,*]),max=maxx),maxx]

	case n_elements(xyr) of
		0	:	wxy = qxyran
		1	:	wxy = [xyr[0],qxyran[1]]
		2	:	wxy = 1.*xyr
		else:	wxy = 1.*xyr[0:1]
	endcase
	wxy[0] = wxy[0] > qxyran[0]
	wxy[1] = wxy[1] < qxyran[1]

	case n_elements(zr) of
		0	:	wz = qzran
		1	:	wz = [zr[0],qzran[1]]
		2	:	wz = 1.*zr
		else:	wz = 1.*zr[0:1]
	endcase
	wz[0] = wz[0] > qzran[0]
	wz[1] = wz[1] < qzran[1]

	dum= where(res[0,*,0] ge wxy[0] and res[0,*,0] le wxy[1],ndum)
	if ndum eq 0 then begin
		wxy = qxyran
		message, 'Bad XY range, ignoring!', /continue
	endif else res = res[*,dum,*]
	dum = where(res[1,0,*] ge wz[0] and res[1,0,*] le wz[1], ndum)
	if ndum eq 0 then begin
		wz = qzran
		message, 'Bad X range, ignoring!', /continue
	endif else res = res[*,*,dum]

	if keyword_set(smo) then begin
			res[2,*,*] = Smooth_mm(res[2,*,*],smo,/edg) > 0
			res[3,*,*] = sqrt(Smooth_mm(res[3,*,*]^2,smo,/edg) > 0)
	endif

	Plvar_keep, act = 'sav'

	window, wnum, xsiz = xsi[tifl], ysiz = ysi[tifl], $
	tit = strcompress('IDL ' + string(wnum) + ' : Linear Detector Results')

	Display_mm, res, min = min, max = max, poff = pof[*,tifl], auz = auz[tifl],$
	tit = 'Raw Data', xtit = xtit, ytit = ytit, log = log, $
	ppos = pps, range = ran, _extra = _e

	swxy = strcompress('[' + strjoin(string(wxy,form='(f7.3)'),',') + ']')
	swz  = strcompress('[' + strjoin(string(wz, form='(f7.3)'),',') + ']')
	itit = strjoin([xtit+ ' range = '+ swxy, ytit+ ' range = '+ swz],'  ;  ')

	plot, [0], [0], /nodata, /noerase, xstyle= 4, ystyle= 4
	Labels, [0.5,0.5], ylb[*,tifl], [tit, itit], align=0.5, charsize=1.2

	!p.region = reg[*,3*tifl]
	LD_xyint, res, log = log, angles = ang, /bare, res = xy_res, /noerase, $
	/flip, title = 'Integrated over ' + xtit, $
	xtit = utit, ytit='', ytickname = replicate(' ',30), xmargin = [4,8]
	axis, yaxis = 1, ystyle = 1, ytit = ytit

	!p.region = reg[*,3*tifl+1]
	LD_zint, res, log = log, angles = ang, /bare, res = z_res, /noerase, $
	ytit = utit, title = 'Integrated over ' + ytit, xmargin = [9,3]

	!p.region = reg[*,3*tifl+2]
	LD_zbin, res, zbin = zbin, off = off, log = log, angles = ang, /zpos, $
	/noerase, title = ytit + '-binned', ytit = 'a.u'

	Plvar_keep, act = 'res'
	if Isnum(rad) and not keyword_set(ang) then $
	Op_sff, rad, ppos = pps, ran = ran, thick = rth, line = rli, color = rco

	wshow, wnum
	Wimg_mm, Clean_name(tit), call = 2, /nodef, /verb, _extra = _e

	return
end