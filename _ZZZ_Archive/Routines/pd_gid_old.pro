Pro PD_GID_old, sp_0, sp_1, sp_2, sp_3, sp_4, sp_5, sp_6, sp_7, $
	frame = fnum, verify = ver, hroi= hroi, vroi= vroi, slit= sli, yoff= yof, $
	raw = raw, angles = ang, signed = sgn, relaxed = rel, sin_norm = snr, $
	patch_range=ptr, clean = cle, location = loc, bsmooth = bsm, smooth = smo, $
	xy_region = xyr, z_region = zr, log = log, title = tit, min= min, max= max,$
	radius = rad, rthick  = rth, rline = rli, rcolor = rco, grid = gri, $
	zbin = zbin, offset = off, progress= prg, tiny= tin, last= lst, wnew= new, $
	result = res, xy_result = xy_res, z_result = z_res, _extra = _e

;+
; NAME:
;		PD_GID
; VERSION:
;		8.42
; PURPOSE:
;		Readout and display of PD GID data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_GID, SP_0... [,keywords]
; INPUTS:
;	SP_0, SP_1, SP_2, SP_3, SP_4, SP_5, SP_6, SP_7
;		List(s) of scan numbers, in any form recognizable by PD_GID_READ.  If
;		more than one scan is provided, PD_GID_READ will attempt patching in the
;		horizontal direction.
;
;		Alternatively, SP_O may already contain PD data in the form returned by
;		PD_GID_READ.  In such case it is used as is and additional inputs (if
;		any) are ignored.  Note that in this case Q-coordinates are assumed as
;		a default.  This can be changed using /RAW or /ANGLES.
;
;		Important:	If the list in SNUM includes scans with more than one value
;					of Beta, the scans belonging to each value of Beta will be
;					combined horizontally separately, with the results then
;					being patched vertically.
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
;	HROI
;		Two element vector defining horizontal region of interest, in *pixels*.
;	VROI
;		Two element vector defining vertical region of interest, in *pixels*.
;	SLIT
;		Numeric scalar, the value of the "eletronic slit" (around the center
;		pixel) in *pixels*.  Defines HROI which overrides directly given HROI
;		(if any).
;	YOFF
;		Numeric scalar, specifying Y_offset along the beam footprint.  If given,
;		results in appropriate offset in HROI.  Ignored if HROI or SLIT not 
;		given.
;	/RAW
;		Switch.  If set, the coordinates of the image are "frame #"	|
;		forhorizontal and "pixel #" for vertical, instead of Q_xy	|
;		and Q_z.  Note that if multiple scans are patched, the frame|	Only one
;		number listed does not correspond to a frame number in any	|	of these
;		specific scan.												|	two may
;	/ANGLES															|	be used.
;		Switch.  If set the coordinates of the image are the angles	|
;		DTHETA (for horizontal) and BETA for vertical, instead of	|
;		Q_xy and Q_z.												|
;	/SIGNED
;		Switch.  If set, Q_xy is multiplied by the sign of the DTH angle.
;		/SIGNED has no effect when /ANGLES is set.
;	/RELAXED
;		Switch.  If set, the internal transformation from angles to Q-values is
;		unconstrained.  By default it is constrained to the internal data
;		region, to avoid edge inaccuracies.
;	/SIN_NORM
;		Specifies additional geometric normalization, consisting of multiplying
;		each column by Sin(dth)/Sin(min(dth)).  This corrects for the changing
;		field of view in Pinhole scans.
;
;		Note:  	/SIN_NORM is set by default in PD_GID, it can be disabled by
;				using SIN_NORM = 0 in the function call.
;	PATCH_RANGE
;		The range of channels to use in patching (only when more than one data
;		set is present).  Note that PATCH_RANGE values are taken as original
;		values, before VROI is applied.
;
;		Note:	The seven keywords above are relevant to data readout.  In the
;				case where SP_0 contains pre-read data, these keywords are
;				ignored.
;	CLEAN
;		Specifies width for 2D background subtraction.  See PD_GID_CLEAN for
;		details.
;	LOCATION
;		2-element vector, specifies the location (in X-coordinates) of the
;		columns to be used for background, on left and right.  By default,
;		the extreme left and extreme right column(s) are used.  Only valid in
;		conjuction with CLEAN.
;	BSMOOTH
;		Scalar, specifies the smoothing width for smoothing background data in
;		the Z direction.  Default is 1, i.e. no smoothing.  Binomial coefficient
;		smoothing is used.  Only valid in conjuction with CLEAN.
;	SMOOTH
;		Provides smoothing width for data smoothing.  Can be given as scalar
;		(which specifies smoothing width in both dimensions) or a 2-element
;		vector with each component providing smoothin width in the corresponding
;		dimension.  Binomial coefficients smoothing is used as default,
;		alternatively Savitzki-Golay or box averaging may be applied (see
;		SMOOTH_MM for details.
;
;		Note:	When 2D smoothing is applied, the data values are no longer
;				independent.  This has bearing on error estimates.
;	XY_REG
;		A two element vector specifying xy region of interest in [min,max]
;		order.  If given, only data within the range will be displayed.
;	Z_REG
;		Same as XY_REG, for z.
;
;		Note:	XY and Z are defined in terms of the coordinates used, whether
;				RAW, ANGLES or QVAL.
;		Note:	XY_REG and Z_REG can be both specified, to define a rectangular
;				region of interest.
;		Note:	If either XY_reg or Z_REG is provided as a single value, this
;				value is taken as the bottom of the range.
;
;		Note:	CLEAN, SMOOTH, XY_REG and Z_REG influence the data being
;				displayed and output, but not the data saved in memory under 
;				LAST.
;		Note:	The order of application is:  	CLEAN (if specified) first
;												SMOOTH (if specified) second
;												regions (if specified) third
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
;	/GRID
;		Switch.  If set, a grid is displayed on top of the image.
;	ZBIN
;		Specifies bin size for the embedded routine PD_ZBIN (see there).  Any
;		value, not just power of 2, may be used.  Default value is 64 (in terms
;		of original, pre-binned channels.  Check out PD_ZBIN for more details.
;	OFFSET
;		Scalar value, specifies the OFFSET parameter for PD_ZBIN.  See there.
;	/PROGRESS
;		Switch.  If set, the evaluation progress is printed to the screen.
;	/TINY
;		Switch.  If set, the display window is reduced in size, making it
;		aappropriate for small laptops.
;	/LAST
;		Switch.  If set, the last processed data is reused.  User still has the
;		option to change regions, smoothing, linear-log display etc.
;	/WNEW
;		Switch.  If set, a new graphics window is created instead of the old
;		one being overwritten.  The windows at the disposal of PD_GID are 24-27,
;		when the last one is reached the sequence reverts to the origin.
;	RESULT
;		Optional output, see below.
;	XY_RESULT
;		Optional output, see below.
;	Z_RESULT
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
;		Note:	/NORM is set by default in PD_GID, through PD_GID_READ.
;				It can be disabled by using NORM = 0 in the function call.
;
;		Note:	If any of the keywords /PNG, /JPG or /BMP is invoked in call
;				to PD_GID, whether directly or through _EXTRA, the
;				appropriate image file will be generated by WIMG_MM.
; OUTPUTS:
;		None other than the graphic output and the opt. outputs.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the processed data, (smoothed and cut down to region of interest
;		only).
;	XY_RESULT
;		Returns the integrated (over Q_xy) data.
;	Z_RESULT
;		Returns the integrated (over Q_z) data.
; COMMON BLOCKS:
;		PD_GID_KEEP.  Contains data pertaining to the last processed PD_GID
;		data, as follows:
;
;		EXS		-	Flag, value of 1 indicates that the common block is defined.
;		FTYP	-	Last data type, RAW (0), ANGLES (1) or QVALS (2).
;		LRES	-	Last Result, i.e. data array.
;		LTIT	-	Last title.
;		LUTIT	-	Last "units title", Counts for raw or NCounts for normalized
;		LWIN	-	Index of last window used.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The restrictions of PD_GID_READ.
; PROCEDURE:
;		Data readout, if needed, performed through PD_GID_READ.  Region 
;		processing is straightforward, rest is done in embedded routines.
;		Calls PD_GID_CLEAN, PD_GID_READ, PD_GID_XYINT, PD_GID_ZINT, PD_GID_ZBIN
;		and OP_SFF.  Calls IMG_EQUALIZE, IMG_SMOOTH and SCAN_PD_CENTER, from 
;		SPEC.  Also calls ARRPACK, CLEAN_NAME, DEFAULT, DISPLAY_MM, ISNUM, 
;		LABELS, ONE_OF, SMOOTH_MM, WHERINSTRUCT and WIMG_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-NOV-2007 by Mati Meron, based on LD.
;		Modified 20-NOV-2007 by Mati Meron.  Enabled saving in JPG and BMP
;		formats (in addition to PNG).
;		Modified 10-MAY-2007 by Mati Meron.  Generalized input pattern and
;		improved graphics.  Added vertical patching capability.  Added keywords
;		WNEW and LAST and the common block PD_GID_KEEP.
;		Modified 15-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 5-MAR-2009 by Mati Meron.  Added keyword SIN_NORM.
;		Modified 5-FEB-2010 by Mati Meron.  Added keyword VERIFY.
;		Modified 10-OCT-2010 by Mati Meron.  Improved superposition of form 
;		factor curves.  Added keywords RTHICK, RLINE and RCOLOR.
;		Modified 5-APR-2011 by Mati Meron.  Added keywords SLIT and YOFF.
;		Modified 1-APR-2013 by Mati Meron.  Internal changes.
;		Modified 25-OCT-2013 by Mati Meron.  Enabled variable spacing of result
;		Qxy coordinates.  Some internal changes.
;		Modified 15-NOV-2013 by Mati Meron.  Internal changes.  Added keyword 
;		GRID.
;		Modified 15-FEB-2014 by Mati Meron.  Internal changes, transferred all
;		readout and patching related function to PD_GID_READ, leaving PD_GID as 
;		display routine.
;		Modified 15-JUN-2014 by Mati Meron.  Internal changes.
;		Modified 5-OCT-2014 by Mati Meron.  Changed default smoothing from
;		Savitzki-Golay to Binomial.  Added keywords CLEAN, LOCATION and BSMOOTH.
;		Some internal changes.
;		Modified 5-MAY-2015 by Mati Meron.  Display bug fix.
;-

	common pd_gid_keep, exs, ltyp, lres, ltit, lutit, lwin
	on_error, 1

	bwnum = 24l
	wpad = 64l
	bsiz = 32l
	xsi = [31, 22]*bsiz
	ysi = [30, 21]*bsiz
	pof = [[0.25, 9.5],[0.25, 8.5]]*bsiz
	prl = [8,7]*bsiz
	auz = [512l, 256l]

	tifl = keyword_set(tin)
	xsi = xsi[tifl]
	ysi = ysi[tifl]
	pof = pof[*,tifl]
	prl = prl[tifl]
	auz = auz[tifl]

	if keyword_set(lst) then begin
		if Isnum(exs) then begin
			typ = ltyp
			res = lres
			siz = size(res)
			tit = Default(tit,ltit,/dtyp)
			utit = lutit
		endif else message, 'There is no previous data!'
	endif else begin
		typ = 1 - One_of(ang,raw)
		if (Wherinstruct('norm',_e))[0] ge 0 then utit='Ncounts' $
		else utit='Counts'

		if (size(sp_0))[0] ne 3 then begin
			snum = Arrpack(sp_0,sp_1,sp_2,sp_3,sp_4,sp_5,sp_6,sp_7)
			res = PD_gid_read(snum,frame=fnum,progress=prg,verify=ver,slit=sli,$
			yoff =yof,hroi= hroi,vroi= vroi,patch_range=ptr,raw=raw,angles=ang,$
			sign=sgn,relax=rel,sin_norm=snr,title=dtit,_extra=_e)
		endif else res = sp_0

		siz = size(res)
		if siz[0] ne 3 then message, 'Invalid Data!'
		tit = Default(tit,Default(dtit,''))

		exs = 1
		ltyp = typ
		lres = res
		ltit = tit
		lutit = utit
	endelse

	if Isnum(cle) then res = PD_GID_clean(res,cle,loc=loc,bsm=bsm)
	if Isnum(smo) then begin
		res = Img_smooth(res,smo,ker=ker,_extra=_e)
		erm = SM_ermult(ker)
	endif else erm = [1.,1.]

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

	xytit = [['frame','Dth','Q!dxy!n'],['pix','Beta','Q!dz!n']]
	xtit = xytit[typ,0]
	ytit = xytit[typ,1]

	if keyword_set(new) then begin
		lwin = Default(lwin,bwnum-1)
		wnum = bwnum + ((lwin - bwnum + 1) mod 4)
	endif else wnum = Default(lwin,bwnum)
	lwin = wnum

	dres = Img_equalize(res,/xonly,/down)
	bin = ceil(1.*(size(dres))[2:3]/auz)
	Display_mm, dres, auz= auz, bin= bin, wsi=wsi, /nodata
	xsi = wsi[0] + prl + 3*wpad/4
	ysi = wsi[1] + prl + 3*wpad/2
	window, wnum, xsiz = xsi, ysiz = ysi, $
	tit = strcompress('IDL ' + string(wnum) + ' : PD GID Results')
	Display_mm, dres, min= min, max= max, poff= pof, auz= auz, bin= bin, /ave, $
	tit= 'Raw Data', xtit=xtit, ytit=ytit, log=log, grid=gri, isi=isi, wsi=wsi,$
	ppos = pps, ran = ran, _extra=_e
	if Isnum(rad) and typ eq 2 then $
	Op_sff, rad, ppos = pps, ran = ran, thick = rth, line = rli, color = rco

	pos = [0,0,xsi-1,ysi-1]
	if typ eq 0 then form = '(i0)' else form = '(f7.3)'
	swxy = strcompress('[' + strjoin(string(wxy,form=form),',') + ']')
	swz  = strcompress('[' + strjoin(string(wz, form=form),',') + ']')
	itit = strjoin([xtit+ ' range = '+ swxy, ytit+ ' range = '+ swz],'  ;  ')
	Labels, xsi/2*[1,1], ysi-bsiz/8*[5,10], [tit,itit], align=0.5,chars=1.2,/dev

	pos = [xsi-prl-wpad,pof[1]+wpad,xsi-wpad-1,pof[1]+isi[1]+wpad-1]
	PD_GID_xyint, res, log = log, raw = raw, angles = ang, /bare, res = xy_res,$
	/flip, /noerase, title = 'Integrated over ' + xtit, position = pos, /dev, $
	/nofile, xtit = utit, ytit= '', ytickname= replicate(' ',30), ermult= erm[1]
	axis, yaxis = 1, ystyle = 1, ytit = ytit, _extra = _e

	if (Wherinstruct('num',_e))[0] ge 0 then ner = 1 else ner = 0
	pos = [pof[0]+wpad,3*wpad/4,pof[0]+isi[0]+wpad-1,prl+3*wpad/4-1]
	PD_GID_zint, res, log = log, raw = raw, angles = ang, /bare, res = z_res, $
	noerr=ner, /noerase, ytit= utit, title ='Integrated over '+ ytit, $
	position= pos, /dev, /nofile, ermult = erm[0], _extra = _e

	pos = [xsi-prl-wpad,3*wpad/4,xsi-wpad-1,prl+3*wpad/4-1]
	PD_GID_zbin, res, zbin = zbin, off = off, log = log, raw= raw, angles= ang,$
	/zpos, /noerase, title= ytit + '-binned', ytit= 'a.u', $
	labco = !pcol.purple, position = pos, /dev

	wshow, wnum
	Wimg_mm, Clean_name(tit), call = 2, /nodef, /verb, _extra = _e

	return
end