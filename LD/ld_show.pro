Pro LD_show, sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, $
	norm=nrm, chan_range=chr, bin=bin, relaxed=rel, angles=ang, horizontal=hor,$
	image = img, surface = srf, contour = cnt, min = min, max = max, $
	smooth = smo, log = log, ax = ax, az = az, title = tit, $
	radius = rad, rthick  = rth, rline = rli, rcolor = rco, $
	xy_reg= xyr, z_reg= zr, result= res, _extra= _e

;+
; NAME:
;		LD_SHOW
; VERSION:
;		8.0
; PURPOSE:
;		2-D display of linear detector data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		LD_SHOW, SL_0,... [,keywords]
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
;				ignored, with the exception of /ANGLES and /HORIZONTAL which
;				influence plot annotation.
;	/IMAGE
;		Switch.  Specifies an Image (TV) display using IDL TVSCL.  This is also
;		the default in the absence of any specification.
;	/CONTOUR
;		Switch.  Specifies contour display using IDL CONTOUR.
;	/SURFACE
;		Switch.  Specifies surface display using IDL SHADE_SURF.
;	MIN
;		A scalar entry in the range (0,1) specifying minimal relative value to
;		be displayed.  The minimum is set at MIN*maximum(IMG) and all image
;		values less than the minimum are set to the minimum.
;	MAX
;		Same as minimum, for the maximal value to be displayed.  All image
;		values greater than the maximum are set to the maximum.
;	SMOOTH
;		Specifies Savitzky-Golay smoothing of the data.  Can be given as scalar
;		(which specifies smoothing width in both dimensions) or a 2-element
;		vector with each component providing smoothin width in the corresponding
;		dimension.
;	/LOG
;		Switch.  Specifies logarithmic display.
;	AX
;		Value, in degrees, specifying rotation around the X-axis (default is
;		45 degrees).  Valid for the /CONTOUR option, ignored otherwise.
;	AZ
;		Value, in degrees, specifying rotation around the Z-axis (default is
;		-45 degrees).  Valid for the /CONTOUR option, ignored otherwise.
;	TITLE
;		Character input to be used as the title of the plot and (optionally)
;		saved image file.
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
;	RESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to
;		embedded routines.  Not to be used directly.
;
;		***Note***:	If LDAT consists of a single LD spectrum, standard plot is
;					provided instead of the 2D display.  In this case, all the
;					keywords above, with the exception of SMOOTH, /LOG, TITLE
;					and RESULT are disabled.
;
;		Note:	If any of the keywords /PNG, /JPG or /BMP is invoked in call
;				to LD_SHOW, whether directly or through _EXTRA, the
;				appropriate image file will be generated by WIMG_MM.
; OUTPUTS:
;		None other than the graphic output and the opt. output through RESULT.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the processed data, (smoothed and cut down to region of interest
;		only.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than those mentioned above.  However, when only scan numbers
;		are provided, to be read using LD_READ, the LD_READ restrictions apply.
; PROCEDURE:
;		Data readout, if needed, performed through LD_READ.  Region processing
;		is straightforward, rest is done in embedded routines.  Calls OP_SFF.
;		Calls SCAN_SHOW from SPEC.  Also calls CLEAN_NAME, DEFAULT, DISPLAY_MM,
;		ISNUM, SMOOTH_MM, STRPARSE_MM and WIMG_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 30-NOV-2005 by Mati Meron.
;		Modified 15-DEC-2005 by Mati Meron.  Added keyword /ANGLES.
;		Modified 15-JAN-2006 by Mati Meron.  Added keyword INT_RESULT.
;		Modified 15-FEB-2006 by Mati Meron.  Added single spectrum capacity.
;		Rewritten 20-JUN-2006 by Mati Meron.  Changed to take over the role of
;		the old LD, including data reading capability but excluding integration
;		which is reserved to LD.
;		Modified 5-MAR-2007 by Mati Meron.  Added /HORIZONTAL option.
;		Modified 20-NOV-2007 by Mati Meron.  Enabled saving in JPG and BMP
;		formats (in addition to PNG).
;		Modified 10-OCT-2010 by Mati Meron.  Improved superposition of form 
;		factor curves.  Added keywords RTHICK, RLINE and RCOLOR.
;-

	on_error, 1

	siz = size(sl_0)
	if siz[0] ge 2 then res = sl_0 else $
	res = LD_read(sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, norm= nrm, $
	chan_range= chr, bin= bin, relax= rel, angles= ang, hor=hor,title= dtit, $
	_extra = _e)
	tit = Default(tit,Default(dtit,''))

	if keyword_set(ang) then begin
		xtit = 'Dth'
		ytit = 'Beta'
	endif else begin
		xtit = 'Q!dxy!n'
		ytit = 'Q!dz!n'
	endelse

	ax = Default(ax,45,/dtyp)
	az = Default(az,-45.,/dtyp)
	lfl = keyword_set(log)

	imfl = ((size(res))[0] eq 3)
	if imfl then begin

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
			message, 'Bad XY range, ignoring!'
		endif else res = res[*,dum,*]
		dum = where(res[1,0,*] ge wz[0] and res[1,0,*] le wz[1], ndum)
		if ndum eq 0 then begin
			wz = qzran
			message, 'Bad X range, ignoring!'
		endif else res = res[*,*,dum]
	endif

	if keyword_set(smo) then begin
		if imfl then begin
			res[2,*,*] = Smooth_mm(res[2,*,*],smo,/edg) > 0
			res[3,*,*] = sqrt(Smooth_mm(res[3,*,*]^2,smo,/edg) > 0)
		endif else begin
			res[1,*] = Smooth_mm(res[1,*],smo,/edg) > 0
			res[2,*] = sqrt(Smooth_mm(res[2,*]^2,smo,/edg) > 0)
		endelse
	endif

	if imfl then begin
		Display_mm, res, image= img, surface= srf, contour= cnt, zoom= zoom,$
		/auz, title = tit, xtit = xtit, ytit = ytit, max = max, min = min, $
		log = log, ax = ax, az = az, za = za, ppos= pps, range= ran, _extra = _e
		if Isnum(rad) and not keyword_set(ang) then $
		Op_sff, rad, ppos = pps, ran = ran, thick = rth, line = rli, color = rco
	endif else begin
		if lfl then begin
			dum = where(res[1,*] gt 0, ndum)
			if ndum eq 0 then off = 1 else off = min(res[1,dum]) < 1
			res[1,*] = (res[1,*] > 0) + off
		endif
		if not keyword_set(hor) then xtit = ytit
		Scan_show, res, ylog= log, tit= tit, xtit= xtit, /nofile, _extra = _e
	endelse

	Wimg_mm, Clean_name(tit), call = 2, /nodef, /verb, _extra = _e

	return
end