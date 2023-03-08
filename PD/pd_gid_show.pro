Pro PD_GID_show, snum, verify= ver, hroi=hroi, vroi=vroi, raw=raw, angles=ang, $
	signed= sgn, relaxed= rel, surface= srf, contour= cnt, min= min, max= max, $
	smooth = smo, log = log, ax = ax, az = az, title = tit, $
	radius = rad, rthick  = rth, rline = rli, rcolor = rco, $
	xy_reg = xyr, z_reg = zr, result = res, _extra = _e

;+
; NAME:
;		PD_GID_SHOW
; VERSION:
;		8.33
; PURPOSE:
;		2-D display of patched PD data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_GID_SHOW, SNUM [,keywords]
; INPUTS:
;	SNUM
;		A list of scan numbers, in any form recognizable by PD_GID_READ.  If
;		more than one scan is provided, PD_GID_READ will attempt patching in the
;		horizontal direction.
;
;		Alternatively, SNUM may already contain PD data in the form returned by
;		PD_GID_READ.  In such case it is used as is.  Note that in this case the
;		default Q-coordinates are assumed.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	HROI
;		Two element vector defining horizontal region of interest, in *pixels*.
;	VROI
;		Two element vector defining vertical region of interest, in *pixels*.
;	/RAW
;		Switch.  If set, the coordinates of the image are "frame #" for
;		horizontal and "pixel #" for vertical, instead of Q_xy and Q_z.  Note
;		that if multiple scans are patched, the frame number listed does not
;		correspond to a frame number in any specific scan.
;	/ANGLES
;		Switch.  If set the coordinates of the image are the angles DTHETA (for
;		horizontal) and BETA for vertical, instead of Q_xy and Q_z.
;	/SIGNED
;		Switch.  If set, Q_xy is multiplied by the sign of the DTH angle.
;		/SIGNED has no effect when /ANGLES is set.
;	/RELAXED
;		Switch.  If set, the internal transformation from angles to Q-values is
;		unconstrained.  By default it is constrained to the internal data
;		region, to avoid edge inaccuracies.
;	/SURFACE
;		Switch.  Specifies surface display using IDL SHADE_SURF.
;	/CONTOUR
;		Switch.  Specifies contour display using IDL CONTOUR.
;	MIN
;		A scalar entry in the range (0,1) specifying minimal relative value to
;		be displayed.  The minimum is set at MIN*maximum(IMG) and all image
;		values less than the minimum are set to the minimum.
;	MAX
;		Same as minimum, for the maximal value to be displayed.  All image
;		values greater than the maximum are set to the maximum.
;	SMOOTH
;		Specifies smoothing (bincoef, by default of the data.  Can be given as 
;		scalar (which specifies smoothing width in both dimensions) or a 
;		2-element vector with each component providing smoothing width in the
;		corresponding dimension.
;	/LOG
;		Switch.  Specifies logarithmic display.
;	AX
;		Value, in degrees, specifying rotation around the X-axis (default is
;		45 degrees).  Valid for the /SURFACE option, ignored otherwise.
;	AZ
;		Value, in degrees, specifying rotation around the Z-axis (default is
;		-45 degrees).  Valid for the /SURFACE option, ignored otherwise.
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
;
;		***Note***:	If the data consists of a single horizontally integrated PD
;					file, standard plot is provided instead of the 2D display.
;					In this case, all the keywords above, with the exception of
;					SMOOTH, /LOG, TITLE and RESULT are disabled.
;	RESULT
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
;		Note:	/NORM is set by default in PD_GID_SHOW, through PD_GID_READ. 
;				It can be disabled by using NORM = 0 in the function call.
;
;		Note:	If any of the keywords /PNG, /JPG or /BMP is invoked in call
;				to PD_GID_SHOW, whether directly or through _EXTRA, the
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
;		are provided, to be read using PD_GID_READ, the PD_GID_READ restrictions
;		apply.
; PROCEDURE:
;		Data readout, if needed, performed through PD_GID_READ.  Region
;		processing is straightforward, rest is done in embedded routines.
;		Calls OP_SFF.  Calls IMG_SMOOTH and SCAN_SHOW from SPEC.  Also calls 
;		CLEAN_NAME, DEFAULT, DISPLAY_MM, ISNUM, ONE_OF, SMOOTH_MM and WIMG_MM,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 1-NOV-2007 by Mati Meron as a minor modification (changes only
;		in the input part) of LD_SHOW.
;		Modified 20-NOV-2007 by Mati Meron.  Enabled saving in JPG and BMP
;		formats (in addition to PNG).
;		Modified 10-MAY-2008 by Mati Meron.  Changed input scheme and added
;		keyword RAW.
;		Modified 5-FEB-2010 by Mati Meron.  Added keyword VERIFY.
;		Modified 10-OCT-2010 by Mati Meron.  Improved superposition of form 
;		factor curves.  Added keywords RTHICK, RLINE and RCOLOR.
;		Modified 5-OCT-2010 by Mati Meron.  Internal changes.	
;-

	on_error, 1

	typ = 1 - One_of(ang,raw)
	xytit = [['frames','Dth','Q!dxy!n'],['pix.','Beta','Q!dz!n']]
	xtit = xytit[typ,0]
	ytit = xytit[typ,1]

	if (size(snum))[0] ge 2 then res = snum else $
	res = PD_GID_read(snum, veri=ver,hroi=hroi, vroi=vroi, raw=raw, angles=ang,$
		signed=sgn, relaxed=rel,title= dtit, _extra = _e)
	tit = Default(tit,Default(dtit,''))

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
		if not imfl then begin
			res[1,*] = Smooth_mm(res[1,*],smo,/edg) > 0
			res[2,*] = Smooth_mm(res[2,*],smo,/edg,/err) > 0
		endif else res = Img_smooth(res,smo)
	endif

	if imfl then begin
		Display_mm, res, image= img, surface= srf, contour= cnt, zoom= zoom,$
		/auz, title = tit, xtit = xtit, ytit = ytit, max = max, min = min, $
		log = log, ax = ax, az = az, ppos= pps, range= ran, _extra = _e
		if Isnum(rad) and typ eq 2 then $
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