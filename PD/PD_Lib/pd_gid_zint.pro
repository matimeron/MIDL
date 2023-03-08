Pro PD_GID_zint, snum, xy_reg = xyr, z_reg = zr, title = tit, log = log, $
	raw = raw, angles = ang, bare = bar, result = res, _extra = _e

;+
; NAME:
;		PD_GID_ZINT
; VERSION:
;		7.05
; PURPOSE:
;		Integration and display of patched PD data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_GID_ZINT, SNUM... [,keywords]
; INPUTS:
;	SNUM
;		A list of scan numbers, in any form recognizable by RANGE_PROC.  If more
;		than one scan is provided, PD_GID_READ will attempt patching in the
;		horizontal direction.
;		Alternatively, SNUM may already contain PD data in the form returned by
;		PD_GID_READ.  In such case it is used as is.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	XY_REG
;		A two element vector specifying Qxy region of interest in [min,max]
;		order.  Optionally, a scalar specifying the lower limit of the ROI (with
;		the higher limit provided by the data.  If not given, the region is
;		determined by the Qxy values present in the data.
;	Z_REG
;		Same as XY_REG, for Qz.
;
;		Note:  XY_REG and Z_REG can be both specified, to define a rectangular
;		region of interest.
;	TITLE
;		Optional title to be used in the graphics output.  If not provided, a
;		title is generated internally.
;	/LOG
;		Switch.  Specifies logarithmic display (note that 1 is added to the
;		diplayed result in this case.
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
;	/BARE
;		Switch.  If set, subtitle is not printed and plot area is expanded.
;	RESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  Not to be used directly.
; OUTPUTS:
;		None other than the graphic output and the opt. output through RESULT.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the integration result in the standard format of [3,N] array,
;		where the first column contains the Q_z-values, the second - the
;		integrated data and the third - the statistical error.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, integrates the data over Q_z (within the range
;		specified by keywords) through a call to PD_GID_INT, and diplays the
;		result through a call to SCAN_SHOW.  Calls DEFAULT, ONE_OF and TOLER
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 1-NOV-2007 by Mati Meron as a minor modification (changes only
;		in the input part) of LD_ZINT.
;		Modified 10-MAY-2008 by Mati Meron.  Added keyword /RAW.
;-

	on_error, 1

	typ = 1 - One_of(ang,raw)
	xytit = [['frame','Dth','Q!dxy!n'],['pix','Beta','Q!dz!n']]
	xtit = xytit[typ,0]
	otit = xytit[typ,1]

	res = PD_GID_int(snum, xy_reg=xyr,z_reg=zr,/z_int,xy_ran=wxy,z_ran=wz,$
			raw=raw,angles=ang,tit=gtit,_extra=_e)

	if keyword_set(bar) then begin
		ymar = [4,2]
		subtit = ''
	endif else begin
		ymar = [6,2]
		swxy = strcompress('[' + strjoin(string(wxy,form='(f7.3)'),',') + ']')
		swz  = strcompress('[' + strjoin(string(wz, form='(f7.3)'),',') + ']')
		subtit= strjoin([xtit+'_range = '+swxy, otit+'_range = '+swz],'  ;  ')
	endelse

	pres = res
	if keyword_set(log) then begin
		dum = where(pres[1,*] gt (Toler() > 1e-4*max(pres[1,*])), ndum)
		if ndum eq 0 then off = 1 else off = min(pres[1,dum]) < 1
		pres[1,*] = (pres[1,*] > 0) + off
	endif

	Scan_show, pres, ylog = log, ymar = ymar, xstyle = 1, $
	tit = Default(tit, gtit), subtit = subtit, xtit = xtit, _extra = _e

	return
end