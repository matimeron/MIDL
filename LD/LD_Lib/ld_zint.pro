Pro LD_zint, sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, $
	xy_reg = xyr, z_reg = zr, title = tit, log = log, angles = ang, bare = bar,$
	result= res, _extra= _e

;+
; NAME:
;		LD_ZINT
; VERSION:
;		5.5
; PURPOSE:
;		Integration and display of linear detector data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		LD_ZINT, SL_0, ..., [,keywords]
; INPUTS:
;	SL_0, SL_1, SL_2, SL_3, SL_4, SL_5, SL_6, SL_7
;		List of linear detector scans, provided in any form that is acceptable
;		by LD_READ.  If more than one input is provided, LD_READ will
;		attempt patching in the horizontal direction.
;
;		Alternatively, SL_O may already contain linear detector data in the form
;		returned by LD_READ.  In such case it is used as is and any additional
;		inputs (if any) are ignored.
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
;	/ANGLES
;		Switch.  If set the data is read and displayed as a function of the
;		angles DTHETA (for horizontal) and BETA (for vertical), instead of Q_xy
;		and Q_z.
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
;		specified by keywords) through a call to LD_INT, and diplays the result
;		through a call to SCAN_SHOW.  Calls TOLER from MIDL.
; MODIFICATION HISTORY:
;		Created 20-FEB-2006 by Mati Meron.
;		Modified 20-JUN-2006 by Mati Meron.  Added keyword /BARE.
;-

	on_error, 1

	res = LD_int(sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, $
	xy_reg=xyr,z_reg=zr,/z_int,xy_ran=wxy,z_ran=wz,angle=ang,tit=gtit,_extra=_e)

	if keyword_set(ang) then begin
		xtit = 'Dth'
		otit = 'Beta'
	endif else begin
		xtit = 'Q!dxy!n'
		otit = 'Q!dz!n'
	endelse

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