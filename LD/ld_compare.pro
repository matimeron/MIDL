Pro LD_compare, ld_0, ld_1, ld_2, ld_3, ld_4, ld_5, ld_6, ld_7, $
	xy_reg = xyr, z_reg = zr, log = log, angles = ang, title = tit, _extra = _e

;+
; NAME:
;		LD_COMPARE
; VERSION:
;		7.15
; PURPOSE:
;		Comparison of LD 2D scans
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		LD_COMPARE, SL_0, ..., [,keywords]
; INPUTS:
;	SL_0, SL_1, SL_2, SL_3, SL_4, SL_5, SL_6, SL_7
;		List of linear detector scans, provided in any form that is acceptable
;		by LD_READ.  If more than one input is provided, LD_READ will
;		attempt patching in the horizontal direction.
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
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  Not to be used directly.
; OUTPUTS:
;		None other than the graphic output.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, rebinning.  Calls LD_INT.  Also calls SCAN_SHOW and
;		SPEC_FILE_INFO from SPEC and WHERINSTRUCT from MIDL.
; MODIFICATION HISTORY:
;		Created 20-FEB-2006 by Mati Meron.
;		Modified 5-APR-2006 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1
	ldnams =  strcompress('ld_' + sindgen(8),/remove)
	rnams =  strcompress('r_' + sindgen(8),/remove)

	n = n_params()
	if n gt 0 then begin
		Spec_file_info, _extra = _e
		dum = (Wherinstruct('new',_e))[0]
		if dum ge 0 then _e.(dum) = 0
		gentit = ''
		tfl = (size(ld_0))[0] ne 3
		sarr = strarr(n)
		si = sindgen(n)
		pcom = ',/z_int,xy_ran=wxy,z_ran=wz'
		if n_elements(xyr) gt 0 then $
		pcom = pcom + ',xy_reg = [' + strjoin(string(xyr),',') + ']'
		if n_elements(zr) gt 0 then $
		pcom = pcom + ',z_reg = [' + strjoin(string(zr),',') + ']'
		pcom = strcompress(pcom + ',angle=ang,_extra=_e)',/rem)
		for i = 0, n-1 do begin
			idum = execute(rnams[i] + '= LD_int(' + ldnams[i] + pcom)
			if tfl then jdum = execute('sarr[' + si[i] + ']=' + ldnams[i])
		endfor

		if tfl then gentit=fildat.name+'!c'+strcompress(strjoin(' # '+sarr,','))
		if keyword_set(ang) then begin
			xtit = 'Dth'
			otit = 'Beta'
		endif else begin
			xtit = 'Q!dxy!n'
			otit = 'Q!dz!n'
		endelse

		swxy = strcompress('[' + strjoin(string(wxy,form='(f7.3)'),',') + ']')
		swz  = strcompress('[' + strjoin(string(wz, form='(f7.3)'),',') + ']')
		subtit = strjoin([xtit+'_range = '+swxy, otit+'_range = '+ swz],'  ;  ')

		Scan_show, r_0, r_1, r_2, r_3, r_4, r_5, r_6, r_7, ylog = log, /count,$
		tit= Default(tit,gentit), subtit = subtit, xtit = xtit, ytit = ctit, $
		ymargin = 2*[3,1+tfl], _extra = _e
	endif else message, 'No data!'

	return
end
