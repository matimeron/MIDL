Pro PD_Kscan, snum, slit = sli,back_ang = bag,left = lef,right = rig, $
	center = cnt, locate= lct, tolerance= tol, vnorm = vnm, fix = fix, $
	progress = prg, rcenter = rct, factors = mfac, result = res, _extra = _e

;+
; NAME:
;		PD_KSCAN
; VERSION:
;		8.475
; PURPOSE:
;		Patches and diplays Kscan data from PD scans.
; CATEGORY:
;		SPEC PD data processing.
; CALLING SEQUENCE:
;		Result = PD_KSCAN( SNUM [, keywords])
; INPUTS:
;	SNUM
;		Scan number or a list of scan numbers, in any form recognizable by
;		RANGE_PROC.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SLIT
;		A 2 element vector specifying electronic slit dimensions, in pixels, in
;		[horizontal,vertical] order.
;	BACK_ANG
;		Numeric scalar, the "sideways" offset angle used to evaluate background,
;		in degrees.  If not given, no background subtraction is performed.
;	/LEFT
;		Switch.  Specifies taking left sided background.
;	/RIGHT
;		Switch.  Specifies taking right sided background.
;
;		Note:	By default, double sided background is used.  Therefore, setting
;				both LEFT and RIGHT is same as not setting both.
;	CENTER
;		Location of the nominal detector center, in pixels.  Provided as a
;		2-element vector ([xy] order).  If not given defaults to the value from
;		the SPEC file (see SCAN_PD_CENTER).
;	/LOCATE
;		Switch,  If set, the center is refined automatically.  If the center
;		such located is beyond the approved range around the nominal center (see
;		TOLERANCE), it is rejected and the nominal center is used instead.
;	TOLERANCE
;		Numeric scalar, value of the acceptable shift of the calculated center
;		from the nominal one, in either direction, in pixels.  If not given,
;		defaults to 3 pixels (about 0.5mm).
;	VNORM
;		Scalar, the value to which the maximum of the result is to be normalized
;	/FIX
;		Temporary fix of rare patching problem.  Will be eliminated later.
;	/PROGRESS
;		Switch.  If set, the evaluation progress is printed to the screen.
;	RCENTER
;		Optional output, see below.
;	FACTORS
;		Optional output, see below.
;	RESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Standard output is graphics only, a plot of the diffuse data as a
;		function of Q_y.  Additional outputs are provided through the output
;		parameters.
; OPTIONAL OUTPUT PARAMETERS:
;	RCENTER
;		Returns the location (in pixels) used for the detector center, as a
;		2-element vector.
;	FACTOR
;		A vector of length N (the number of scans present) returning the
;		multiplicative factors of the matches.  The i-th value is the factor by
;		which scans #i is multiplied relative to scan #0.  The 0-th value is
;		always 1.
;	RESULT
;		Returns the patched data in the standard [3,*] form:
;
;		Column 0	:	QY
;		Column 1	:	Diffuse data.
;		Column 2	:	Data errors.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		The diffuse scattering values for each scan are evaluated by
;		SCAN_PD_KSCAN, then they are patched together using standard techniques.
;		Calls SCAN_COLUMN, SCAN_FIELD_READ, SCAN_JOIN, SCAN_ORDER, 
;		SCAN_PATCH_SHOW, SCAN_PD_CENLOC, SCAN_PD_CENTER, SCAN_PD_KSCAN,
;		SCAN_PD_TOL, SCAN_SCALE and SPEC_FILE_CHECK.  Also calls DEFAULT and
;		RANGE_COMP, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-OCT-2008 by Mati Meron, as a modification of PD_XR.
;		Modified 10-NOV-2008 by Mati Meron.  Added keywords LEFT and RIGHT.
;		Modified peak location and streamlined operation.
;		Modified 5-FEB-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 20-FEB_2011 by Mati Meron, to accomodate the far detector case.
;		Modified 25-AUG-2016 by Mati Meron.  Internal changes, Pilatus1M related
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum,/pildet,par_const=['Det_th'],nsc=nsc,lis=lis,_extra=_e
	sord = Scan_order(lis,col=[1,-1,-2])
	slis = lis[sord]
	mfac = replicate(1.,nsc)

	qym = (qys = fltarr(nsc))
	for i = 0l, nsc-1 do begin
		qy = Scan_column(slis[i],1)
		qyl = min(qy,max=qyh)
		qym[i] = (qyh + qyl)/2
		qys[i] = (qyh - qyl)/((n_elements(qy) - 1) > 1)
	endfor
	dum = min(abs(qym),cind)
	cnum = slis[cind]

	if max(Scan_field_read(lis,'PDfar')) eq 0 then begin
		if keyword_set(lct) then begin
			wtol = Scan_PD_tol(tol,pdstat=fildat.scan[cnum].pdstat)
			dct = Scan_PD_cenloc($
				cnum,tol=wtol,ran=(wtol+1)/2,/mean,cent=cnt,_extra=_e)
			rct = total(dct,2)/(size(dct))[2]
		endif else begin
			rct = Default(cnt,Scan_PD_center(cnum))
			if n_elements(rct) ne 2 then message, 'Center needs 2 elements!'
		endelse
		rct = round(rct)
	endif

	fxfl = keyword_set(fix)
	prfl = keyword_set(prg)
	for i = 0l, nsc-1 do begin
		if prfl then print, i, slis[i]
		next = Scan_PD_Kscan(slis[i],slit=sli,back=bag,left=lef,right=rig,$
			cen=rct,tol=tol,_extra=_e)
		if i gt 0 then begin
				if fxfl then rev = (i lt cind) else rev= (qys[i] lt qys[i-1])
				res = Scan_join(res,next,rev=rev,fact=fac,_extra=_e)
				mfac[i] = fac[1]
		endif else res = next
	endfor
	res = Scan_scale(res,Default(vnm,1.,/dtyp)/max(res[1,*]))

	Scan_patch_show, res, /ylog, xtit='Q!dy!n', ytit='Diffuse scattering', $
	tit = fildat.name + ' S# ' + Range_comp(slis), _extra= _e

	return
end