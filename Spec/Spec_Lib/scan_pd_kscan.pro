Function Scan_PD_Kscan, snum, fnum, verify = ver, $
	slit = sli, back_ang = bag,left = lef,right = rig, $
	center= cnt, locate= lct, free= fre, tolerance= tol, show=sho, rcenter=rct,$
	_extra= _e

;+
; NAME:
;		SCAN_PD_KSCAN
; VERSION:
;		8.475
; PURPOSE:
;		Extracts Y-scan (Kscan) data from some or all frames within a PD scan.
; CATEGORY:
;		SPEC PD data processing.
; CALLING SEQUENCE:
;		Result = SCAN_PD_KSCAN( SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
; KEYWORD PARAMETERS:
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
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
;		Switch,  If set, the center is located automatically.  If the center
;		such located is beyond the approved range around the nominal center (see
;		TOLERANCE), it is rejected and the nominal center is used instead.
;	/FREE
;		Switch.  If set and if LOCATE is set, SCAN_PD_KSCAN is free to use an
;		individual center for each frame, as located.  Default is to use the
;		mean center of the evaluated frames.
;		
;		Note:	In the "far detector" mode both LOCATE and FREE are set
;				automatically.
;	TOLERANCE
;		Numeric scalar, value of the acceptable shift of the reflectivity peak
;		from the nominal center, in either direction, in pixels, when /LOCATE is
;		used.  If not given, defaults to 3 pixels (about 0.5mm).
;	/SHOW
;		Switch.  If set, the resulting Y-scan and background are displayed
;		to the screen.
;	RCENTER
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Returns (nonnormalized) Y-scan values, optionally corrected for
;		background, in the standard 3-column 1D data format.
; OPTIONAL OUTPUT PARAMETERS:
;	RCENTER
;		Returns the location (in pixels) used for the detector center, as a
;		2-element vector.  If LOCATE with FREE are used, the returned center is
;		the average center for the scan.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Evaluates the Y-scan values as the sums of counts within the electronic
;		window defined by SLIT around the detector center, in each frame.
;		Optionally, if BACK_ANGLE is provided, background counts are evaluated
;		by summing counts within same size windows at locations offset
;		horizontally in both directions from the center by BACK_ANGLE, and
;		averaging.  Said background is then subtracted from the signal.
;		Calls SCAN_COLUMN, SCAN_LC, SCAN_PD_CENLOC, SCAN_PD_CENTER,
;		SCAN_PD_FRAMES, SCAN_PD_FPREAD, SCAN_PD_TOL, SCAN_SHOW and
;		SPEC_FILE_CHECK.  Calls ERREST from SURF_LIB.  Also calls DEFAULT,
;		FPU_FIX, HOW_MANY and ISNUM from MIDL.
; MODIFICATION HISTORY:
;		Created 25-OCT-2008 by Mati Meron as a modification of SCAN_PD_XR.
;		Modified 10-NOV-2008 by Mati Meron.  Added keywords LEFT, RIGHT and
;		FREE.  Modified peak location and streamlined operation.
;		Modified 5-FEB-2010 by Mati Meron.  Added keyword VERIFY.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 30-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 5-APR-2010 by Mati Meron.  Internal changes, APEX related.
;		Modified 15-FEB-2011 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2011 by Mati Meron.  Updated error calculations for APEX
;		Modified 25-AUG-2016 by Mati Meron.  Internal changes, Pilatus1M related
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum, /sing, /pildet, list = wsnum
	wfnum = Scan_PD_frames(wsnum,fnum,verify=ver,nframes=nfnum,_extra=_e)
	qy = (Scan_column(wsnum,'K'))[wfnum]
	s = sort(qy)
	wfnum = wfnum[s]
	qy = qy[s]
	sig = (rbg = (lbg = fltarr(3,nfnum)))
	sig[0,*] = (rbg[0,*] = (lbg[0,*] = qy))
	if n_elements(sli) eq 2 then hs = round(sli)/2 $
	else message, 'Slit needs two elements!'

	cur = fildat.scan[wsnum]
	apfl = (cur.pdstat - 3)/2
	if cur.pdfar then lct = (fre = 1)

	if Isnum(bag) then begin
		hoff = round([bag*!dtor*cur.pdist/cur.pdpix,0])
		if hoff[0] le 2*hs[0] then begin
			bagm = cur.pdpix*(2*hs[0]+1)/(!dtor*cur.pdist)
			message, 'Minimal offset for current slit is ' + $
			string(bagm,form='(f6.3)') + ' degrees!'
		endif
		bfl = 1
		dum = How_many(fir=lef,sec=rig,whi=whi)
		if dum then begin
			if whi then coef = [1.,-1.,0] else coef = [1.,0,-1.]
		endif else coef = [1.,-0.5,-0.5]
	endif else bfl = 0

	if keyword_set(lct) then begin
		wtol = Scan_PD_tol(tol,pdstat=cur.pdstat)
		wct = Scan_PD_cenloc($
		wsnum,wfnum,tol=wtol,ran=(wtol+1)/2,/mean,cent=cnt,_extra=_e)
		rct = total(reform(wct,2,nfnum),2)/nfnum
		if not keyword_set(fre) then $
		wct = transpose([[replicate(rct[0],nfnum)],[replicate(rct[1],nfnum)]])
	endif else begin
		rct = Default(cnt,Scan_PD_center(wsnum))
		if n_elements(rct) ne 2 then message, 'Center needs 2 elements!'
		wct = transpose([[replicate(rct[0],nfnum)],[replicate(rct[1],nfnum)]])
	endelse
	wct = round(wct)
	rct = round(rct)

	fdat = Scan_PD_fpread(wsnum,wfnum,/norm,nfac=nfc,_extra=_e)
	napfl = not apfl
	for i = 0l, nfnum-1 do begin
		ct = wct[*,i]
		dat = reform(fdat[i,*,*])
		pdat = dat[ct[0]-hs[0]:ct[0]+hs[0],ct[1]-hs[1]:ct[1]+hs[1]]
		sig[1,i] = total(pdat)
		if napfl then sig[2,i] = sqrt(nfc[i]*sig[1,i]) $
		else sig[2,i] = nfc[i]*Errest(pdat/nfc[i],/emod,/tot)
		if bfl then begin
			cl = ct - hoff
			pdat = dat[cl[0]-hs[0]:cl[0]+hs[0],cl[1]-hs[1]:cl[1]+hs[1]]
			lbg[1,i] = total(pdat)
			if napfl then lbg[2,i] = sqrt(nfc[i]*lbg[1,i]) $
			else lbg[2,i] = nfc[i]*Errest(pdat/nfc[i],/emod,/tot)
			cr = ct + hoff
			pdat = dat[cr[0]-hs[0]:cr[0]+hs[0],cr[1]-hs[1]:cr[1]+hs[1]]
			rbg[1,i] = total(pdat)
			if napfl then rbg[2,i] = sqrt(nfc[i]*rbg[1,i]) $
			else rbg[2,i] = nfc[i]*Errest(pdat/nfc[i],/emod,/tot)
		endif
	endfor

	if bfl then begin
		if nfnum eq 1 then begin
			sig = reform(sig,3,1)
			lbg = reform(lbg,3,1)
			rbg = reform(rbg,3,1)
		endif
		res = Scan_lc(sig,lbg,rbg,coef=coef)
	endif else res = sig

	if keyword_set(sho) then begin
		lcol=[!pcol.red,!pcol.green,!pcol.blue]
		dum = where(coef eq 0, ndum)
		if ndum gt 0 then lcol[dum] = !pcol.white
		Scan_show, sig, lbg, rbg, lcol=lcol, xtit='Q!dy!n', ytit='Counts',$
		tit = fildat.name + ' S# ' + string(snum,form='(i0)'), _extra = _e
	endif

	return, FPU_fix(res)
end