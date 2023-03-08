Function Scan_PD_cenloc, snum, fnum, mean = mea, range = rng, average = ave, $
	keep = kep, tolerance = tol, center = cnt, dth_scan = dth, $
	flist = wfnum, title = tit, shift = shf, _extra = _e

;+
; NAME:
;		SCAN_PD_CENLOC
; VERSION:
;		8.16
; PURPOSE:
;		Returns the location(s) of the center pixel within the selected frame(s)
;		in a PD scan.
; CATEGORY:
;		SPEC PD processing.
; CALLING SEQUENCE:
;		Result = SCAN_PD_CENLOC( SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		2 possible options:
;		1)	Single scan number.
;		2)	A complete preread image, either in the [M,N] or the [4,M,N] format.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
;		Relevant only to SNUM option (1).
; KEYWORD PARAMETERS:
;	/MEAN
;		Switch.  If set, the the calculation uses centroid location(s) instead
;		of peak location(s).
;	RANGE
;		Provides range around the peak location within which the centroid is
;		calculated.  Can be given as a scalar (in which case same range is used
;		for both dimensions) or a 2 element vector, containing ranges for the 
;		horizontal and vertical dimensions.  The centroid within each dimension
;		is calculated over [peak-range:peak+range].  RANGE is only meaningful if
;		MEAN is set.  If not provided, RANGE defaults to the full frame.
;	/AVERAGE
;		Switch.  If set, the average of the results for all frames processed is
;		returned.
;	/KEEP
;		Switch.  If set and AVERAGE is set, assures that the read data is kept 
;		for future use (see keyword PREV in SCAN_PD_FPREAD).  If AVERAGE is not
;		set, KEEP has no effect.  KEEP is intended for internal use.
;	TOLERANCE
;		Numeric scalar, range of search for a maximum, relative to the location 
;		of the nominal one, in any direction, in pixels.  Default is unlimited
;		range.
;	CENTER
;		Nominal location of the detector center, provided as a 2-element vector
;		in [x,y] order.  If not given, defaults to the values in the SPEC file.
;	/DTH_SCAN
;		Switch.  If set, a DTH scan (with ALPHA and BETA kept constant is
;		assumed and the angles are evaluated using SCAN_GET_ANGS instead of
;		SCAN_Q_ANGS which may be unstable close to the origin.
;	FLIST
;		Optional output, see below.
;	TITLE
;		Optional output, see below.
;	SHIFT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.
; OUTPUTS:
;		Returns the location(s) of the center pixel (i.e. the pixel which 
;		should receive the direct beam for the present values of Alpha, Beta and
;		Dth, for the specified frames.  If a single frame is specified the 
;		result is a 2-element vector else it is a [2,N] array where N is the 
;		number of frames, with each [2,i] couple being the center pixel 
;		coordinates for the corresponding frame.
; OPTIONAL OUTPUT PARAMETERS:
;	FLIST
;		Returns the list of frames for which the evaluation has been done.
;		The result is a scalar for a single frame, else it is a vector.
;		Note:	In case of SNUM option 2 FLIST returns -1.
;	TITLE
;		Returns the Pilatus data filename to be used as a plot title, for the
;		use of calling routines.
;		Note:	In case of SNUM option 2 TITLE returns a null string.
;	SHIFT
;		Returns the difference between the final result and the calculated
;		result, for debugging purposes.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data files must exist.
; PROCEDURE:
;		Straightforward data processing.  Calls SCAN_GET_ANGS, SCAN_PD_CENTER,
;		SCAN_PD_FAROFF, SCAN_PD_FRAMES, SCAN_PD_PEAK, SCAN_PD_READ, SCAN_Q_ANGS
;		and SPEC_FILE_CHECK.  Also calls DEFAULT, FPU_FIX and ISNUM from MIDL.
; MODIFICATION HISTORY:
;		Created 10-NOV-2008 by Mati Meron.
;		Modified 5-FEB-2010 by Mati Meron.  Internal changes.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 5-APR-2010 by Mati Meron.  Significant internal changes, 
;		especially change of the meaning of TOLERANCE.  Added the option of 
;		processing preread data.
;		Modified 5-FEB-2011 by Mati Meron.  Internal changes.  Added far 
;		detector support.  Added keyword SHIFT.
;		Modified 15-FEB-2011 by Mati Meron.  More internal changes.
;		Modified 25-DEC-2011 by Mati Meron.  Added keyword KEEP.
;		Modified 15-AUG-2012 by Mati Meron.  Added keyword DTH_SCAN.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	afl = keyword_set(ave)
	tfl = Isnum(tol)
	vafl = 0
	if n_elements(snum) eq 1 then begin
		Spec_file_check, snum, /sing, /pildet, list = wsnum
		cur = fildat.scan[wsnum]
		wfnum = Scan_PD_frames(wsnum,fnum,nframes=nfnum,_extra=_e)
		if not keyword_set(dth) then begin
			wang = Scan_q_angs(wsnum,wfnum,/rad,/xrev)
			cwang = Scan_q_angs(wsnum,wfnum,/rad,/xrev,/con,csta=cst)
		endif else wang = Scan_get_angs(wsnum,wfnum,/rad,/full,cflag=cst)
		if not min(cst) then begin
			if afl then message, 'Warning: Variable angles/positions', /con $
			else vafl = 1
		endif
		if not cur.pdfar then begin
			off = fltarr(2,nfnum)
			dist = cur.pdist + (cur.g_l)[3]*(1/cos(wang[1,*]) - 1)
			fac = $
			dist/(cur.pdpix*(cos(wang[2,*]) + tan(wang[0,*])*tan(wang[1,*])))
			off[0,*] = fac*sin(wang[2,*])/cos(wang[1,*])
			off[1,*] = fac*(tan(wang[0,*]) - cos(wang[2,*])*tan(wang[1,*]))
		endif else off = Scan_PD_faroff(wsnum,wfnum,/ref)
		if tfl then begin
			ovec = replicate(1,nfnum)
			case n_elements(cnt) of
				0	:	cen = Scan_PD_center(wsnum)#ovec - off
				2	:	cen = cnt#ovec
				else:	message, 'Center needs 2 elements!'
			endcase
		endif
		if afl then begin
			if keyword_set(kep) then wsnum = $
			total(Scan_PD_fpread(wsnum,wfnum,/norm,title=tit,_extra=_e),1) $
			else wsnum=Scan_PD_read(wsnum,wfnum,/jimg,/norm,title=tit,_extra=_e)
		endif
	endif else begin
		wsnum = snum
		off = fltarr(2)
		nfnum = 1
		wfnum = -1
		if tfl and Isnum(cnt) and n_elements(cnt) eq 2 then cen = cnt $
		else tfl = 0
	endelse

	if tfl then begin
		if vafl then reg = round([cen-tol,cen+tol]) $
		else reg = round([cen[*,0] - tol, cen[*,0] + tol])
		res = Scan_PD_peak(wsnum,wfnum, $
			reg=reg,mean=mea,range=rng,title=tit,_extra=_e)
	endif else res = Scan_PD_peak(wsnum,wfnum, $
	mean=mea,range=rng,title=tit,_extra=_e) - off
	res = round(res)
	shf = res - Default(cen,0l,/dtype)

	return, FPU_fix(res)
end