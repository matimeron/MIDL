Function Scan_PD_faroff, snum, fnum, check = chk, alpha = alp, radians = rad, $
	reflect = ref, slit = sli, average = ave, _extra = _e

;+
; NAME:
;		SCAN_PD_FAROFF
; VERSION:
;		8.07
; PURPOSE:
;		Returns the correction for center pixel location of area detector 
;		frames, for the case of stationary detector.
; CATEGORY:
;		SPEC data analysis.
; CALLING SEQUENCE:
;		Result = SCAN_PD_FAROFF( SNUM [,FNUM] [,keywords])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
; KEYWORD PARAMETERS:
; 	/CHECK
; 		Switch.  If set, the routine verifies that SNUM is indeed a single 
; 		valid scan.
;	ALPHA
;		Input of Alpha angles value(s) for SNUM, in degrees unless /RADIANS (see
;		below) is set.  If given, must be either a scalar (in which case the 
;		value applies to all frames, or a vector of same length as the number 
;		of frames.  By default Alpha values are read from the scan data.
;	/RADIANS
;		Switch.  If set, the ALPHA values are taken to be in radians.
;	/REFLECT
;		Switch.  If set, the correction is calculated for reflectivity geometry,
;		i.e Beta = Alpha, Dth = 0.  The default calculation is for
;		Beta = Dth = 0.
;	/SLIT
;		Switch.  If set, the correction is performed for the slit location, 
;		instead of the detector location.
;	/AVERAGE
;		Switch.  If set, the average of the results for all frames processed is
;		returned.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.
; OUTPUTS:
;		Returns the correction(s) to the location(s) of the center pixel 
;		(as given by SCAN_PD_CENTER) for the case where the detector is 
;		stationary(i.e. not tracking the beam).   If a single frame is 
;		specified the result is a 2-element vector else it is a [2,N] array 
;		where N is the number of frames, with each [2,i] couple being the 
;		correction for the center pixel coordinates for the corresponding frame.
;		The correction is negative, thus the corrected center is given by
;		
;		Cent = SCAN_PD_CENTER(SNUM) - SCAN_PD_FAROFF(SNUM)
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data files must exist.
; PROCEDURE:
;		Straightforward data processing.  Calls SCAN_PAR_READ, SCAN_PD_FRAMES,
;		SCAN_Q_ANGS and SPEC_FILE_CHECK.
; MODIFICATION HISTORY:
;		Created 15-FEB-2011 by Mati Meron.
;		Modified 15-JUN-2011 by Mati Meron.  Internal changes.
;		Modified 20-JUL-2011 by Mati Meron.  Added keywords RADIANS and SLIT.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	if keyword_set(chk) then Spec_file_check, snum,/sing,/pildet,list=wsnum $
	else wsnum = snum
	wfnum = Scan_PD_frames(wsnum,fnum,nframes=nfnum,_extra=_e)

	cur = fildat.scan[wsnum]
	if cur.pdfar then begin
		dist = cur.pdist
		if keyword_set(sli) then dist = dist - cur.pdsdist
		if keyword_set(rad) then mult = 1 else mult = !dtor
		case n_elements(alp) of
			0	:	walp = reform((Scan_q_angs(wsnum,wfnum,/rad,/xrev))[0,*])
			1	:	walp = replicate(mult*alp,nfnum)
			nfnum:	walp = mult*alp
			else:	message, 'Bad Alpha input!'
		endcase 
		tphi = 2*!dtor*(Scan_par_read(snum,'phi'))[0]
		htan = cos(tphi)*sin(walp)^2/ $
		(sin(tphi)*cos(walp)^2 + sqrt(sin(tphi)^2 - sin(walp)^2))
		res = transpose([[((cur.g_l)[2] + dist)*htan],$
		[((cur.g_l)[2] - keyword_set(ref)*dist)*tan(walp)]])
	endif else res = fltarr(2,nfnum)
	if keyword_set(ave) and nfnum gt 1 then res = total(res,2)/nfnum

	return, res/cur.pdpix
end