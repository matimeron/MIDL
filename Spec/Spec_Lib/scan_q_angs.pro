Function Scan_q_angs, snum, fnum, constant = con, toler = tol, warn = wrn, $
	status = sta, astatus = ast, cstatus = cst, _extra = _e

;+
; NAME:
;		SCAN_Q_ANGS
; VERSION:
;		8.05
; PURPOSE:
;		Converts the H, K, L values of a scan in SPEC data file to angles.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_Q_ANGS( SNUM [ ,FNUM ] [, keywords ])
; INPUTS:
;	SNUM
;		Positive integer, scan #, mandatory.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
; KEYWORD PARAMETERS:
;	/CONSTANT
;		Switch.  If set, constancy of the angles through the scan is required.
;	TOLER
;		Tolerance specification, sets the maximal allowed difference for values
;		to be considered equal for the purpose of /CONSTANT.  If provided,
;		overrides the internal setting which is 5e-4 (1e-5 if /RADIANS is set).
;		If /CONSTANT is not set, TOLER has no effect.
;	/WARN
;		Switch, if set and /CONSTANT is set, a warning is issued for any 
;		nonconstant angle within the scan.
;		If /CONSTANT is not set, /WARN has no effect.
;	STATUS
;		Optional output, see below.
;	ASTATUS
;		Optional output, see below.
;	CSTATUS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
;
;		Note:	Some of the keywords that can be passed are:
;				/RADIANS -  Results in the output being in radian units.
;				/XREVERSE - Reverses the X-axis direction.
; OUTPUTS:
;		In case of success returns the data as a [3,N] numeric array, where N
;		is the number of frames specified by FNUM (all if FNUM not given).  The
;		consecutive colums of the output contain the angles ALPHA, BETA and DTH.
;		If /CONSTANT is set, the routine returns a 3-element vector with the 
;		angles ALPHA, BETA and DTH.  These values correspond to the first frame
;		processed (even if the constancy test fails).  The output is in degrees
;		(unless /RADIANS is set).  In case of failure (STAT = 0) returns 0b.
;
;		Note:	In the case of "far detector" all the angles other than ALPHA
;				are returned as 0.
; OPTIONAL OUTPUT PARAMETERS:
;	STATUS
;		Returns, the read status, 1 for success, 0 for failure.  Success is
;		defined as:
;			1)	The scan designated by SNUM exists and contains valid data.
;			2)	The columns H, K, L exist.
;	ASTATUS
;		Returns the status codes of the Q_TO_ANG routine, consisting of 1 for
;		each point where evaluation has been performed and zero for points 
;		where evaluation is no possible (all Q values are 0).
;	CSTATUS
;		Vector of length 3, returns the constancy status for the angles.
;		0 signifies not constant, 1 signifies constant.  If /CONSTANT is not set
;		CSTATUS returns al 1s but carries no meaning.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward calculation of the angles from the Q-values.  Calls
;		SCAN_GET, SCAN_PD_FRAMES and SPEC_FILE_CHECK.  Calls Q_TO_ANG.  Also 
;		calls APPROX, DEFAULT, STRMATCH_MM and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-APR-2009 by Mati Meron.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 5-FEB-2011 by Mati Meron.  Far detector support.
;		Modified 10-JUN-2011 by Mati Meron.  Added keywords WARN and CSTATUS.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum, /sing, list = wsnum
	wfnum = Scan_PD_frames(wsnum,fnum,nfr=nfnum)
	cur = fildat.scan[wsnum]
	scan = Scan_get(wsnum, header = head, status = sta, _extra = _e)
	if sta then begin
		hind = Strmatch_mm('h',head)
		kind = Strmatch_mm('k',head)
		lind = Strmatch_mm('l',head)
		if hind ge 0 and kind ge 0 and lind ge 0 then $
		res = Q_to_ang(scan[hind,wfnum],scan[kind,wfnum],scan[lind,wfnum],$
		lam=cur.lambda,sta=ast,_extra=_e) else sta = 0
	endif
	if not sta then res = 0b else if cur.pdfar then res[1:2,*] = 0

	cst = [1,1,1]
	if sta and keyword_set(con) then begin
		unam = ['Alpha','Beta','Dth']
		tres = res
		res = res[*,0]
		rcheck = (Wherinstruct('rad',_e))[0]
		if rcheck then dtol = 1e-5 else dtol = 5e-4
		wtol = Default(tol,dtol)
		wfl = keyword_set(wrn)
		for i = 0, 2 do begin
			lo = min(tres[i,*],max=hi)
			if not Approx(lo,hi,thre=wtol) then begin
				cst[i] = 0
				if wfl then message, unam[i] + ' is not constant in scan # ' + $
				string(wsnum,form='(i0,"!")'), /con 
			endif
		endfor
	endif

	return, res
end