Function Scan_PD_max, snum, fnum, total = tot, flist = wfnum, _extra = _e

;+
; NAME:
;		SCAN_PD_MAX
; VERSION:
;		7.09
; PURPOSE:
;		Returns the maximal or (optionally) total number of counts for selected
;		frame(s) in a PD scan.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_PD_MAX( SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
; KEYWORD PARAMETERS:
;	/TOTAL
;		Switch.  If set, the total number of counts in each frame is returned
;		instead of the (default) maximum.
;	FLIST
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.
; OUTPUTS:
;		Returns the maximal value(s) or (if /TOTAL is set) the total(s) of the
;		specified frames.  If a single frame is specified the result is a scalar
;		else it is a vector with same dimensionality as the number of frames.
; OPTIONAL OUTPUT PARAMETERS:
;	FLIST
;		Returns the list of frames for which the evaluation has been done.
;		Again, the result is a scalar for a single frame, else it is a vector.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data files must exist.
; PROCEDURE:
;		Straightforward data processing.  Calls SCAN_PD_FPREAD and
;		SPEC_FILE_CHECK.
; MODIFICATION HISTORY:
;		Created 10-MAY-2008 by Mati Meron.
;		Modified 20-MAY-2008 by Mati Meron.  Internal changes.
;		Modified 20-JUN-2008 by Mati Meron.  Internal changes.
;		Modified 25-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 10-NOV-2008 by Mati Meron.  Internal changes.
;-

	on_error, 1

	Spec_file_check, snum, /sing, /pildet, list = wsnum
	tfl = keyword_set(tot)
	dat = Scan_PD_fpread(snum,fnum,flist=wfnum,nframes=nfnum,_extra=_e)
	res = fltarr(nfnum)
	for i = 0l, nfnum-1 do $
	if tfl then res[i] = total(dat[i,*,*]) else res[i] = max(dat[i,*,*])
	if nfnum eq 1 then begin
		wfnum = wfnum[0]
		res = res[0]
	endif

	return, res
end