Function Scan_PD_frames_old, snum, fnum, verify= ver, nframes= nfr, complete= cmp, $
	_extra = _e

;+
; NAME:
;		SCAN_PD_FRAMES
; VERSION:
;		7.15
; PURPOSE:
;		Returns the expanded list of frame numbers for a given scan
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_PD_FRAMES( SNUM [, FNUM] [, NFRAMES = NFR])
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
;	NFRAMES
;		Optional output, see below.
;	COMPLETE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to RANGE_PROC.
;		This includes:
;			/SORT
;				Switch.  Sorts output in ascending order.
;			/UNIQUE
;				Switch.  Eliminates multiple occurences of frame numbers.
; OUTPUTS:
;		Returns an expanded list of frame numbers, based on the FNUM input.
; OPTIONAL OUTPUT PARAMETERS:
;	NFRAMES
;		Returns the number of frames in the output list.
;	COMPLETE
;		Returns 1 if the output contains all the frames in SNUM, counted once
;		(not necessarily in order), else returns 0.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data file must exist.
; PROCEDURE:
;		Straightforward, expands (if needed) then provided frame list and checks
;		against available frames using data present in the common block.  Calls
;		ARREQ, DEFAULT and RANGE_PROC, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-AUG-2008 by Mati Meron.
;		Modified 5-APR-2009 by Mati Meron.  Internal changes.
;		Modified 1-NOV-2009 by Mati Meron.  Added keywords COMPLETE and _EXTRA.
;		Modified 5-FEB-2010 by Mati meron.  Added keyword VERIFY.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	if n_elements(snum) eq 1 then begin
		nf = fildat.scan[snum].ncr[1]
		wfnum = Default(fnum,lindgen(nf))
		if Arreq(wfnum,-1) then wfnum= lindgen(nf) $
		else wfnum= Range_proc(wfnum,_extra=_e)
		fdum = where(wfnum ge 0 and wfnum lt nf,ndum)
		if ndum eq 0 then begin
			fran = strcompress(strjoin(string([0,nf-1]),'-'),/rem)
			message, 'No valid frames in range, frames '+ fran+ ' are present!'
		endif else wfnum = wfnum[fdum]
	endif else message, 'Single scan number required!'
	if keyword_set(ver) then wfnum = Scan_PD_ver(snum,wfnum,_extra=_e)
	nfr = n_elements(wfnum)
	cmp = Arreq(wfnum[sort(wfnum)],lindgen(nf))

	return, wfnum
end
