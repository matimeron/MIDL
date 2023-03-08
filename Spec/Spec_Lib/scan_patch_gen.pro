Function Scan_patch_gen, snum, columns = col, force = frc, tau = tau, $
	nscan = nsc, order = sord, slist = slis, factors = mfac, _extra= _e

;+
; NAME:
;		SCAN_PATCH_GEN
; VERSION:
;		7.15
; PURPOSE:
;		Generic data patching routine.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = SCAN_PATCH_GEN( SNUM, COLUMNS = COL [,keywords])
; INPUTS:
;	SNUM
;		Scan list can be of any list form (i.e. scan numbers, not actual scans)
;		acceptable by SCAN_LIST_VER (see there).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COLUMNS
;		An integer vector with 2 or 3 entries, specifying which of the data
;		columns present in the scan should be used.  The first two entries are
;		the numbers of the X and Y columns (either counting from 0 up, from
;		left, or from -1 down, from right).  If a third entry is provided, it
;		is used as the number of a normalization column.
;
;		Mandatory input, no defaults provided.
;	/FORCE
;		Switch.  If set, forces junction even when there is no overlap between
;		some scans.  This involves extrapolation which greatly increases
;		uncertainties, so it should be used with care.
;	TAU
;		The time constant for the calculation of pileup correction.  If zero or
;		not given, no correction is performed.
;	NSCAN
;		Optional output, see below.
;	ORDER
;		Optional output, see below.
;	SLIST
;		Optional output, see below.
;	FACTORS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to
;		SPEC_FILE_INFO.  Not to be used directly.
; OUTPUTS:
;		Returns the patched data in the standard [3,*] form.
; OPTIONAL OUTPUT PARAMETERS:
;	NSCAN
;		Returns the number of scans present.
;	ORDER
;		Returns the order of the scans, in SNUM.
;	SLIST
;		Returns the list of scans in SNUM, ordered by ORDER.
;	FACTOR
;		A vector of length N (the number of scans present) returning the
;		multiplicative factors of the matches.  The i-th value is the factor by
;		which scan #i is multiplied relative to scan #0.  The 0-th value is
;		always 1.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The scans on the list must exist and, unless /FORCE is set, each scan
;		should have at least one-point overlap with some other scan.
; PROCEDURE:
;		Reads the scans, orders them (by the minimal value of the X column) and
;		joins them together.  Calls SCAN_JOIN, SCAN_LIST_VER, SCAN_ORDER and
;		SPEC_FILE_INFO, from SPEC.  Also calls ARREQ and WHERINSTRUCT from MIDL.
; MODIFICATION HISTORY:
;		Created 15-NOV-2003 by Mati Meron.
;		Modified 1-MAR-2004 by Mati Meron.  Added optional pileup correction.
;		Renamed SCAN_PATCH_GEN (from the previous PATCH_GEN) 25-OCT-2007, by
;		Mati Meron.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	if n_elements(col) eq 0 then message, 'Missing column numbers!'

	nsc = Scan_list_ver(snum,list=slis,flag=sfl)
	if nsc gt 0 and sfl then begin
		Spec_file_info, _extra = _e
		dum = (Wherinstruct('new',_e))[0]
		if dum ge 0 then _e.(dum) = 0
		check = Arreq([slis gt 0 and slis le fildat.nscan],replicate(1b,nsc))
		if fildat.status and check then begin
			sord = Scan_order(slis,col=col)
			slis = slis(sord)
			res = Scan_join(slis,col=col,force=frc,fact=mfac,tau=tau,_extra=_e)
		endif else message, 'No such file or scan(s)!'
	endif else message, 'Patch what?!'

	return, res
end