Function LD_patch_gen, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, $
	orientation = ori, chan_range = chr, bin = bin, force = frc, $
	nscan = nsc, order = sord, slist = slis, factors = mfac, _extra = _e

;+
; NAME:
;		LD_PATCH_GEN
; VERSION:
;		7.15
; PURPOSE:
;		Patches together LD scans, for horizontal or vertical LD direction.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		LD_PATCH_GEN, S_0 [, S_1, ....] [,keywords]
; INPUTS:
;	S_0, S_1, ....
;		List(s) of linear detector scans, provided in any form that is
;		acceptable (individually) by LD_READ.  The lists will be combined into
;		a single list, internally
;
;		Note:	The ordering of S_0, ... S_N is arbitrary.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ORIENTATION
;		Character string input specifying LD orientation.  Two possible inputs,
;		"horizontal" and "vertical" (first letter suffices). Alternatively can
;		be provided as numerical input, with 0 signifying horizontal and 1,
;		vertical.
;	CHAN_RANGE
;		The range of channels to use in the readout.  Default is full range
;		present in the data.
;	BIN
;		Specifies the bin size to be applied to each scan.  Default is 1, i.e.
;		no binning.  Any size provided is always rounded downward to the nearest
;		power of 2 (but the resulting binsize is never smaller than 1).
;		Note:	When both CHAN_RANGE and BIN are present, the CHAN_RANGE values
;				are taken as pre-binned values.
;	/FORCE
;		Switch.  If set, forces junction even when there is no overlap between
;		some scans.  This involves extrapolation which greatly increases
;		uncertainties, so it should be used with care.
;	NSCAN
;		Optional output, see below.
;	ORDER
;		Optional output, see below.
;	SLIST
;		Optional output, see below.
;	FACTORS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  Not to be used directly.
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
;		Reads the scans, orders then (by the value of DTH for horizontal
;		orientation or of BETA for vertical), and joins them together.  Calls
;		SCAN_ANG_ORDER, SCAN_JOIN, SCAN_LD_SUM, SCAN_PAR_READ and
;		SPEC_FILE_INFO, from SPEC.  Calls ISNUM, FPU_FIX and WHERINSTRUCT
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 25-OCT-2007 by Mati Meron.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1
	checkv = ['Out_rot','Det_th']
	checknam = ['Beta','Dth']

	if Isnum(ori) then wori = long(ori) $
	else wori = Strmatch_mm(ori,['hor','ver'],1,/nosub)

	Spec_file_info, _extra = _e
	dum = (Wherinstruct('new',_e))[0]
	if dum ge 0 then _e.(dum) = 0
	sord = Scan_ang_order(s_0,s_1,s_2,s_3,s_4,s_5,s_6,s_7,$
			ord=checknam[1-wori],list=slis,_extra=_e)
	nsc = n_elements(sord)
	slis = slis[sord]
	check = Scan_par_read(slis,checkv[wori],/const,tol=1e-3,cfl=cfl)
	if not cfl then message, 'Non-constant ' + checknam[wori] + '!'

	mfac = replicate(1.,nsc)
	for i = 0l, nsc-1 do begin
		next = Scan_LD_sum(slis[i],ori=ori,chan_range=chr,bin=bin,_extra=_e)
		if i gt 0 then begin
			res = Scan_join(res,next,force=frc,fact=fac,_extra=_e)
			mfac[i] = fac[1]
		endif else res = next
	endfor

	return, FPU_fix(res)
end