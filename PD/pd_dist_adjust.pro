Pro PD_dist_adjust, snum, correction= cor, value= val, dist=dst, s4dist=s4d, $
	restore= rst, _extra = _e

;+
; NAME:
;		PD_DIST_ADJUST
; VERSION:
;		8.23
; PURPOSE:
;		Modifies the distances PDIST and/or PDSDIST in the FILDAT structure for
;		SPEC files (see SPSCAN__DEFINE for details).
; CATEGORY:
;		SPEC Data processing.
; CALLING SEQUENCE:
;		PD_DIST_ADJUST, SNUM [, keywords ]
; INPUTS:
;	SNUM
;		Scan number or a list of scan numbers, in any form acceptable by
;		SCAN_LIST_VER.  A value of 0 or no value at all translates to "all the
;		scans in the file".
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	CORRECTION													 |	One and only
; 		Scalar or 2-element vector, the additive correction(s)	 |	one of these
; 		to be applied toPDIST and/or PDSDIST.  If both distances |	two must be
; 		are to be corrected, a 2-element input is required.		 |	used. "None"
; 	VALUE														 |	is allowed
; 		Scalar or 2-element vector, the values to be used to	 |	when RESTORE
; 		replace current PDIST and/or PDSDIST.  If both distances |	is set.
; 		are to be changed, a 2-element input is required.		 |
;	/DIST
;		Switch.  If set, specifies that PDIST is to be modified.
;	/S4DIST
;		Switch.  If set, specifies that PDSDIST is to be modified.
;
;		Note:	Not setting any of /DIST and /S4DIST is equivalent to setting
;		both.
;	/RESTORE
;		Switch.  If set, all the PDIST and PDSDIST values are restored to their
;		original values.
;	_EXTRA
;		A formal keyword used to pass keywords to SPEC_FILE_INFO. All
;		SPEC_FILE_INFO keywords are accepted.  Not to be used directly.
; OUTPUTS:
;		None, other than changes in the FILDAT values.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See SPEC_FILE_INFO for details.
;		DIST_ADJUST_KEEP.  Includes:
;			SAV		-	Set to 1 once the common block is initialized.
;			NAME	-	Name of the current SPEC file.
;			NSCAN	-	Number of scans in the file.
;			FLIST	-	List of all the valid Pilatus scans in the file.
;			PDIST	-	Original PDIST values for all the scans in FLIST.
;			PDSDIST	-	Original PDSDIST values for all the scans in FLIST.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls SPEC_FILE_INFO.  Calls ARREQ, DEFAULT, HOW_MANY,
;		INTERSECT_MM, ONE_OF, RANGE_PROC and STREQ, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-DEC-2013 by Mati Meron as SPEC_DIST_ADJUST.
;		Renamed PD_DIST_ADJUST on Feb-20-2014, by Mati Meron.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	common dist_adjust_keep, sav, name, nscan, flist, pdist, pdsdist
	on_error, 1

	Spec_file_info, _extra = _e
	if not fildat.status then message, 'No file or no data in file!'

	if Streq(fildat.name,name) then begin
		if fildat.nscan gt Default(nscan,0) then begin
			rst = 1
			sfl = 1
		endif else sfl = 0
	endif else sav = 0

	if keyword_set(rst) then begin
		if sav then begin
			fildat.scan[1:nscan].pdist = pdist
			fildat.scan[1:nscan].pdsdist = pdsdist
			if sfl then sav = 0
		endif else message, "Can't restore!"
	endif
	
	if sav eq 0 then begin
		name = fildat.name
		nscan = fildat.nscan
		flist = lindgen(nscan) + 1
		flist = flist[where(fildat.scan[flist].pdstat gt 1, nlist)]
		if nlist gt 0 then begin
			pdist = fildat.scan[1:nscan].pdist
			pdsdist = fildat.scan[1:nscan].pdsdist
			sav = 1
		endif else message, 'No Pilatus data in file!'
	endif

	wha = One_of(cor,val,val=use)
	if wha ge 0 then begin
		wsnum = Range_proc(Default(snum,0,/dtyp))
		if Arreq(wsnum,[0]) then wsnum = flist $
		else wsnum = Intersect_mm(wsnum,flist)
		dum = How_many(fir=dst,sec=s4d,whi=whi)
		if whi eq 0 then whi = 3
		if n_elements(use) eq 1 then begin
			case whi of
				1	:	
				2	:	use = [0,use]
				3	:	message, 'To change both PDIST and PDSDIST,' + $
						' 2 corrections or values are needed!'
			endcase
		endif

		if whi then if wha then fildat.scan[wsnum].pdist = use[0] $
			else fildat.scan[wsnum].pdist = fildat.scan[wsnum].pdist + use[0]
		if whi/2 then if wha then fildat.scan[wsnum].pdsdist = use[1] $
			else fildat.scan[wsnum].pdsdist = fildat.scan[wsnum].pdsdist+ use[1]
	endif else if not rst then message, 'Either CORRECT of VALUE must be used!'

	return
end