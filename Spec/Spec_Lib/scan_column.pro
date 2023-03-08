Function Scan_column, snum, col, constant = con, toler = tol, average = ave, $
	cname = cnm, status = stat, _extra = _e

;+
; NAME:
;		SCAN_COLUMN
; VERSION:
;		7.15
; PURPOSE:
;		Reads a single data column from a SPEC data file.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_COLUMN( SNUM, COL [, keywords ])
; INPUTS:
;	SNUM
;		Positive integer, scan #, mandatory.
;	COL
;		A numeric or character scalar.  If numeric, is taken as the column
;		number.  If character, it is taken as the column title.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/CONSTANT
;		Switch.  If set, constancy of the column's elements is required.
;	TOLER
;		Tolerance specification, sets the maximal allowed difference for values
;		to be considered equal for the purpose of /CONSTANT.  If provided,
;		overrides the internal setting which is twice the machine precision
;		multiplied by inputs' values.  If /CONSTANT is not set, TOLER has no
;		effect.
;	/AVERAGE
;		Switch.  If set, the average value of the column is returned.
;	CNAME
;		Optional output, see below.
;	STATUS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to SPEC_FILE_INFO. All
;		SPEC_FILE_INFO keywords are accepted.  Not to be used directly.
; OUTPUTS:
;		In case of success returns the data as a numeric vector, or scalar if
;		/CONSTANT is set.  Else, returns 0b.
; OPTIONAL OUTPUT PARAMETERS:
; 	CNAME
; 		If STATUS (see below) is 1, CNAME returns the name of the variable in
; 		the selected column, else returns null string.
;	STATUS
;		Returns, the read status, 1 for success, 0 for failure.  Success is
;		defined as:
;			1)	The scan designated by SNUM exists and contains valid data.
;			2)	The column designated by SCOL exists.
;
;		In addition, if /CONSTANT is set, then the following is also required:
;			3)	The constancy condition is fulfilled.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Uses SCAN_GET to obtain the scan data, then selects the required column
;		corresponding to the provided column number or title.  Calls APPROX,
;		STRMATCH_MM and TYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 20-DEC-2005 by Mati Meron.
;		Modified 30-MAR-2006 by Mati Meron.  Added keyword AVERAGE.
;		Modified 10-NOV-2009 by Mati Meron.  Added keyword CNAME.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	if n_elements(col) eq 1 then begin
		scan = Scan_get(snum, header = head, status = stat,_extra = _e)
		if stat then begin
			nh = n_elements(head)
			if Type(col) eq 7 then begin
				cloc = Strmatch_mm(col,head)
				if cloc lt 0 then cloc = nh
			endif else cloc = col
			if cloc ge (-nh) and cloc lt nh then begin
				cind = (cloc+nh) mod nh
				res = scan[cind,*]
				cnm = head[cind]
			endif else begin
				res = 0b
				cnm = ''
				stat= 0
			endelse
		endif
	endif else message, 'Only single column allowed!', /con

	if stat then begin
		res = reform(res)
		if keyword_set(con) then begin
			lo = min(res,max=hi)
			if Approx(lo,hi,thre=tol) then res = (lo + hi)/2. else stat = 0
		endif
		if keyword_set(ave) then res = total(res)/n_elements(res)
	endif

	return, res
end