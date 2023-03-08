Function Scan_sort, scan, column = col, descending = dsc, sord = sor

;+
; NAME:
;		SCAN_SORT
; VERSION:
;		4.9
; PURPOSE:
;		Sorts a scan.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_SORT( SCAN [, keywords])
; INPUTS:
;	SCAN
;		Scan data, a [2,n] or [3,n] array.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COLUMN
;		The # of the column to sort on.  Default is 0.
;	/DESCENDING
;		Switch.  If set, sorting is in descending order.  Default is ascending.
;	SORD
;		Optional output, see below.
; OUTPUTS:
;		Returns the original data, with all the columns sorted according to the
;		ascending (or descending) order of the sort column.
; OPTIONAL OUTPUT PARAMETERS:
;	SORD
;		Returns the sort order, i.e. the direct output of the sort function, as
;		used internally, in case it needs to be applied elsewhere.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls DEFAULT from MIDL.
; MODIFICATION HISTORY:
;		Created 10-SEP-2002 by Mati Meron.
;		Modified 25-FEB-2004 by Mati Meron.  Added keyword SORD.
;-

	on_error, 1

	siz = size(scan)
	if siz[0] eq 2 then begin
		res = scan
		sor = sort(res[Default(col,0,/dtyp),*])
		if keyword_set(dsc) then sor = reverse(sor)
		for i = 0, siz[1]-1 do res[i,*] = res[i,sor]
	endif else message, 'Not a scan!'

	return, res
end