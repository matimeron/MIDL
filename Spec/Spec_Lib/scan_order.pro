Function Scan_order, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, $
	s_8, s_9, s_10, s_11, s_12, s_13, s_14, s_15, order_by = orb, _extra = _e

;+
; NAME:
;		SCAN_ORDER
; VERSION:
;		7.1
; PURPOSE:
;		Orders scans.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_ORDER(S_0, ...  [, keywords])
; INPUTS:
;	S_0, S_1, .. S_15
;		May be:
;			1)  Scans, i.e. [3,n] arrays.
;			2)	Scan numbers, i.e. integers
;			3)  A single array of scan numbers.
;			4)  A single character scalar or array to be processed by RANGE_PROC
;				from MIDL.  See there for allowed expressions.
;
;		In the first 2 cases the  maximal number of scans is 16.  In the third
;		and last case there is no limit.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ORDER_BY
;		Determines the sorting parameter.  The 2 options currently defined are:
;
;			1)	MIN	-	Sorts scans in order of minimum value of the X-range
;						followed by, in case ambiguity remains, by a sort on
;						the maximum value of the X-range.
;			2)	MAX	-	Reverses the procedure above, maximum first, minimum,
;						if needed, second.
;		The default option is the first one, MIN.
;
;	_EXTRA
;		A formal key, used to transfer additional keywords to SCAN_READ.  Only
;		active when a list is used.
; OUTPUTS:
;		Returns a long integer array specifying the order of the scans, same as
;		sort.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		1)	In case a scan list is provided, the scan numbers in it must
;			correspond to valid scan numbers in a currently open spec file.
;		2)	If a scan number list is provided, SCAN_READ requires COLUMNS input.
;			See there for details.
; PROCEDURE:
;		Straightforward.  Calls SCAN_LIST_VER and SCAN_READ (only when in the
;		list mode).  Calls LEXISORT, STRMATCH_MM and WHERINSTRUCT from MIDL.
; MODIFICATION HISTORY:
;		Created 20-NOV-2003 by Mati Meron.
;		Modified 5-APR-2006 by Mati Meron.  Internal changes.
;		Modified 15-AUG-2009 by Mati Meron.  Increased maximal number of scans
;		from 8 to 16.
;-

	on_error, 1
	snams =  strcompress('s_' + sindgen(16),/remove)

	posib = ['min','max']
	whi = Strmatch_mm(orb,posib,3) > 0

	nsc = Scan_list_ver(s_0,s_1,s_2,s_3,s_4,s_5,s_6,s_7, $
	s_8, s_9, s_10, s_11, s_12, s_13, s_14, s_15, flag=lfl,lis=slis)
	if nsc eq 0 then message, 'Missing or inconsistent input!'

	minv = (maxv = fltarr(nsc))
	dum = (Wherinstruct('tau',_e))[0]
	if dum ge 0 then _e.(dum) = 0
	for i = 0l, nsc-1 do begin
		if lfl then scan = Scan_read(slis[i],_extra=_e) $
		else idum = execute('scan = ' + snams[i])
		imin = min(scan[0,*],max=imax)
		minv[i] = imin
		maxv[i] = imax
	endfor

	if whi eq 0 then res = Lexisort(minv,maxv) else res = Lexisort(maxv,minv)

	return, res
end