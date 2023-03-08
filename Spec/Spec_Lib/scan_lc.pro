Function Scan_lc, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, $
	coeffs = cof, dcoeffs = dcof, inter = int, _extra = _e

;+
; NAME:
;		SCAN_LC
; VERSION:
;		5.4
; PURPOSE:
;		Generates a linear combination of scans.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_LC(S_0, ..., COEFFS = COF, [, keywords])
; INPUTS:
;	S_0, S_1, .. S_7
;		May be:
;			1)  Scans, i.e. [3,n] arrays.
;			2)	Scan numbers, i.e. integers
;			3)  A single array of scan numbers.
;			4)  A single character scalar or array to be processed by RANGE_PROC
;				from MIDL.  See there for allowed expressions.
;		In the first 2 cases the  maximal number of scans is 8.  In the third
;		and fourth case there is no limit.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COEFFS
;		Numeric vector, the coefficients of the linear combination.  The number
;		of entries in COEFFS *must* agree with the number of scans provided.
;	DCOEFFS
;		Numeric vector, optional.  The statistical errors of the coefficients
;		from COEFFS.  If given, must be same length as COEFFS.  DCOEFFS has no
;		impact on the Y values of the combination but it influences the errors.
;	/INTER
;		Switch.  If set, all scans following the first are interpolated to match
;		their X values with the first.
;	_EXTRA
;		A formal key, used to transfer additional keywords to SCAN_READ.  Only
;		active when a list is used.
; OUTPUTS:
;		Returns an output in a scan format (i.e. a [3,n] array), where the 3
;		columns (in order) are:  X values (must be common to all the scans,
;		unless /INTER is set), the Y values of the linear combination and the
;		values of the combined Y (calculated from the individual scans' errors
;		as sqrt of a linear combination of squares, using the squares of the
;		coefficients).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		1)  If /INTER is not set, all the scans provided must share the same
;			X-column.
;		2)  Either actual scans or a list of scan numbers may be provided, but
;			not both.
;		3)	If a scan number list is provided, SCAN_READ requires COLUMNS input.
;			See there for details.
; PROCEDURE:
;		Straightforward.  Calls SCAN_INTER, SCAN_LIST_VER and SCAN_READ (only
;		when in the list mode).  Uses ARREQ, DIAGOARR, FPU_FIX and WHERINSTRUCT
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 20-SEP-2002 by Mati Meron.
;		Modified 5-JUL-2003 by Mati Meron.  Added keyword INTER.
;		Modified 20-NOV-2003 by Mati Meron.  Added keyword DCOEFFS.
;		Modified 15-APR-2004 by Mati Meron.  Changed internal data formats.
;		Modified 5-APR-2006 by Mati Meron.  Internal changes.
;-

	on_error, 1
	snams =  strcompress('s_' + sindgen(8),/remove)

	nsc = Scan_list_ver(s_0,s_1,s_2,s_3,s_4,s_5,s_6,s_7,flag=lfl,lis=slis)
	if nsc eq 0 then message, 'Missing or inconsistent input!'
	dcfl = 0
	intfl = keyword_set(int)

	nco = n_elements(cof)
	if n_elements(dcof) eq nco then dcfl = 1 $
	else if n_elements(dcof) ne 0 then message, 'Coeffs and errors mismatch!'

	if nsc eq nco then begin
		if lfl then begin
			res = Scan_read(slis[0],_extra=_e)
			dum = (Wherinstruct('new',_e))[0]
			if dum ge 0 then _e.(dum) = 0
		endif else res = s_0
		res = Diagoarr([1,0,0])#res
		comp = res[0,*]
		for i = 0, nsc - 1 do begin
			if lfl then scan = Scan_read(slis[i],_extra=_e) $
			else idum = execute('scan = ' + snams[i])
			if intfl then scan = Scan_inter(scan,comp)
			if Arreq(comp,scan[0,*]) then begin
				if dcfl then cerr = (scan[1,*]*dcof[i])^2
				scan = Diagoarr([0,cof[i],cof[i]])#scan
				scan[2,*] = scan[2,*]^2
				if dcfl then scan[2,*] = scan[2,*] + cerr
				res = res + scan
			endif else message, 'Incompatible spectra!'
		endfor
		res[2,*] = sqrt(res[2,*])
	endif else message, 'Bad or missing input!'

	return, FPU_fix(res)
end