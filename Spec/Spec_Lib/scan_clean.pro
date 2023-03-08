Function Scan_clean, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, $
	coeffs = cof, inter = int, _extra = _e

;+
; NAME:
;		SCAN_CLEAN
; VERSION:
;		4.9
; PURPOSE:
;		Performs background subtraction on a scan.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_CLEAN (S_0, ... [, keywords])
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
;
;		Important:  Unlike in the other SCAN routines, there is a distinction
;		here between the first scan (S_0) and the rest.  S_0 is the "data", all
;		the other scans (if any) are the "background".  This can be overriden,
;		though, in modes 3-4, by using "signed lists".  If some of the scan
;		numbers are positive and the rest negative, then all the positive ones
;		are taken as signal and the negative as background.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COEFFS
;		Numeric vector, the coefficients of the linear combination.  The number
;		of entries in COEFFS *must* agree with the number of scans provided.
;		If COEFFS is not provided, it is generated internally, assigning equal
;		weights (summing to 1) to all the signal scans and equal weights
;		summing to -1) to all the backgrounds.
;	/INTER
;		Switch, specifies that unmatched X values are to be interpolated.  See
;		SCAN_LC for explanation.
;	_EXTRA
;		A formal key, used to transfer additional keywords to SCAN_READ.  Only
;		active when a list is used.
; OUTPUTS:
;		Returns an output in a scan format (i.e. a [3,n] array), where the 3
;		columns (in order) are:  X values (must be common to all the scans),
;		the Y values of the "cleaned" scan and the squared error values of Y.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		1)  All the scans provided must share the same X-column
;		2)  Either actual scans or a list of scan numbers may be provided, but
;			not both
;		3)	If a scan number list is provided, SCAN_READ requires COLUMNS input.
;			See there for details.
; PROCEDURE:
;		Straightforward.  Calls SCAN_LIST_VER and SCAN_LC.  Generates the
;		COEFFS values, if needed.
; MODIFICATION HISTORY:
;		Created 25-OCT-2002 by Mati Meron.
;		Modified 5-JUL-2003 by Mati Meron.  Added keyword INTER.
;-

	on_error, 1
	snams =  strcompress('s_' + sindgen(8),/remove)

	ns = Scan_list_ver(s_0,s_1,s_2,s_3,s_4,s_5,s_6,s_7,flag=lfl,lis=slis)
	if ns eq 0 then message, 'Missing or inconsistent input!'

	if ns gt 1 and n_elements(cof) eq 0 then begin
		if lfl then begin
			pos = where(slis gt 0,npos,comp=neg,ncomp=nneg)
			if npos eq 0 or nneg eq 0 then $
			pos = where(indgen(ns) eq 0,npos,comp=neg,ncomp=nneg)
			ord = [pos,neg]
			slis = slis[ord]
		endif else begin
			ord = lindgen(ns)
			pos = where(ord eq 0,npos,comp=neg,ncomp=nneg)
		endelse
		cof = fltarr(ns)
		cof[pos] = 1./npos
		cof[neg] = -1./nneg
		cof = cof[ord]
	endif else if ns eq 1 then cof = 1.

	if lfl then res = Scan_lc(abs(slis),coef=cof,inter=int,_extra=_e) else $
	res = Scan_lc(s_0,s_1,s_2,s_3,s_4,s_5,s_6,s_7,coef=cof,inter=int,_extra=_e)

	return, res
end