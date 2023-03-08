Function Prune, arr, del, gap = gap, strict = str

;+
; NAME:
;	PRUNE
; VERSION:
;		8.214
; PURPOSE:
;		"Pruning" a data set.
; CATEGORY:
;		Array manipulation.
; CALLING SEQUENCE:
;		Result = PRUNE( ARR, DEL [, GAP = GAP] [, STRICT = STR)
; INPUTS:
;	ARR
;		Numeric array or scalar (treated as vector of length 1).  Always treated
;		as a vector.  Mandatory.
;	DEL
;		Positive numeric constant.  Must be of one of the integer types (BYTE,
;		INT, LONG etc.) unless the keyword GAP is set.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/GAP
;		Switch.  If set, the value of DEL is taken as the required X gap.  Else
;		the X values (the numeric values of ARR) are ignored and DEL is taken as
;		index gap.
;	/STRICT
;		Switch.  If set, even the last gap cannot be smaller than DEL.  Instead
;		the previous element is removed.
; OUTPUTS:
;		If the keyword GAP is *not* set, PRUNE returns a set of indices such
;		that the subset of the vector ARR given by SARR = ARR(result) is:
;		SARR = [ARR[0], ARR[DEL], ... ARR[N-1]], where N is the length of ARR.
;		Note that the last index, N-1 is included even if N-1 is not divisible ;
;		by DEL).  If STRICT is set, ARR[(floor((N-1)/DEL))] will be removed in
;		such case.
;
;		If GAP is set, PRUNE returns a set of indices such that the subset of
;		the vector ARR given by SARR = ARR(result) has the following properties:
;
;		1)	SARR includes the smallest and largest element of ARR.
;		2)	The spacing between any two elements of SARR (other than, possibly,
;			the spacing between the largest and the second largest) equals at
;			least DEL.  If STRICT is set and the spacing between largest and 
;			secondlargest is less than DEL, the second largest will be removed.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calling DIF and ISNUM from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JAN-2005 by Mati Meron.
;		Modified 15-MAR-2005 by Mati Meron.  Added keyword GAP.
;		Modified 5-NOV-2013 by Mati Meron.  Added keyword STRICT.
;-

	on_error, 1

	if del le 0 then message, 'DEL must be positive!'
	len = n_elements(arr)

	if len gt 1 then begin
		keep = lonarr(len)
		keep[len-1] = 1
		if keyword_set(gap) then begin
			sor = sort(arr)
			warr = arr[sor]
			if del gt min(Dif(warr,/lin)) then begin
				k = (done = 0)
				repeat begin
					keep[k] = 1
					dum = where(warr ge warr[k] + del, ndum)
					if ndum gt 0 then k = dum[0] else done = 1
				endrep until done
				if keyword_set(str) then begin
					dum = where(keep,ndum)
					if ndum gt 2 and (warr[dum[-1]] - warr[dum[-2]]) lt del $
					then keep[dum[-2]] = 0 
				endif
			endif else keep = replicate(1,len)
			keep = keep[sort(sor)]
		endif else begin
			if Isnum(del,/int) then begin
				ndum = ceil(1.*len/del)
				keep[del*lindgen(ndum)] = 1
				if keyword_set(str) then begin
					last = del*(ndum-1)
					if ndum gt 2 and (len-1) gt last and (len-1) lt last+del $
					then keep[last] = 0
				endif
			endif else message, 'DEL must be of some integer type!'
		endelse
		res = where(keep)
	endif else res = [0l]

	return, res
end