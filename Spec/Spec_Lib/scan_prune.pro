Function Scan_prune, scan, tol, pad = pad, partial = prt, netlen = len

;+
; NAME:
;		SCAN_PRUNE
; VERSION:
;		8.425
; PURPOSE:
;		"Prunes" scan data, merging points with same abcissa.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_PRUNE( SCAN [, TOL] [, keywords])
; INPUTS:
;	SCAN
;		A single valid scan, i.e. a [3,n] array.
; OPTIONAL INPUT PARAMETERS:
;	TOL
;		Numeric scalar, tolerance value.  Abcissa values with difference LE TOL
;		are considered same.  Default value is twice machine accuracy.
; KEYWORD PARAMETERS:
;	PAD
;		Directs the "padding" of the result in case only a single data point is
;		present to begin with, or left after the pruning.  By default, in such 
;		case the point is doubled, resulting in two points separated by 0.5*TOL.
;		Setting PAD explicitly to zero prevents the padding.
; 	/PARTIAL
; 		Switch.   If set, "partial scans", i.e. [2,n] arrays are accepted.
;	NETLEN
;		Optional output, see below.
; OUTPUTS:
;		Returns an output in a scan format (i.e. a [3,n] array), where all the 
;		multiple points (with abcissa values differing by less than TOL) are 
;		merged into single points, by proper weighted averaging.
; OPTIONAL OUTPUT PARAMETERS:
;	NETLEN
;		Returns the length of the pruned scan.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the input must be a proper scan.
; PROCEDURE:
;		Straightforward.  Calls SCAN_SORT.  Uses ARREQ, DEFAULT, DIF, FPU_FIX,
;		TOLER and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUN-2011 by Mati Meron.
;		Modified 15-AUG-2011 by Mati Meron.  Internal changes to accomodate 
;		partial scans.  Added keyword PARTIAL.
;		Modified 25-AUG-2015 by Mati meron.  Internal changes.
;-

	on_error, 1

	pfl = 0
	if keyword_set(prt) then begin
		siz = size(scan)
		if Arreq(siz[0:1],[2,2]) then begin
			res = make_array(3,siz[2],typ=Type(scan)>4)
			res[0:1,*] = scan
			res[2,*] = replicate(1.,siz[2])
			pfl = 1
		endif else res = scan
	endif else begin
		res = scan
		dum = where(res[2,*] gt 0, ndum)
		if ndum gt 0 then res[2,*]= res[2,*] > min(res[2,dum]) else res[2,*]= 1.
	endelse

	wtol = Default(tol,2*Toler(res))		
	siz = size(res)
	if Arreq(siz[0:1],[2,3]) then begin
		eps = Toler(res)
		len = siz[2]
		res = Scan_sort(res)
		repeat begin
			if len eq 1 then break
			check = Dif(res[0,*],/edge)
			l = (where(abs(check[1:*]) le wtol, nl))[0]
			if nl gt 0 then begin
				top = (res[1,l]/res[2,l])^2 + (res[1,l+1]/res[2,l+1])^2
				bot = (res[1,l]/(res[2,l])^2 + res[1,l+1]/(res[2,l+1])^2) > eps
				res[1,l] = top/bot
				res[2,l] = (sqrt(abs(top))/bot) > eps
				len = len - 1
				if l lt len-1 then begin
					res[*,l+1:len-1] = res[*,l+2:len]
					done = 0
				endif else done = 1
				res = res[*,0:len-1]
			endif else done = 1
		endrep until done
	endif else begin
		if n_elements(res) eq 3 then len = 1 else message, 'Not a scan!'
	endelse

	if len eq 1 and Default(pad,1,/dtyp) then begin
		tem = res
		len = 2l
		res = make_array(3,2,type=Type(res)>4)
		res[0,*] = tem[0] + [0,wtol/2]
		res[1,*] = tem[1]
		res[2,*] = tem[2]*sqrt(2)
	endif

	if pfl then res = res[0:1,*]

	return, FPU_fix(res)
end