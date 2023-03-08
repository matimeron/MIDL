Function Extrema, x, min_only = mino, max_only = maxo, ceiling = ceil, $
	threshold = tre, signature = sig, number = num

;+
; NAME:
;		EXTREMA
; VERSION:
;		7.09
; PURPOSE:
;		Finding all local minima and maxima in a vector.
; CATEGORY:
;		Mathematical Function (array).
; CALLING SEQUENCE:
;		Result = EXTREMA( X [, keywords])
; INPUTS:
;	X
;		Numerical vector, at least three elements.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/MIN_ONLY
;		Switch.  If set, EXTREMA finds only local minima.
;	/MAX_ONLY
;		Switch.  If set, EXTREMA finds only local maxima.
;	THRESHOLD
;		A nonnegative value.  If provided, entries which differ by less then
;		THRESHOLD are considered equal.  Default value is 0.
;	/CEILING
;		Switch.  Determines how results for extended extrema (few consecutive
;		elements with the same value) are returned.  See explanation in OUTPUTS.
;	SIGNATURE
;		Optional output, see below.
;	NUMBER
;		Optional output, see below.
; OUTPUTS:
;		Returns the indices of the elements corresponding to local maxima
;		and/or minima.  If no extrema are found returns -1.  In case of
;		extended extrema returns midpoint index.  For example, if
;		X = [3,7,7,7,4,2,2,5] then EXTREMA(X) = [2,5].  Note that for the
;		second extremum the result was rounded downwards since (5 + 6)/2 = 5 in
;		integer division.  This can be changed using the keyword CEILING which
;		forces upward rounding, i.e. EXTREMA(X, /CEILING) = [2,6] for X above.
; OPTIONAL OUTPUT PARAMETERS:
;	SIGNATURE
;		The name of the variable to receive the signature of the extrema, i.e.
;		+1 for each maximum and -1 for each minimum.
;	NUMBER
;		The name of the variable to receive the number of extrema found.  Note
;		that if MIN_ONLY or MAX_ONLY is set, only the minima or maxima,
;		respectively, are counted.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls ARREQ, DEFAULT and ONE_OF from MIDL.
; MODIFICATION HISTORY:
;		Created 15-FEB-1995 by Mati Meron.
;		Modified 15-APR-1995 by Mati Meron.  Added keyword THRESHOLD.
;		Checked for operation under Windows, 25-JAN-2001, by Mati Meron.
;		Corrected and streamlined 10-MAY-2003, by Mati Meron.
;		Modified 15-MAY-2009 by Mati Meron.  Slight internal changes.
;-

	on_error, 1
	wx = reform(x)
	siz = size(wx)
	if siz[0] ne 1 then message, 'X must be a vector!' else $
	if siz[1] lt 3 then message, 'At least 3 elements are needed!'

	len = siz[1]
	res = replicate(0l,len)
	sig = res
	both = One_of(mino,maxo,/nozero) eq -1
	cef = keyword_set(ceil)
	tre = Default(tre,0.,/dtype) > 0

	xn = [0, wx[1:*] - wx[0:len-2]]
	if tre gt 0 then begin
		tem = where(abs(xn) lt tre, ntem)
		if ntem gt 0 then xn[tem] = 0
	endif
	xp = shift(xn,-1)
	xn = xn[1:len-2]
	xp = xp[1:len-2]

	if keyword_set(mino) or both then begin
		fir = where(xn lt 0 and xp ge 0, nfir) + 1
		sec = where(xn le 0 and xp gt 0, nsec) + 1
		if nfir gt 0 and Arreq(fir,sec) then begin
			res[fir] = fir
			sig[fir] = -1
		endif else begin
			if nfir le nsec then begin
				for i = 0l, nfir-1 do begin
					j = (where(fir[i] le sec))[0]
					if j ne -1 then begin
						i = (where(fir le sec[j],ni))[ni-1]
						ind = (fir[i] + sec[j] + cef)/2
						res[ind] = ind
						sig[ind] = -1
					endif
				endfor
			endif else begin
				for j = nsec-1, 0l, -1 do begin
					i = (where(fir le sec[j], ni))[(ni-1) > 0]
					if i ne -1 then begin
						j = (where(fir[i] le sec))[0]
						ind = (fir[i] + sec[j] + cef)/2
						res[ind] = ind
						sig[ind] = -1
					endif
				endfor
			endelse
		endelse
	endif

	if keyword_set(maxo) or both then begin
		fir = where(xn gt 0 and xp le 0, nfir) + 1
		sec = where(xn ge 0 and xp lt 0, nsec) + 1
		if nfir gt 0 and Arreq(fir,sec) then begin
			res[fir] = fir
			sig[fir] = 1
		endif else begin
			if nfir le nsec then begin
				for i = 0l, nfir-1 do begin
					j = (where(fir[i] le sec))[0]
					if j ne -1 then begin
						i = (where(fir le sec[j],ni))[ni-1]
						ind = (fir[i] + sec[j] + cef)/2
						res[ind] = ind
						sig[ind] = 1
					endif
				endfor
			endif else begin
				for j = nsec-1, 0l, -1 do begin
					i = (where(fir le sec[j], ni))[(ni-1) > 0]
					if i ne -1 then begin
						j = (where(fir[i] le sec))[0]
						ind = (fir[i] + sec[j] + cef)/2
						res[ind] = ind
						sig[ind] = 1
					endif
				endfor
			endelse
		endelse
	endif

	res = where(res gt 0, num)
	sig = sig(res > 0)

    return, res
end