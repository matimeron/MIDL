Function Img_exorcise, arr, thresh, substitute= sub, iter= itr, threshold= tre,$
	count = cnt

;+
; NAME:
;		IMG_EXORCISE
; VERSION:
;		8.44
; PURPOSE:
;		Replaces exceptional elements of a 2D array by the average of their
;		neighbors.
; CATEGORY:
;		Array Function.
; CALLING SEQUENCE:
;		Result = IMG_EXORCISE( ARR, THRESH [,keywords])
; INPUTS:
;	ARR
;		2D Array, numeric.
;	THRESH
;		Threshold value for element replacement.  Must be > 1.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SUBSTITUTE
;		Optional scalar value to be substituted for exceptional elements.
;	ITER
;		Specifies number of iterations.  Default is 1.
;	THRESHOLD
;		Alternative threshold value for element replacement.  Must be > 1.  If
;		given, overrides the value provided by THRESH.
;	COUNT
;		Optional output, see below.
; OUTPUTS:
;		Returns the corrected array, i.e. an array of the same format as ARR,
;		in which all the exceptional elements have been replaced by the average
;		of their neighbors, or by the SUBSTITUTE value if provided.
; OPTIONAL OUTPUT PARAMETERS:
;	COUNT
;		The name of the variable to receive the number of exceptional elements.
;		COUNT returns a vector of length ITER.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		IMG_EXORCISE is a simplified version of the routine DEGLITCH, from MIDL,
;		optimised for use with 2D arrays.  See DEGLITCH for details.  Uses
;		CALCTYPE, CAST, DEFAULT and ISNUM from MIDL.
; MODIFICATION HISTORY:
;		Created 30-OCT-2008 by Mati Meron.
;		Modified 25-FEB-2009 by Mati Meron.  Added keywords ITER and THRESHOLD.
;		Modified 20-May-2009 by Mati Meron.  Added keyword SUBSTITUTE.
;		Modified 20-DEC-2011 by Mati Meron.  Internal changes.
;		Modified 1-JUN-2013 by Mati Meron.  Internal changes.
;		Modified 5-AUG-2015 by Mati Meron.  Internal changes.
;		Modified 5-OCT-2015 by Mati Meron.  Internal changes.
;-

	on_error, 1

	typ = Calctype(arr,thresh,tre,def=3)
	siz = size(arr)
	if siz[0] ne 2 then message, 'Only 2D arrays accepted!'
	res = Cast(arr,4)
	dum = where(res gt 0,ndum)
	if ndum gt 0 then one = min(res[dum]) else one = 0
	cav = total(arr)/n_elements(arr) > one
	ptre = (sqrt(cav) + 2*one)^2
	tsiz = siz
	tsiz[1:2] = siz[1:2] + 2
	buf = make_array(size=tsiz)
	subfl = Isnum(sub)
	witr = Default(itr,1,/dtyp)
	cnt = lonarr(witr)
	for i = 0, witr-1 do begin
		buf[1:siz[1],1:siz[2]] = res
		tarr = (shift(buf,[0,1])+ shift(buf,[0,-1])+ $
		shift(buf,[1,0])+shift(buf,[-1,0]))/4.
		tarr = tarr[1:siz[1],1:siz[2]]
		dum = where(res gt (Default(tre,thresh)*tarr > ptre),dcnt)
		cnt[i] = dcnt
		if dcnt gt 0 then if subfl then res[dum] = sub else res[dum] = tarr[dum]
	endfor

	return, Cast(res,typ,typ,/fix)
end