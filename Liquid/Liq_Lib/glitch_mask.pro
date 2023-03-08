Function Glitch_mask, arr, threshold, above = abv, below = blw, $
	difference = diff, backward = back, centered = cent, forward = forw

;+
; NAME:
;		GLITCH_MASK
; VERSION:
;		4.2
; PURPOSE:
;		Generating a data mask used to eliminate improper data.
; CATEGORY:
;		Array processing.
; CALLING SEQUENCE:
;		Result = GLITCH_MASK( ARR [, THRESHOLD] [, keywords])
; INPUTS:
;	ARR
;		Numeric array
; OPTIONAL INPUT PARAMETERS:
;	THRESHOLD.
;		Scalar value, the threshold above (below) which the data is considered
;		to be good.  Default value is 0.
; KEYWORD PARAMETERS:
;	/ABOVE
;		Switch.  If set, data values >= THRESHOLD are taken as good.
;	/BELOW
;		Switch.  If set, data values <  THRESHOLD are taken as good.
;
;		Note:  	Only one of ABOVE, BELOW may be set.  Default is ABOVE.
;
;	DIFFERENCE
;		Integer scalar.  If not zero, the data is evaluated in view of being
;		used for difference calculation, with the diffrence being performed on
;		the DIFF direction.  For example, if backward difference is being
;		used, Both ARR[i] and ARR[i-1] must be good points in order for
;		Dif(ARR)[i] to be defined.  If the array is 2-D and DIFF = 2 then both
;		ARR[i,j] and ARR[i,j-1] need to be good points, for any specific i.
;	/BACKWARD
;		Switch.  Signifies backward difference.
;	/FORWARD
;		Switch.  Signifies forward difference.
;	/CENTERED
;		Switch.  Signifies centered difference.
;
;		Note:  	Only one of BACKWARD, FORWARD, CENTERED may be set.  Default is
;				BACKWARD.
; OUTPUTS:
;		Returns a mask of type BYTE and same format as the input array, with
;		values of 1 corresponding to valid points and 0 to invalid ones.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward search.  Calls DEFAULT and ONE_OF from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2001 by Mati Meron.
;-

	on_error, 1
	ndmx = 7
	siz = size(reform(arr))
	ndm = siz[0]
	tre = Default(tre,0)
	ab = One_of(abv,blw) > 0
	res = 0b

	if ndm gt 0 then begin
		res = make_array(size=siz,/byte)
		if ab then dum = where(arr lt tre,nd) else dum = where(arr ge tre,nd)
		if nd gt 0 then res[dum] = 1b
		if n_elements(diff) ne 0 then begin
			dift = One_of(back,cent,forw) > 0
			s = lonarr(ndmx)
			refind = 1 + s
			refind[ndmx-ndm:ndmx-1] = siz[1:ndm]
			res = reform(res,refind)
			ores = res

			tem = replicate('*',ndmx)
			tem[ndmx-ndm] = '0'
			scom = strarr(ndm)
			for i = 0l, ndm-1 do begin
				scom[i] = strjoin(tem,',')
				tem = shift(tem,1)
			endfor
			scom = 'tres[' + scom + '] = 0'
			ecom = strjoin('s[' + string(indgen(ndmx)) + ']',',')
			ecom = strcompress('tres = shift(tres,' + ecom + ')',/remove)

			for i = 0l, ndm-1 do begin
				if (where(diff eq (i+1)))[0] ge 0 then begin
					j = i + ndmx - ndm
					if dift le 1 then begin
						s[j] = 1
						tres = ores
						idum = execute(ecom)
						idum = execute(scom[i])
						s[j] = 0
						res = res and tres
					endif
					if dift ge 1 then begin
						s[j] = -1
						tres = ores
						idum = execute(scom[i])
						idum = execute(ecom)
						s[j] = 0
						res = res and tres
					endif
				endif
			endfor
			res = reform(res)
		endif
	endif else message, 'Not an Array!', /continue

	return, res
end