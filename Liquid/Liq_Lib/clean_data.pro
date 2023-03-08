Function Clean_data, dat, velocity = vel, zero_mean = zerm, mask = mask

;+
; NAME:
;		CLEAN_DATA
; VERSION:
;		4.2
; PURPOSE:
;		Generates a "cleaned up" version of position or velocity data.
; CATEGORY:
;		Data processing.
; CALLING SEQUENCE:
;		Result = CLEAN_DATA( DAT [, keywords])
; INPUTS:
;	DAT
;		Input data, must be a 1 or 2D numeric array.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/VELOCITY
;		Switch.  If set, velocity data (calculated using forward difference on
;		the second dimension of the array) is returned.
;	/ZERO_MEAN
;		Switch.  If set, the data is corrected to yield zero mean for each
;		column.
;	MASK
;		Optional output.  See below.
; OUTPUTS:
;		Returns either the original position data or (if VELOCITY is set) the
;		resulting velocity data, with all the bad points set to zero byt the
;		glitch mask.
; OPTIONAL OUTPUT PARAMETERS:
;	MASK
;		The name of a variable to receive the calculated data mask.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward application of the calculated mask to data.  Calls DIF
;		and FPU_FIX from MIDL.  Also calls GLITCH_MASK.
; MODIFICATION HISTORY:
;		Created 15-SEP-2001 by Mati Meron.
;-

	on_error, 1

	siz = size(reform(dat))
	if siz[0] eq 0 or siz[0] gt 2 then message, 'Bad input!'

	if keyword_set(vel) then begin
		mask = Glitch_mask(dat,diff=2,/forw)
		if siz[0] eq 2 then begin
			res = make_array(siz=siz)
			for i= 0l, siz[1]-1 do res[i,*] = Dif(reform(dat[i,*]),/forw)
		endif else res = Dif(dat,/forw)
	endif else begin
		mask = Glitch_mask(dat)
		res = reform(dat)
	endelse

	res = res*mask
	if keyword_set(zerm) then begin
		mvals = total(res,2)/(total(mask,2) > 1)
		for i = 0l, siz[1]-1 do res[i,*] = res[i,*] - mvals[i]
		res = res*mask
	endif

	return, FPU_fix(res)
end