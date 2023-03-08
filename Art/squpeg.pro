Function Squpeg, shape, reverse = rev

;+
; NAME:
;		SQUPEG
; PURPOSE:
;		Acts on 2-D shapes to induce a square --> circle transformation.
; CATEGORY:
;		Graphic Art.
; CALLING SEQUENCE:
;		Result = SQUPEG ( SHAPE [, /REVERSE])
; INPUTS:
;	SHAPE
;		An (2,*) numeric array.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/REVERSE
;		Switch.  Reverses the transformation direction.
; OUTPUTS:
;		Returns 0 for failure (missing or invalid shape), otherwise returns the
;		transformed shape as a floating array.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Uses SHAPE_VER from MIDL.
; MODIFICATION HISTORY:
;		Created 15-DEC-1991 by Mati Meron.
;-

	on_error, 1

	if Shape_ver(shape) ne 2 then begin
		message, 'Missing or invalid shape', /continue
		return, 0
	endif else begin
		hpi = !pi/2
		r = sqrt(total(shape^2,1))
		t = r
		tem = where(r ne 0, tnum)
		if tnum gt 0 then t(tem) = atan(shape(1,tem),shape(0,tem))
		it = round(t/hpi)
		t = t - hpi*it
		if keyword_set(rev) then begin
			t = atan(2/hpi*t)
			r = r/cos(t)
		endif else begin
			r = r*cos(t)
			t = hpi/2*tan(t)
		endelse
		t = t + hpi*it
	endelse

	return, transpose([[r*cos(t)],[r*sin(t)]])
end
