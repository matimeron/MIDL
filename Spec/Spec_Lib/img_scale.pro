Function Img_scale, dat, svar

;+
; NAME:
;		IMG_SCALE
; VERSION:
;		7.16
; PURPOSE:
;		Scales an image through an multiplication by a scalar or an array.
; CATEGORY:
;		Array Function.
; CALLING SEQUENCE:
;		Result = IMG_SCALE( DAT, SVAR)
; INPUTS:
;	DAT
;		Either a 2D [M,N] array, or the standard data representation 3D array 
;		with dimensions [4,M,N]
;	SVAR
;		Either a scalar or an [M,N] array (corresponding to the M, N dimensions
;		above
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns the scaled array, in the same format as DAT.  In the case of a
;		full [4,M,N] input, only the data (DAT[2,*,*]) and errors (DAT[3,*,*])
;		are scaled.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Multiplies the relevant parts of DAT, i.e. all of DAT in the 2D case and
;		DAT[2,*,*], DAT[3,*,*] in the 3D case, by SVAR.  Calls ARREQ, FPU_FIX
;		and ISNUM, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-APR-2010 by Mati Meron.
;-

	on_error, 1

	if Isnum(svar) then begin
		siz = size(dat)
		case siz[0] of
			2	:	ffl = 0
			3	:	ffl = 1
			else:	message, 'Not an image data!'
		endcase
		dim = siz[siz[0]-1:siz[0]]
		sdim = size(svar,/dim)
		if not (sdim[0] eq 0 or Arreq(dim,sdim)) $
		then message, 'Wrong scale size!'
	
		res = dat
		if ffl then begin
			res[2,*,*] = res[2,*,*]*svar
			res[3,*,*] = res[3,*,*]*svar
		endif else res = res*svar
	endif else res = dat

	return, FPU_fix(res)
end