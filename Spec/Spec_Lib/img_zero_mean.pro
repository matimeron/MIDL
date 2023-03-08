Function Img_zero_mean, arr, detrend = det

;+
; NAME:
;		IMG_ZERO_MEAN
; VERSION:
;		7.09
; PURPOSE:
;		Image processing.
; CATEGORY:
;		Mathematical, array function.
; CALLING SEQUENCE:
;		Result = IMG_ZERO_MEAN (ARR [, /DETAILED])
; INPUTS:
;	ARR
;		An at most two dimensional numeric array.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/DETREND
;		Switch.  Valid only for 2D arrays.  When set, the array is transformed
;		so that not only its mean is zero (the default) but the mean of any row
;		or column is zero.
; OUTPUTS:
;		Returns the input with its mean subtracted so that the mean of the
;		output is zero (note that for scalar inputs the result is always 0).
;		optionally, if the input is a 2D array and /DETAILED is set, the means
;		of rows and coulmns are subtracted so as to make the mean of any row
;		or colum zero.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The dimansion of the input can be no higher than 2.
; PROCEDURE:
;		Straightforward. Calls CALCTYPE,CAST and ISNUM, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-MAY-2009 by Mati Meron.
;-

	on_error, 1

	if Isnum(arr) then begin
		siz = size(arr)
		typ = Calctype(arr,0.)
		warr = Cast(arr,5)

		case siz[0] of
			0	:	res = 0*warr
			1	:	res = warr - total(warr)/n_elements(warr)
			2	:	begin
						if keyword_set(det) then begin
							res = warr
							tem = total(res,1)/siz[1]
							for j = 0, siz[2]- 1 do res[*,j] = res[*,j] - tem[j]
							tem = total(res,2)/siz(2)
							for i = 0, siz[1]- 1 do res[i,*] = res[i,*] - tem[i]
						endif else res = warr - total(warr)/n_elements(warr)
					end
			3	:	message, 'Dimensions higher than 2 not supported!
		endcase
	endif else message, 'No data or no numeric data!

	return, Cast(res,typ,typ,/fix)
end