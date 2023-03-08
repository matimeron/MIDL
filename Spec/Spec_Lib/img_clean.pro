Function Img_clean, id_0, id_1, id_2, id_3, id_4, id_5, id_6, id_7, $
	coeffs = cof, extrapolate = ext, positive = pos, reform = ref, tol = tol

;+
; NAME:
;		IMG_CLEAN
; VERSION:
;		7.1
; PURPOSE:
;		Performs background subtraction on an image.
; CATEGORY:
;		2D data processing.
; CALLING SEQUENCE:
;		Result = IMG_CLEAN(ID_0, ...  [, keywords])
; INPUTS:
;	ID_0, ID_1, .. ID_7
;		2D image data sets.  Each input must be either a [3,M,N] array or a
;		[4,M,N] array, where the first "slice", [0,*,*], represents the X
;		coordinates of the data, the second slice represents the Y coordinates,
;		the third slice is the data itself and the fourth slice (if present)
;		represents the data errors.  Mixing of the two types of inputs is not
;		allowed.  The maximal number of data sets accepted is 8 (can be
;		increased in the future if needed.
;		
;		Note:	Unless otherwise specified by the keyword COEFFS, the first 
;				defined input is considered data and additional inputs (if
;				present) are taken as background.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COEFFS
;		Numeric vector, the coefficients of the linear combination.  If given,
;		the number of entries in COEFFS *must* agree with the number of images
;		provided.  If not given, the coefficients are generated internally, with
;		the first coefficient being 1 and (if N, the number of inputs, is 
;		greater than 1), all additional coefficients being -1/(N-1).
;	/EXTRAPOLATE
;		Switch.  See IMG_LC for details.
;	/POSITIVE
;		Switch.  If set, any negative data points in the result are set to 0.
;	/REFORM
;		Switch.  If set, the result is reformed to conform with the coordinates
;		of ID_0.
;	TOL
;		Tolerance value for REFORM purposes.  See IMG_REFORM for details.
; OUTPUTS:
;		Returns the background subtracted data, in same format as the inputs 
;		(see above).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the restrictions of IMG_LC.
;		area.
; PROCEDURE:
;		Generates the linear combination coefficients, if needed, and uses 
;		IMG_LC for the actual subtraction.  Calls IMG_REFORM if needed.  Also
;		calls NPARDEF, from MIDL. 
; MODIFICATION HISTORY:
;		Created  1-OCT-2009 by Mati Meron.
;		Modified 1-OCT-2014 by Mati Meron.  Added keywords REFORM and TOL.
;-

	on_error, 1

	n = Npardef(id_0, id_1, id_2, id_3, id_4, id_5, id_6, id_7, whi=j)
	if n gt 0 then begin
		nco = n_elements(cof)
		if nco eq 0 then begin
			cof = fltarr(n)
			cof[0] = 1
			if n gt 1 then cof[1:*] = -1./(n-1)
		endif
	endif else message, 'Missing Data!'

	res = Img_lc(id_0,id_1,id_2,id_3,id_4,id_5,id_6,id_7,coeffs =cof, ext=ext)
	if keyword_set(ref) then res = $
		Img_reform(res,tol,xvals=reform(id_0[0,*,0]),yvals=reform(id_0[1,0,*]))
	if keyword_set(pos) then res[2,*,*] = res[2,*,*] > 0
		
	return, res
end