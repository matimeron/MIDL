Function Bimorph_proj, vec, cvec

;+
; NAME:
;		BIMORPH_PROJ
; VERSION:
;		8.15
; PURPOSE:
;		Corrects a "slopes vector" to attain zero-sum. 
; CATEGORY:
;		Bimorph mirror specific.
; CALLING SEQUENCE:
;		Result = BIMORPH_PROJ( VEC, CVEC)
; INPUTS:
;	VEC
;		Numeric vector, mandatory.
;	CVEC
;		Second numeric vector, mandatory.  Must be same length as VEC.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 		None.
; OUTPUTS:
; 		Returns the vector VEC corrected by a constant offset so as to be 
; 		orthogonal to CVEC.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Standard "skewed projection".
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron.
;-

	on_error, 1

	n = n_elements(vec)
	if n_elements(cvec) ne n then message, 'Dimensional mismatch!'
	uvec = replicate(1.,n)

	res = vec - (total(cvec*vec)/total(cvec*uvec))*uvec

	return, res
end