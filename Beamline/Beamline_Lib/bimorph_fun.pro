Function Bimorph_fun, x, matrix = mat, cmatrix = cmat

;+
; NAME:
;		BIMORPH_FUN
; VERSION:
;		8.15
; PURPOSE:
;		Evaluation function to be used in bimorph matrix fitting.
; CATEGORY:
;		Bimorph mirror specific.
; CALLING SEQUENCE:
;		Result = BIMORPH_FUN( X, MATRIX = MAT, CMATRIX = CMAT, _EXTRA = _E)
; INPUTS:
;	X
;		Numeric vector or scalar.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	MATRIX
; 		Numeric matrix.
; 	CMATRIX
; 		Second (comparison) numeric matrix.  Must be have same dimensions as
; 		MATRIX.
; OUTPUTS:
; 		Returns a vector of same length as X, where the i-th component equals
; 		TOTAL((X[i]*MATRIX - CMATRIX)^2).  If the length of X is 1, the result
; 		is a scalar.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Straightforward.  Calls CALCTYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron.
;-

	on_error, 1

	n = n_elements(x)
	typ = Calctype(x,mat,cmat,def=4)
	res = make_array(n,typ=typ)

	for i = 0, n-1 do res[i] = total((x[i]*cmat - mat)^2)
	if n eq 1 then res = res[0]
	
	return, res
end