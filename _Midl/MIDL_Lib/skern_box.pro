Function Skern_BOX, m

;+
; NAME:
;		SKERN_BOX
; VERSION:
;		8.33
; PURPOSE:
;		Generates a "box" kernel for the use of smoothing routines.
; CATEGORY:
;		Mathematical function.
; CALLING SEQUENCE:
;		Result = SKERN_BOX (M)
; INPUTS:
;	M
;		Nonnegative scalar or vector with number of components not exceeding 8.
;		Should be integer (if not then rounded downwards to integer on input).  
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns an array with as many dimensions as there are entries in M.  The
;		dimensions of the result are [2*M[0]+1,...2*M[k]+1]].  The values along
;		each dimension represent a box function, thus the full result is a
;		(possibly multidimensional) box function.
;		
;		The result is always of type DOUBLE.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the restrictions on M mentioned above.
; PROCEDURE:
; 		Straightforward.
; MODIFICATION HISTORY:
;		Created 10-OCT-2014 by Mati Meron.
;-

	on_error, 1

	ndm = n_elements(m)
	if ndm gt 0 then begin
		mm = 2*floor(m)
		ker = make_array(mm+1,typ=5)
		if mm[-1] eq 0 then ker = reform(ker,mm+1)
		ker[*,*,*,*,*,*] = 1d/product(mm+1d)
	endif else message, 'Missing input!'

	return, ker
end