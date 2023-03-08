Function Skern_BNC, m

;+
; NAME:
;		SKERN_BNC
; VERSION:
;		8.33
; PURPOSE:
;		Generates a "binomial coefficients" kernel for the use of smoothing
;		routines.
; CATEGORY:
;		Mathematical function.
; CALLING SEQUENCE:
;		Result = SKERN_BNC (M)
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
;		each dimension are proportional to an appropriate set of binomial
;		coefficients, thus the full result is a (possibly multidimensional)
;		binomial coefficient distribution which is an discrete equivalent of a
;		Gaussian distribution.
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
; 		Straightforward.  Calls BINCOEF from MIDL.
; MODIFICATION HISTORY:
;		Created 10-OCT-2014 by Mati Meron.
;-

	on_error, 1

	ndm = n_elements(m)
	if ndm gt 0 then begin
		mm = 2*floor(m)
		ker = make_array(mm+1,typ=5,val=1)
		if mm[-1] eq 0 then ker = reform(ker,mm+1)
		for i = 0, ndm - 1 do begin
			bc = Bincoef(1d*mm[i],lindgen(mm[i]+1))/2d^mm[i]
			case i of
				0:	for j = 0, mm[i] do ker[j,*,*,*,*,*]= bc[j]*ker[j,*,*,*,*,*]
				1:	for j = 0, mm[i] do ker[*,j,*,*,*,*]= bc[j]*ker[*,j,*,*,*,*]
				2:	for j = 0, mm[i] do ker[*,*,j,*,*,*]= bc[j]*ker[*,*,j,*,*,*]
				3:	for j = 0, mm[i] do ker[*,*,*,j,*,*]= bc[j]*ker[*,*,*,j,*,*]
				4:	for j = 0, mm[i] do ker[*,*,*,*,j,*]= bc[j]*ker[*,*,*,*,j,*]
				5:	for j = 0, mm[i] do ker[*,*,*,*,*,j]= bc[j]*ker[*,*,*,*,*,j]
			endcase
		endfor
	endif else message, 'Missing input!'

	return, ker
end