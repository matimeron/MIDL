Function Skern_SVG, m

;+
; NAME:
;		SKERN_BOX
; VERSION:
;		8.33
; PURPOSE:
;		Generates a "Savitzky-Golay" kernel for the use of smoothing routines.
; CATEGORY:
;		Mathematical function.
; CALLING SEQUENCE:
;		Result = SKERN_SVG (M)
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
;		each dimension are proportional to an appropriate set of Savitzky-Golay
;		coefficients, thus the full result is a (possibly multidimensional)
;		Savitzky-Golay coefficient distribution.
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
; 		Straightforward.  Calls CAST, DIAGOARR, MAKE_GRID and SOLVE_LINSYS,
; 		from MIDL.
; MODIFICATION HISTORY:
;		Created 10-OCT-2014 by Mati Meron.
;-

	on_error, 1

	ndm = n_elements(m)
	if ndm gt 0 then begin
		dm = Cast(floor(m),5,5)
		ks = (Make_grid(transpose([[-dm],[dm]]),2*dm+1,fun=ker))^2

		s2 = [1.,dm*(dm+1)/3]
		s4 = s2*[1d,(3*dm^2 + 3*dm - 1)/5]
		sar = s2#transpose(s2) + Diagoarr(s4 - s2^2)
		rhs = [1d*ndm/n_elements(ks),fltarr(ndm)]
		lam = Solve_linsys(sar,rhs,/svd)

		ker = ker + lam[0]
		if ndm eq 1 then ker = ker + lam[1]*ks else $
			for i = 0, ndm-1 do ker = ker + lam[i+1]*ks[i,*,*,*,*,*]
		ker = reform(ker,2*dm+1)
	endif else message, 'Missing input!'

	return, ker
end